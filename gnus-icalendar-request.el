;;; gnus-icalendar-request.el --- Create icalendar events  -*- lexical-binding: t; -*-

;; Copyright (C) 2020,2024  Ferdinand Pieper

;; Author: Ferdinand Pieper <mail@pie.tf>
;; Keywords: mail, icalendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions to create icalendar events to be sent out to other
;; people.

;;; Code:

(require 'gnus-icalendar)

(defun gnus-icalendar--format-attendee (attendee role)
  (when (member role '("req" "opt"))
    (format "ATTENDEE;PARTSTAT=NEEDS-ACTION;ROLE=%s-PARTICIPANT;RSVP=TRUE:mailto:%s" (upcase role) attendee)))

(defun gnus-icalendar--create-attendee-list (req &optional opt role)
  "Format a list of event attendees.

REQ is a list of required attendees emails, OPT of optional
attendees and ROLE can be used to override the REQ attendees
role."
  (concat
   (when req
     (mapconcat
      (lambda (req) (gnus-icalendar--format-attendee req (or role "req")))
      req "\n"))
   (when opt
     (concat
      "\n"
      (gnus-icalendar--create-attendee-list opt nil "opt")))))

(defun gnus-icalendar--ical-from-event (event)
  (with-slots (summary description location organizer recur uid start-time end-time req-participants opt-participants) event
    (let ((dtstamp (format-time-string "DTSTAMP:%Y%m%dT%H%M%SZ" nil t)) ;; current UTC time
          (summary (format "SUMMARY:%s" summary))
          (description (when (and (stringp description) (not (string-empty-p description)))
                         (format "DESCRIPTION:%s"
                                 (with-temp-buffer
                                   (insert description)
                                   (beginning-of-buffer)
                                   (while (re-search-forward "\n" nil t)
                                     (replace-match "\\n" t t))
                                   (buffer-string))))) ;; TODO: How to do this properly?
          (dtstart (format-time-string "DTSTART:%Y%m%dT%H%M%SZ" start-time t)) ;; start-time in UTC
          (dtend (format-time-string "DTEND:%Y%m%dT%H%M%SZ" end-time t)) ;; end-time in UTC
          (attendee (gnus-icalendar--create-attendee-list req-participants opt-participants))
          (location (when (and (stringp location) (not (string-empty-p location)))
                      (format "LOCATION:%s" location)))
          (organizer (format "ORGANIZER:mailto:%s" organizer))
          (uid (format "UID:%s" uid))
          (sequence "SEQUENCE:0") ;; TODO: Consider follow-up event modifications.
          ;; TODO: handle recur
          )
      (with-temp-buffer
        (insert
         (mapconcat #'identity
                    (list "BEGIN:VEVENT"
                          dtstamp
                          dtstart
                          dtend
                          summary
                          description
                          attendee
                          location
                          organizer
                          uid
                          sequence
                          "END:VEVENT") "\n"))
        (flush-lines "^$" (point-min) (point-max))
        (buffer-string)))))

;; Vcalendar creation

(defun gnus-icalendar--build-vcalendar-from-vevent (event)
  "Create VCALENDAR part with VEVENT part EVENT."
  (mapconcat #'identity `("BEGIN:VCALENDAR"
                          "PRODID:-//Emacs//NONSGML gnus-icalendar.el//EN"
                          "VERSION:2.0"
                          "METHOD:REQUEST"
                          ,event
                          "END:VCALENDAR") "\n"))

(defun gnus-icalendar-message-insert-request (event)
  "Insert text/calendar part into message with request for VEVENT
  specified in EVENT."
  (when (provided-mode-derived-p major-mode 'message-mode)
    (mml-insert-part "text/calendar; method=\"REQUEST\"; charset=UTF-8")
    (insert (gnus-icalendar--build-vcalendar-from-vevent
             (gnus-icalendar--ical-from-event event)))))

(defun gnus-icalendar-from-message-and-insert (&optional date location)
  "Create a event request based on the current message.

Direct recipients of the message (in To header) are interpreted
as required participants. Recipients in Cc are optional
participants. The From header is always converted to the event
organizer. Message subject is interpreted as summary and message
body (if existant) as description. Time and date of the event can
be provided as org formatted date range (only with time for now)
or will be asked for if nil. Same for location."
  (interactive)
  (if (not message-draft-article) ;; internally set by message-mode
      (message "Not in a message draft")
    (unless (or date (featurep 'org))
      (error "Timestamp creation requires org. Please load org or provide a org-styled date range"))
    (message-check-recipients) ;; check for bogus recipients
    (let* ((date (or date
                     (with-temp-buffer
                       (org-time-stamp nil)
                       (buffer-string))))
           (start-time (org-timestamp-to-time  ;; in UTC
                        (org-timestamp-from-string date) nil))
           (end-time (org-timestamp-to-time ;; set end-time if input was a time-range
                      (org-timestamp-from-string date) t))
           (end-time (if (equal end-time start-time) ;; ask for end-time if previous input was not a range
                         (org-read-date nil t nil "End time:" start-time)
                       end-time))
           ;; TODO: better differentiate date-time ranges and date (whole-day) ranges
           (recur nil) ;; TODO
           (location (or location (read-string "Event location: ")))
           (description (when (message-goto-body)
                          (buffer-substring (point) (point-max))))
           (summary (save-restriction
                      (message-narrow-to-headers)
                      (message-fetch-field "Subject")))
           (organizer (caar (mail-header-parse-addresses
                            (save-restriction
                              (message-narrow-to-headers)
                              (message-fetch-field "From"))))) ;; TODO insert common name for "name <mail@address.net>" addresses
           (rsvp nil) ;; TODO
           (participation-type 'non-participant) ;; TODO
           (req-participants (mapcar #'car
                                     (mail-header-parse-addresses
                                      (save-restriction
                                        (message-narrow-to-headers)
                                        (message-fetch-field "To")))))
           (opt-participants (mapcar #'car
                                     (mail-header-parse-addresses
                                      (save-restriction
                                        (message-narrow-to-headers)
                                        (message-fetch-field "Cc")))))
           (uid (icalendar--create-uid (format "%s%s%s%s"
                                               summary
                                               description
                                               location
                                               organizer)
                                       (format "DTSTART:%s" start-time)))
           (event (gnus-icalendar-request :uid uid
                                                :recur recur
                                                :location location
                                                :description description
                                                :summary summary
                                                :method "REQUEST"
                                                :organizer organizer
                                                :start-time start-time
                                                :end-time end-time
                                                :rsvp rsvp
                                                :participation-type participation-type
                                                :req-participants req-participants
                                                :opt-participants opt-participants)))
      (message-goto-body)
      (delete-region (point) (point-max))
      (when (not (string-empty-p description))
        (mml-insert-multipart "mixed")
        (mml-insert-part "text/plain")
        (insert description "\n")
        (re-search-forward "<#/part>\n"))
      (gnus-icalendar-message-insert-request event))))

(provide 'gnus-icalendar-request)
;;; gnus-icalendar-request.el ends here
