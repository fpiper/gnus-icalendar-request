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

(defun gnus-icalendar--ical-from-event (event)
  (with-slots (summary description location organizer recur uid start-time end-time req-participants opt-participants) event
    (let ((dtstamp (format-time-string "DTSTAMP:%Y%m%dT%H%M%SZ" nil t)) ;; current UTC time
          (summary (format "SUMMARY:%s" summary))
          (description (when (and (stringp description)
                                  (not (string-empty-p description)))
                         (format "DESCRIPTION:%s"
                                 (string-replace "\n" "\\n" description))))
          (dtstart (format-time-string "DTSTART:%Y%m%dT%H%M%SZ" start-time t)) ;; in UTC -> suffix "Z"
          (dtend (format-time-string "DTEND:%Y%m%dT%H%M%SZ" end-time t))
          (attendee
           (mapconcat
            (lambda (p)
              (format
               "ATTENDEE%s"
               (gnus-icalendar--format-ical-property-parameters p)))
            (append
             (mapcar (lambda (entry)
                       (gnus-icalendar--parse-message-email-to-alist
                        entry
                        '((PARTSTAT . "NEEDS-ACTION")
                          (ROLE . "REQ-PARTICIPANT")
                          (RSVP . "TRUE"))))
                     (mail-header-parse-addresses (mapconcat #'identity req-participants ", ")))
             (mapcar (lambda (entry)
                       (gnus-icalendar--parse-message-email-to-alist
                        entry
                        '((PARTSTAT . "NEEDS-ACTION")
                          (ROLE . "OPT-PARTICIPANT")
                          (RSVP . "TRUE"))))
                     (mail-header-parse-addresses (mapconcat #'identity opt-participants ", "))))
            "\n"))
          (location (when (and (stringp location) (not (string-empty-p location)))
                      (format "LOCATION:%s" location)))
          (organizer (format "ORGANIZER%s"
                             (gnus-icalendar--format-ical-property-parameters
                              (gnus-icalendar--parse-message-email-to-alist
                               (car (mail-header-parse-addresses
                                     organizer))
                               ))))
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

(defun gnus-icalendar--format-ical-property-parameters (item)
  "Format a cons ITEM according to RFC5545 rules.

Car of ITEM is the property value and the cdr is an alist of additional
property parameters."
  (format "%s:%s"
          (seq-reduce
           (lambda (a b) (format "%s;%s=%s" a (car b) (cdr b)))
           (cdr item) "")
          (car item)))

(defun gnus-icalendar--parse-message-email-to-alist (entry &optional alist)
  "Parse a (mail . name) cons ENTRY as returned by
 `mail-header-parse-addresses'.

Optional argument ALIST specifies will be appended to the entry."
  (cons (format "mailto:%s" (car entry))
        (append
         alist
         (when (cdr entry)
           `((CN . ,(if (string-match "[:;,]" (cdr entry))
                        ;; quote property value if necessary
                        (format "\"%s\"" (cdr entry))
                      (cdr entry))))))))

;;;###autoload
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
           (organizer (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "From")))
           (rsvp nil) ;; TODO
           (participation-type 'non-participant)
           (req-participants
            (save-restriction
              (message-narrow-to-headers)
              (message-fetch-field "To")))
           (opt-participants
                     (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "Cc")))
           (uid (icalendar--create-uid (format "%s%s%s%s"
                                               summary
                                               description
                                               location
                                               organizer)
                                       (format "DTSTART:%s" start-time)))
           (event (gnus-icalendar-event-request :uid uid
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
