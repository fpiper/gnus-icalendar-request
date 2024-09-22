;;; gnus-icalendar-request-tests.el --- tests                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ferdinand Pieper

;; Author: Ferdinand Pieper <mail@pie.tf>
;; Keywords:

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'gnus-icalendar-request)

(ert-deftest gnus-icalendar--build-vcalendar-from-vevent ()
  ""
  (let ((tz (getenv "TZ"))
        (event "\
BEGIN:VEVENT
DTSTAMP:20240915T120000Z
DTSTART:20240917T080000Z
DTEND:20240917T100000Z
SUMMARY:Party
DESCRIPTION:Lots of reasons to celebrate!
ATTENDEE;PARTSTAT=NEEDS-ACTION;ROLE=REQ-PARTICIPANT;RSVP=TRUE:mailto:required@company.invalid
ATTENDEE;PARTSTAT=NEEDS-ACTION;ROLE=OPT-PARTICIPANT;RSVP=TRUE:mailto:optional@company.invalid
LOCATION:Party room
ORGANIZER:mailto:organizer@company.invalid
UID:ac44f43e-f5cd-4b0a-878e-add01aeb12dd
SEQUENCE:0
END:VEVENT"))
    (setenv "TZ" "CET-1CEST,M3.5.0/2,M10.5.0/3")
    (let ((vcalendar (gnus-icalendar--build-vcalendar-from-vevent event)))
      ;; Ensure there is exactly one of VERSION and PRODID
      (should (string-match "^VERSION:2.0\\(\n\\|.\\)*END:VCALENDAR" vcalendar))
      (should-not (string-match "^VERSION:" (match-string 1 vcalendar)))
      (should (string-match "^PRODID:\\(\n\\|.\\)*END:VCALENDAR" vcalendar))
      (should-not (string-match "^PRODID:" (match-string 1 vcalendar)))
      ;; METHOD should be REQUEST
      (should (string-match "^METHOD:REQUEST$" vcalendar))
      ;; Ensure the vevent remains intact
      (should (string-match "^\\(BEGIN:VEVENT\\(\n\\|.\\)*\nEND:VEVENT\\)" vcalendar))
      (should (string-match (match-string 1 vcalendar) event)))
    (setenv "TZ" tz)))

(ert-deftest gnus-icalendar--ical-from-event ()
  ""
  (let* ((event
          (gnus-icalendar-event-request
           :uid "ac44f43e-f5cd-4b0a-878e-add01aeb12dd"
           :recur nil
           :location "Party room"
           :description "Lots of reasons to celebrate!"
           :summary "Party"
           :method "REQUEST"
           :organizer "organizer@company.invalid"
           :start-time (encode-time '(0 0 8 17 9 2024 nil -1 t))
           :end-time (encode-time '(0 0 10 17 9 2024 nil -1 t))
           :rsvp nil
           :participation-type 'non-participant
           :req-participants '("Required CN <required@company.invalid>" "required2@company.invalid")
           :opt-participants '("optional@company.invalid")))
         (ical (gnus-icalendar--ical-from-event event)))
    (should (string-match "^BEGIN:VEVENT$" ical))
    (should (string-match "^END:VEVENT$" ical))
    (should (string-match "^DTSTAMP:" ical))
    (should (string-match "^DTSTART:20240917T080000Z$" ical))
    (should (string-match "^DTEND:20240917T100000Z$" ical))
    (should (string-match "^SUMMARY:Party$" ical))
    (should (string-match "^DESCRIPTION:Lots of reasons to celebrate!$" ical))
    (should (string-match "^ATTENDEE;PARTSTAT=NEEDS-ACTION;ROLE=REQ-PARTICIPANT;RSVP=TRUE;CN=Required CN:mailto:required@company.invalid$" ical))
    (should (string-match "^ATTENDEE;PARTSTAT=NEEDS-ACTION;ROLE=OPT-PARTICIPANT;RSVP=TRUE:mailto:optional@company.invalid$" ical))
    (should (string-match "^LOCATION:Party room$" ical))
    (should (string-match "^ORGANIZER:mailto:organizer@company.invalid$" ical))
    (should (string-match "^UID:ac44f43e-f5cd-4b0a-878e-add01aeb12dd$" ical))
    (should (string-match "^SEQUENCE:0$" ical))))

(ert-deftest gnus-icalendar--create-attendee-list ()
  ""
  (let* ((req-part '("Colleague1 <required@company.invalid>"
                     "Cool Colleague <required2@company.invalid>"))
         (opt-part '("My boss <required@company.invalid>"))
         (attendees (gnus-icalendar--create-attendee-list req-part opt-part))
         (expected-attendee-entry "^ATTENDEE.*;ROLE=%s-PARTICIPANT.*;RSVP=TRUE:mailto:%s.*$"))
    (should (string-match (format expected-attendee-entry
                                  "REQ" (nth 0 req-part))
                          attendees))
    (should (string-match (format expected-attendee-entry
                                  "REQ" (nth 1 req-part))
                          attendees))
    (should (string-match (format expected-attendee-entry
                                  "OPT" (nth 0 opt-part))
                          attendees))))
(ert-deftest gnus-icalendar--create-empty-required-attendee-list ()
  ""
  (let* ((req-part nil)
         (opt-part '("My boss <required@company.invalid>"))
         (attendees (gnus-icalendar--create-attendee-list req-part opt-part))
         (expected-attendee-entry "^ATTENDEE.*;ROLE=%s-PARTICIPANT.*;RSVP=TRUE:mailto:%s.*$"))
    (should (string-match (format expected-attendee-entry
                                  "OPT" (nth 0 opt-part))
                          attendees))))

