;; Copyright 2014, 2016 Free Software Foundation, Inc.

;; This file is part of Guile-Ncurses.

;; Guile-Ncurses is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; Guile-Ncurses is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Guile-Ncurses.  If not, see
;; <http://www.gnu.org/licenses/>.

(use-modules (test automake-test-lib)
             (srfi srfi-1)
             (srfi srfi-13)
             (ncurses curses)
             (ncurses form))

(setlocale LC_ALL "C")
(automake-test
 (with-utf8-locale*
  (lambda ()
    (let ((mainwin (initscr)))
      (if %is-form-driver-wide
	  (begin
	    ;; Initialize curses
	    (start-color!)
	    (cbreak!)
	    (noecho!)
	    (keypad! mainwin #t)
	    
	    ;; Initialize the color pairs
	    (init-pair! 1 COLOR_WHITE COLOR_BLUE)
	    (init-pair! 2 COLOR_WHITE COLOR_BLUE)

	    ;; Initialize the fields
	    (let ((field (list
			  (new-field 1 10 4 18 0 0)
			  (new-field 1 10 6 18 0 0)
			  (new-field 1 10 8 18 0 0))))

	      ;; Set field options
	      (set-field-fore! (first field) (color-pair 1))
	      (set-field-back! (first field) (color-pair 2))
	      (field-opts-off! (first field) O_AUTOSKIP)

	      (set-field-back! (second field) A_UNDERLINE)
	      (field-opts-off! (second field) O_AUTOSKIP)

	      ;; Create the new form and post it
	      (let ((my-form (new-form field))
		    (string1 "również")
		    (string2 "クラクフ") )
		(post-form my-form)
		(refresh mainwin)

		(addstr mainwin "Value 1:" #:y 4 #:x 10)
		(addstr mainwin "Value 2:" #:y 6 #:x 10)
		(addstr mainwin "Send" #:y 8 #:x 10)
		(refresh mainwin)
		(maybe-sleep 2)

		;; Fake the user typing a word
		(string-for-each (lambda (c)
				   (form-driver my-form c))
				 string1)
		(refresh mainwin)
		(maybe-sleep 1)

		;; Go to the beginning of the next field
		(form-driver my-form REQ_NEXT_FIELD)
		(form-driver my-form REQ_BEG_LINE)

		;; Fake the user typing a word
		(string-for-each (lambda (c)
				   (form-driver my-form c))
				 string2)

		;; Jump to the 3rd field so that the 2nd field's entry
		;; is stored.
		(form-driver my-form REQ_NEXT_FIELD)

		(refresh mainwin)
		(maybe-sleep 2)
		;; Unpost the form
		(unpost-form my-form)

		;; Check the values in the fields
		(let ((result1 (field-buffer (first field) 0))
		      (result2 (field-buffer (second field) 0)))
		  (endwin)
		  (newline)
		  (format #t "field 1: ~s~%" result1)
		  (format #t "field 2: ~s~%" result2)
		  (and (string=? (substring result1 0 (string-length string1))
				 string1)
		       (string=? (substring result2 0 (string-length string2))
				 string2))))))
	  ;; else the wide form driver is unavailable
	  'skipped)))))
