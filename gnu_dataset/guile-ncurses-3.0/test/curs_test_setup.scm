;;;; -*- Mode: scheme; -*-

;;;; curs_test_setup.test -- print some diagnostic info for this
;;;; test

;; Copyright 2009, 2010, 2016 Free Software Foundation, Inc.

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

(use-modules (ncurses curses)
             (test automake-test-lib))

(automake-test
 (begin
   (newline)
   (display "Load path: ") (newline)
   (write %load-path) (newline)
   (display "Sysname:                 ") (write (utsname:sysname (uname))) (newline)
   (display "Release:                 ") (write (utsname:release (uname))) (newline)
   (display "Machine:                 ") (write (utsname:machine (uname))) (newline)
   (display "Current test directory:  ") (write (getcwd)) (newline)
   (display "Current locale:          ") (display (setlocale LC_ALL "")) (newline)
   (display "TERM:                    ") (display (getenv "ORIGTERM")) (newline)
   (display "TERM for this test:      ") (display (getenv "TERM")) (newline)
   (display "isatty?:                 ") (write (isatty? (current-output-port))) (newline)
   (display "ttyname:                 ") (display (false-if-exception (ttyname (current-output-port)))) (newline)
   (display "Guile version:           ") (display (version)) (newline)
   (if %ucs4-chars
       (display "Guile character storage: UCS-4")
       (display "Guile character storage: 8-bit locale chars"))
   (newline)

   (display "Ncurses version:         ") (display (curses-version))
   (if %wide-ncurses
       (display " wide")
       (display " standard"))
   (newline)
   (newline)

   ;; (format #t "TERMINFO ~a~%" (getenv "TERMINFO"))

   (if (or (not (string? (getenv "ORIGTERM")))
           (not (string=? (getenv "ORIGTERM") "xterm")))
       (begin
         (if (or (not (string? (getenv "ORIGTERM")))
                 (string-null? (getenv "ORIGTERM")))
             (format #t "WARNING: I can't detect the terminal you're running now.~%")
             (format #t "WARNING: This terminal is a ~s.~%" (getenv "ORIGTERM")))
         (format #t
                 "This test suite assumes it is being run on an 'xterm'.
This mismatch won't affect the results of the tests, but,
you may see some strange things while the test is being run.

")
         (maybe-sleep 2)))

   (let ((win (initscr)))
     (start-color!)
     (clear win)
     (endwin)

     (format #t "Termname:              ~a~%" (termname))
     (format #t "Longname:              ~a~%" (longname))
     (format #t "Baudrate:              ~a~%" (baudrate))
     (format #t "Killchar:              ~s~%" (killchar))
     (format #t "Erasechar:             ~s~%" (erasechar))
     (format #t "Has IC?:               ~s~%" (has-ic?))
     (format #t "Has IL?:               ~s~%" (has-il?))
     (format #t "Has colors?:           ~a~%" (has-colors?))
     (format #t "Can change color?:     ~a~%" (can-change-color?))
     (format #t "Color count:           ~a~%" (colors))
     (format #t "Color-pairs:           ~a~%" (color-pairs))
     (let ((ta (term-attrs)))
       (define (yn attrib)
         (if (logtest attrib ta)
             "yes"
             " no"))
       (format #t "~%")
       (format #t "Terminal Capabilities~%")
       (format #t "------------------------------~%")
       (format #t "  PROTECT ~a~%" (yn A_PROTECT))
       (format #t "INVISIBLE ~a~%" (yn A_INVIS))
       (format #t "      ALT ~a~%" (yn A_ALTCHARSET))
       (format #t "     BOLD ~a~%" (yn A_BOLD))
       (format #t "      DIM ~a~%" (yn A_DIM))
       (format #t "    BLINK ~a~%" (yn A_BLINK))
       (format #t "  REVERSE ~a~%" (yn A_REVERSE))
       (format #t "UNDERLINE ~a~%" (yn A_UNDERLINE))
       (format #t " STANDOUT ~a~%" (yn A_STANDOUT))
       (format #t "    COLOR ~a~%" (yn A_COLOR)))

     (maybe-sleep 5)

     ;; This isn't a pass/fail test
     'skipped)))
