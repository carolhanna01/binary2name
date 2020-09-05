;; Copyright 2009, 2010, 2011, 2016 Free Software Foundation, Inc.

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
	     (ncurses curses))

(define EXPECTED_MOUSEINTERVAL 166)

(automake-test
 (let ((win (initscr)))
   (clear win)
   (refresh win)
   (let* ((m (mouseinterval -1))
	  (pass (eqv? EXPECTED_MOUSEINTERVAL m)))
     (endwin)
     (newline)
     (format #t "mouseinterval for ~s: ~s (expecting ~s)~%" (termname) m EXPECTED_MOUSEINTERVAL)
     pass)))
