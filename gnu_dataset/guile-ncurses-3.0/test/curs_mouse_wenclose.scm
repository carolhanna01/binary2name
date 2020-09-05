;; Copyright 2016 Free Software Foundation, Inc.

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
             (ncurses curses)
             (srfi srfi-1))

;; xterm can do mouse events

(automake-test
 (let* ((mainwin (initscr))
	(win (newwin 10 10 0 0)))
   (mousemask ALL_MOUSE_EVENTS)
   (clear win)
   (refresh win)
   (let* ((in (wenclose? win 5 5)))
     (endwin)
     (newline)
     (format #t "wenclose?: ~S~%" in)
     in)))
