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
	     (srfi srfi-1)
             (ncurses curses))

;; On an xterm, you can set the default colors.

(setlocale LC_ALL "")
(automake-test
 (begin
   (let ((mainwin (initscr)))
     (start-color!)
     (init-pair! 1 COLOR_BLACK COLOR_RED)
     (let ((ret1 (use-default-colors))
	   (ret2 (init-pair! 1 -1 -1)))
       (endwin)
       (newline)
       (format #t "use-default-colors: ~s" ret1)
       (newline)
       (format #t "init-pair!: ~s" ret2)
       (newline)
       ;; Note that on MinGW, there is a bug in upstream ncurses that
       ;; returns #f for use-default-colors on xterm.  For some
       ;; color procedures on MinGW, ncurses mistakenly returns the
       ;; values for TERM=#w32con instead of TERM=xterm.
       (or (and ret1 ret2) 'skipped)))))
