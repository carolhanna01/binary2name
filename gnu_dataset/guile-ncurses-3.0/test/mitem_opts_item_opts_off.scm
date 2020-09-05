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

(use-modules (test automake-test-lib)
             (srfi srfi-1)
             (srfi srfi-13)
             (ncurses curses)
             (ncurses menu))


(automake-test
 (let* ((win (initscr))
	(item1 (new-item "item1" "description1")))
   (set-item-opts! item1 O_SELECTABLE)
   (item-opts-off! item1 O_SELECTABLE)
   (endwin)
   (newline)
   (format #t "item-opts: ~s~%" (item-opts item1))
   (equal? (item-opts item1) 0)))

