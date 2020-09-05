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
	     (ncurses curses)
	     (ncurses menu))

(automake-test
 (let* ((mainwin (initscr))
	(my-item (new-item "item1" "description1"))
	(my-menu (false-if-exception (new-menu (list my-item)))))
   (post-menu my-menu)
   (refresh mainwin)
   (maybe-sleep 1)
   (unpost-menu my-menu)
   (endwin)
   (newline)
   (format #t "item: ~s~%" my-item)
   (format #t "menu: ~s~%" my-menu)
   (menu? my-menu)))
