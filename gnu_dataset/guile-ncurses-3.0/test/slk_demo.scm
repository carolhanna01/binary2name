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
	     (ncurses slk))

(slk-init 1)
(define win (initscr))

(addstr win "add text to soft keys" #:y 0 #:x 0)
(refresh win)

(slk-set 1 "one" 1)
(slk-set 2 "two" 1)
(slk-set 3 "three" 1)
(slk-set 4 "four" 1)
(slk-set 5 "five" 1)
(slk-set 6 "six" 1)
(slk-set 7 "seven" 1)
(slk-set 8 "eight" 1)
(slk-refresh)
(sleep 2)

(move win 0 0)
(clrtoeol win)
(addstr win "clear text from soft keys" #:y 0 #:x 0)
(refresh win)

(slk-clear)
(sleep 2)

(move win 0 0)
(clrtoeol win)
(addstr win "restore text to soft keys" #:y 0 #:x 0)
(refresh win)

(slk-restore)
(sleep 2)

(move win 0 0)
(clrtoeol win)
(map (lambda (c)
       (addstr win (format #f "label ~s is ~s" c (slk-label c))
	       #:y c #:x 0))
     (list 1 2 3 4 5 6 7 8))
(refresh win)
(sleep 3)

(start-color!)
(init-pair! 1 COLOR_BLUE COLOR_BLACK)
(init-pair! 2 COLOR_GREEN COLOR_BLACK)
(init-pair! 3 COLOR_YELLOW COLOR_BLACK)
(init-pair! 4 COLOR_RED COLOR_BLACK)
(init-pair! 5 COLOR_CYAN COLOR_BLACK)
(init-pair! 6 COLOR_MAGENTA COLOR_BLACK)
(init-pair! 7 COLOR_WHITE COLOR_BLACK)
(init-pair! 8 COLOR_WHITE COLOR_BLUE)

(clear win)
(addchstr win (color 1 "color pair one") #:y 1 #:x 0)
(addchstr win (color 2 "color pair two") #:y 2 #:x 0)
(addchstr win (color 3 "color pair three") #:y 3 #:x 0)
(addchstr win (color 4 "color pair four") #:y 4 #:x 0)
(addchstr win (color 5 "color pair five") #:y 5 #:x 0)
(addchstr win (color 6 "color pair six") #:y 6 #:x 0)
(addchstr win (color 7 "color pair seven") #:y 7 #:x 0)
(addchstr win (color 8 "color pair eight") #:y 8 #:x 0)
(refresh win)
(sleep 3)

(slk-color! 1)
(slk-set 1 "one" 1)
(slk-refresh)
(sleep 1)
(slk-color! 2)
(slk-set 2 "two" 1)
(slk-refresh)
(sleep 1)
(slk-color! 3)
(slk-set 3 "three" 1)
(slk-refresh)
(sleep 1)
(slk-color! 4)
(slk-set 4 "four" 1)
(slk-refresh)
(sleep 1)
(slk-color! 5)
(slk-set 5 "five" 1)
(slk-refresh)
(sleep 1)
(slk-color! 6)
(slk-set 6 "six" 1)
(slk-refresh)
(sleep 1)
(slk-color! 7)
(slk-set 7 "seven" 1)
(slk-refresh)
(sleep 1)
(slk-color! 8)
(slk-set 8 "eight" 1)
(slk-refresh)
(sleep 1)
(slk-refresh)
(sleep 3)

(slk-attr-set! A_NORMAL 0)
(slk-set 1 "normal" 1)
(slk-refresh)
(sleep 1)
(slk-attr-set! A_STANDOUT 0)
(slk-set 2 "standout" 1)
(slk-refresh)
(sleep 1)
(slk-attr-set! A_UNDERLINE 0)
(slk-set 3 "underline" 1)
(slk-refresh)
(sleep 1)
(slk-attr-set! A_REVERSE 0)
(slk-set 4 "reverse" 1)
(slk-refresh)
(sleep 1)
(slk-attr-set! A_BLINK 0)
(slk-set 5 "blink" 1)
(slk-refresh)
(sleep 1)
(slk-attr-set! A_DIM 0)
(slk-set 6 "dim" 1)
(slk-refresh)
(sleep 1)
(slk-attr-set! A_BOLD 0)
(slk-set 7 "bold" 1)
(slk-refresh)
(sleep 1)
(slk-attr-set! A_PROTECT 0)
(slk-set 8 "protect" 1)
(slk-refresh)
(sleep 3)

(endwin)
