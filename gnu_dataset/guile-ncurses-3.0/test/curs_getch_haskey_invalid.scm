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
             (ncurses curses))

;; There is no terminal in the world that could have all these keys,
;; so has-key? should return #f on something.

(automake-test
 (let* ((win (initscr)))
   (cbreak!)
   (echo!)
   (keypad! win #t)
   (let* ((keylist (list
		    KEY_BREAK
		    KEY_DOWN
		    KEY_UP
		    KEY_LEFT
		    KEY_RIGHT
		    KEY_HOME
		    KEY_BACKSPACE
		    KEY_F0
		    (key-f 1)
		    (key-f 2)
		    (key-f 3)
		    (key-f 4)
		    (key-f 5)
		    (key-f 6)
		    (key-f 7)
		    (key-f 8)
		    (key-f 9)
		    (key-f 10)
		    (key-f 11)
		    (key-f 12)
		    KEY_DL
		    KEY_IL
		    KEY_DC
		    KEY_IC
		    KEY_EIC
		    KEY_CLEAR
		    KEY_EOS
		    KEY_EOL
		    KEY_SF
		    KEY_SR
		    KEY_NPAGE
		    KEY_PPAGE
		    KEY_STAB
		    KEY_CTAB
		    KEY_CATAB
		    KEY_ENTER
		    KEY_SRESET
		    KEY_RESET
		    KEY_PRINT
		    KEY_LL
		    KEY_A1
		    KEY_A3
		    KEY_B2
		    KEY_C1
		    KEY_C3
		    KEY_BTAB
		    KEY_BEG
		    KEY_CANCEL
		    KEY_CLOSE
		    KEY_COMMAND
		    KEY_COPY
		    KEY_CREATE
		    KEY_END
		    KEY_EXIT
		    KEY_FIND
		    KEY_HELP
		    KEY_MARK
		    KEY_MESSAGE
		    KEY_MOUSE
		    KEY_MOVE
		    KEY_NEXT
		    KEY_OPEN
		    KEY_OPTIONS
		    KEY_PREVIOUS
		    KEY_REDO
		    KEY_REFERENCE
		    KEY_REFRESH
		    KEY_REPLACE
		    KEY_RESIZE
		    KEY_RESTART
		    KEY_RESUME
		    KEY_SAVE
		    KEY_SBEG
		    KEY_SCANCEL
		    KEY_SCOMMAND
		    KEY_SCOPY
		    KEY_SCREATE
		    KEY_SDC
		    KEY_SDL
		    KEY_SELECT
		    KEY_SEND
		    KEY_SEOL
		    KEY_SEXIT
		    KEY_SFIND
		    KEY_SHELP
		    KEY_SHOME
		    KEY_SIC
		    KEY_SLEFT
		    KEY_SMESSAGE
		    KEY_SMOVE
		    KEY_SNEXT
		    KEY_SOPTIONS
		    KEY_SPREVIOUS
		    KEY_SPRINT
		    KEY_SREDO
		    KEY_SREPLACE
		    KEY_SRIGHT
		    KEY_SRSUME
		    KEY_SSAVE
		    KEY_SSUSPEND
		    KEY_SUNDO
		    KEY_SUSPEND
		    KEY_UNDO))
	  (names  (map keyname keylist))
	  (validity (map has-key? keylist)))
     (endwin)
     (newline)
     (not (every
	   (lambda (x) x)
	   (fold (lambda (key name valid prev)
		   (if valid
		       (format #t "OK ~s ~s ~%" key name)
		       (format #t "ERR ~s ~s~%" key name))
		   (append prev (list valid)))
		 '()
		 keylist
		 names
		 validity))))))

