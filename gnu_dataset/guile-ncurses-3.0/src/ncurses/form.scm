;; form.scm

;; Copyright 2009, 2010, 2013, 2014, 2016 Free Software Foundation, Inc.

;; This file is part of GNU Guile-Ncurses.

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

(define-module (ncurses form)
  #:use-module (ncurses eti)
  #:export (
            current-field
            data-ahead?
            data-behind?
            field-back
            field-count
            field-fore
            field-index
            field-just
            field-opts
            field-opts-off!
            field-opts-on!
            field-pad
            field-status?
            form-driver
            form-opts
            form-opts-on!
            form-opts-off!
            form-page
            form-request-by-name
            form-request-name
            form-sub
            form-win
            free-field
            free-form
            move-field
            new-page?
            pos-form-cursor
            post-form
            set-current-field!
            set-field-back!
            set-field-buffer!
            set-field-fore!
            set-field-just!
            set-field-opts!
            set-field-pad!
            set-field-status!
            set-form-opts!
            set-form-page!
            set-form-sub!
            set-form-win!
            set-max-field!
            set-new-page!
            unpost-form
            NO_JUSTIFICATION
            JUSTIFY_LEFT
            JUSTIFY_CENTER
            JUSTIFY_RIGHT
            O_VISIBLE
            O_ACTIVE
            O_PUBLIC
            O_EDIT
            O_WRAP
            O_BLANK
            O_AUTOSKIP
            O_NULLOK
            O_PASSOK
            O_STATIC
            O_NL_OVERLOAD
            O_BS_OVERLOAD
            REQ_NEXT_PAGE
            REQ_PREV_PAGE
            REQ_FIRST_PAGE
            REQ_LAST_PAGE
            REQ_NEXT_FIELD
            REQ_PREV_FIELD
            REQ_FIRST_FIELD
            REQ_LAST_FIELD
            REQ_SNEXT_FIELD
            REQ_SPREV_FIELD
            REQ_SFIRST_FIELD
            REQ_SLAST_FIELD
            REQ_LEFT_FIELD
            REQ_RIGHT_FIELD
            REQ_UP_FIELD
            REQ_DOWN_FIELD
            REQ_NEXT_CHAR
            REQ_PREV_CHAR
            REQ_NEXT_LINE
            REQ_PREV_LINE
            REQ_NEXT_WORD
            REQ_PREV_WORD
            REQ_BEG_FIELD
            REQ_END_FIELD
            REQ_BEG_LINE
            REQ_END_LINE
            REQ_LEFT_CHAR
            REQ_RIGHT_CHAR
            REQ_UP_CHAR
            REQ_DOWN_CHAR
            REQ_NEW_LINE
            REQ_INS_CHAR
            REQ_INS_LINE
            REQ_DEL_CHAR
            REQ_DEL_PREV
            REQ_DEL_LINE
            REQ_DEL_WORD
            REQ_CLR_EOL
            REQ_CLR_EOF
            REQ_CLR_FIELD
            REQ_OVL_MODE
            REQ_INS_MODE
            REQ_SCR_FLINE
            REQ_SCR_BLINE
            REQ_SCR_FPAGE
            REQ_SCR_BPAGE
            REQ_SCR_FHPAGE
            REQ_SCR_BHPAGE
            REQ_SCR_FCHAR
            REQ_SCR_BCHAR
            REQ_SCR_HFLINE
            REQ_SCR_HBLINE
            REQ_SCR_HFHALF
            REQ_SCR_HBHALF
            REQ_VALIDATION
            REQ_NEXT_CHOICE
            REQ_PREV_CHOICE
            MIN_FORM_COMMAND
            MAX_FORM_COMMAND
            dup-field
            dynamic-field-info
            field-buffer
            field-info
            field-type
            form-fields
            scale-form
            set-field-type              ; misnamed in 0.6
            set-field-type!             ; the correct name for set-field-type
            set-form-fields!
            new-field
            new-form
            form?
            field?
            %field-refcount
            TYPE_ALNUM
            TYPE_ALPHA
            TYPE_ENUM
            TYPE_INTEGER
            TYPE_NUMERIC
            TYPE_REGEXP
            TYPE_IPV4
            TYPE_NUMERIC
            %is-form-driver-wide
            )
  #:re-export (

               E_OK
               E_SYSTEM_ERROR
               E_BAD_ARGUMENT
               E_POSTED
               E_CONNECTED
               E_BAD_STATE
               E_NO_ROOM
               E_NOT_POSTED
               E_UNKNOWN_COMMAND
               E_NO_MATCH
               E_NOT_SELECTABLE
               E_NOT_CONNECTED
               E_REQUEST_DENIED
               E_INVALID_FIELD
               E_CURRENT

               ))

(load-extension "libguile-ncurses" "gucu_form_init")
(define set-field-type set-field-type!)

(define TYPE_ALNUM 'TYPE_ALNUM)
(define TYPE_ALPHA 'TYPE_ALPHA)
(define TYPE_ENUM 'TYPE_ENUM)
(define TYPE_INTEGER 'TYPE_INTEGER)
(define TYPE_NUMERIC 'TYPE_NUMERIC)
(define TYPE_REGEXP 'TYPE_REGEXP)
(define TYPE_IPV4 'TYPE_IPV4)
(define TYPE_NUMERIC 'TYPE_NUMERIC)
