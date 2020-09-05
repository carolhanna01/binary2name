;;; pascal-pool.scm

;; Copyright (C) 2013 Thien-Thi Nguyen
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(define-module (guile-baux pascal-pool)
  #:export (pascal-pool)
  #:use-module ((srfi srfi-13) #:select (string-join))
  #:use-module ((guile-baux common) #:select (fs)))

(define (alloc name)
  (let ((len (string-length name)))
    (or (< 0 len 256)
        (error "invalid name:" name))
    len))

;; Return a string representing C code that implements
;; a Pascal string pool for @var{elems} (a list of strings).
;; With null @var{flags}, this has the general form:
;;
;; @example
;; static const @var{byte-type} @var{pool-name}[] =
;; @{
;;    @var{count} /* count */,
;;    @var{pascal-string-0},
;;    @dots{}
;; @};
;; @end example
;;
;; @noindent
;; where @var{count} is a the length of @var{elems},
;; and each @var{pascal-string-N} corresponds to the
;; @var{n}th element in @var{elems}.
;; These symbols in @var{flags} modify the general form:
;;
;; @table @code
;; @item global
;; Omit @samp{static}.
;; @item zero
;; Include a nul byte after each Pascal string.
;; @item numeric
;; Use integers only, never C char literals.
;; @item essential
;; Omit @var{count}.
;; @end table
;;
(define (pascal-pool elems byte-type pool-name . flags)

  (define (cfg x)
    (memq x flags))

  (let ((n (length elems))
        (len (map alloc elems))
        (essential (cfg 'essential))
        (numeric (cfg 'numeric))
        (zero (cfg 'zero)))
    (fs (string-append "~Aconst ~A ~A[] = ~%"
                       "{~%"
                       "  ~A~A~%"
                       "};~%")
        (if (cfg 'global) "" "static ")
        byte-type
        pool-name
        (if essential
            ""
            (fs "~A /* count */,~%  " n))
        (string-join
         (map (lambda (len name)
                (fs "~A~A,~A~A"
                    (if numeric
                        (fs "/* ~A */ " name)
                        "")
                    len
                    (string-join
                     (map (lambda (c)
                            (if (and (not numeric)
                                     (or (char-numeric? c)
                                         (char-alphabetic? c)
                                         (memq c '(#\- #\_))))
                                (fs "'~A'" c)
                                (number->string (char->integer c))))
                          (string->list name))
                     ",")
                    (if zero
                        (fs ",~A" (if numeric
                                      0
                                      "'\\0'"))
                        "")))
              len elems)
         (fs ",~%  ")))))

;;; pascal-pool.scm ends here
