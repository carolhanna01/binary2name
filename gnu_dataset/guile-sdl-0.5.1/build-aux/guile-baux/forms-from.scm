;;; forms-from.scm

;; Copyright (C) 2011 Thien-Thi Nguyen
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

(define-module (guile-baux forms-from)
  #:export (forms<-port
            forms<-file))

;; Return a list of forms @code{read} from @var{port}.
;;
(define (forms<-port port)
  (let loop ((acc '()))
    (let ((form (read port)))
      (if (eof-object? form)
          (reverse! acc)
          (loop (cons form acc))))))

;; Return a list of forms @code{read} from file @var{filename}.
;;
(define (forms<-file filename)
  (call-with-input-file filename forms<-port))

;;; forms-from.scm ends here
