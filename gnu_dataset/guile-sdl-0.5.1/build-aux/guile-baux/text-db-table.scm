;;; text-db-table.scm

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

(define-module (guile-baux text-db-table)
  #:export (read-text-db-table
            read-text-db-table-records)
  #:use-module ((ice-9 rdelim) #:select (read-line
                                         read-delimited))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((srfi srfi-13) #:select (string-trim-both)))

(define (read-zonk-trailing-ws port)    ; handle eol: LF, CR, CRLF

  (define (pc) (peek-char port))
  (define (nc) (read-char port))

  (let ((val (read port)))
    (let loop ((c (pc)))
      (case c
        ((#\space #\tab)
         (nc)
         (loop (pc)))
        ((#\cr)
         (nc)
         (and (char=? #\newline (pc))
              (nc)))
        ((#\newline)
         (nc))))
    val))

(define (grokker delim)

  (define (read-thru-delim port)
    (read-delimited delim port))

  (lambda (type)
    (case type
      ((sexp)            read)
      ((sexp-line)       read-zonk-trailing-ws)
      ((line)            read-line)
      ((rest-lines)      read-thru-delim)
      ((rest-lines-trim) (lambda (port)
                           (string-trim-both
                            (read-thru-delim port))))
      (else
       (error "unknown field type:" type)))))

(define (ponder-peculiarities correct . stuff)

  (define (the key)
    (assq-ref stuff key))

  (or (eq? 'text-db-table-config correct)
      (error "missing magic"))

  (let* ((delim (or (the 'delim) (error "missing delim")))
         (specs (or (the 'fields) (error "missing fields")))
         (names (map car specs))
         (types (map cadr specs))
         (fgroks (map (grokker delim) types)))

    (define (read-record port)

      (define (read-field name fgrok)
        (let ((val (fgrok port)))
          (and (eof-object? val)
               (throw 'done))
          (cons name val)))

      (catch 'done (lambda () (map read-field
                                   names
                                   fgroks))
             (lambda args
               #f)))

    (values (the 'meta) delim read-record)))

(define (snarf discard-meta?)
  (lambda (port)
    (let-values (((meta delim read-record) (apply ponder-peculiarities
                                                  (read port))))
      (read-delimited delim port)
      (let loop ((acc '()))
        (cond ((read-record port) => (lambda (record)
                                       (loop (cons record acc))))
              (discard-meta? (reverse! acc))
              (else (values (reverse! acc)
                            meta)))))))

;; Read the text-db-table in @var{filename} and return two values:
;; a list of records, and the table metadata (@code{#f} if none).
;; Each record is an alist whose keys are the field names, in order.
;;
(define (read-text-db-table filename)
  (call-with-input-file filename
    (snarf #f)))

;; Return the records from the text-db-table in @var{filename}.
;;
(define (read-text-db-table-records filename)
  (call-with-input-file filename
    (snarf #t)))

;;; text-db-table.scm ends here
