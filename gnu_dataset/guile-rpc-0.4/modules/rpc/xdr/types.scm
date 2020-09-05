;;; GNU Guile-RPC --- A Scheme implementation of ONC RPC.  -*- coding: utf-8 -*-
;;; Copyright (C) 2007, 2008, 2009, 2010  Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Guile-RPC.
;;;
;;; GNU Guile-RPC is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guile-RPC is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rpc xdr types)
  :autoload   (srfi srfi-1) (find)
  :autoload   (srfi srfi-34) (raise)
  :use-module (srfi srfi-35)
  :use-module (rpc xdr)
  :use-module (rnrs bytevectors)
  :use-module (rnrs io ports)
  :export (make-xdr-fixed-length-opaque-array
           make-xdr-variable-length-opaque-array
           xdr-variable-length-opaque-array
           make-xdr-enumeration
           xdr-boolean
           xdr-integer
           xdr-unsigned-integer
           xdr-hyper-integer xdr-unsigned-hyper-integer
           xdr-float xdr-double
           xdr-string make-xdr-string
           xdr-void %void

           &xdr-enumeration-error xdr-enumeration-error?
           xdr-enumeration-error:enumeration-alist
           xdr-enumeration-error:value))

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;;
;;; Commentary:
;;;
;;; An implementation of some of the types defined in RFC 4506.  The missing
;;; types (e.g., floats and quadruple-precision floats) are due to the lack
;;; of a proper handling in the R6RS bytevector API.
;;;
;;; Code:


;;;
;;; Additional error conditions.
;;;

(define-condition-type &xdr-enumeration-error &xdr-type-error
  xdr-enumeration-error?
  (enumeration-alist xdr-enumeration-error:enumeration-alist)
  (value             xdr-enumeration-error:value))



;;;
;;; Actual base types.
;;;

(define (array-for-each/index proc array start step)
  ;; XXX: Hack to iterate on ARRAY's elements while maintaining an index
  ;; starting at START and increased by STEP for each element.
  (array-for-each (let ((index start))
                    (lambda (value)
                      (proc value index)
                      (set! index (+ index step))))
                  array))


(define xdr-integer
  ;; Section 4.1.
  (make-xdr-basic-type 'int32 4
                       (lambda (value)
                         (and (integer? value)
                              (>= value -2147483648)
                              (<  value  2147483648)))

                       (lambda (type value bv index)
                         (bytevector-s32-set! bv index value
                                              %xdr-endianness))
                       (lambda (type port)
                         (bytevector-s32-ref (get-bytevector-n port 4)
                                             0 %xdr-endianness))

                       (lambda (type value bv index) ;; vector-encode
                         (array-for-each/index
                          (lambda (value index)
                            (bytevector-s32-set! bv index value
                                                 %xdr-endianness))
                          value index 4))
                       (lambda (type count port)     ;; vector-decode
                         (let ((bv (get-bytevector-n port (* 4 count))))
                           (list->vector
                            (bytevector->sint-list bv %xdr-endianness 4))))))

(define xdr-unsigned-integer
  ;; Section 4.2.
  (make-xdr-basic-type 'uint32 4
                       (lambda (value)
                         (and (integer? value)
                              (>= value 0)
                              (<= value 4294967295)))
                       (lambda (type value bv index)
                         (bytevector-u32-set! bv index value
                                              %xdr-endianness))
                       (lambda (type port)
                         (bytevector-u32-ref (get-bytevector-n port 4)
                                             0 %xdr-endianness))

                       (lambda (type value bv index) ;; vector-encode
                         (array-for-each/index
                          (lambda (value index)
                            (bytevector-u32-set! bv index value
                                                 %xdr-endianness))
                          value index 4))
                       (lambda (type count port)     ;; vector-decode
                         (let ((bv (get-bytevector-n port (* 4 count))))
                           (list->vector
                            (bytevector->uint-list bv %xdr-endianness 4))))))

(define (make-xdr-enumeration name enum-alist)
  ;; Section 4.3.
  (define symbol->integer
    (let ((table (make-hash-table)))
      (for-each (lambda (item)
                  (hashq-set! table (car item) (cdr item)))
                enum-alist)
      (lambda (sym)
        (hashq-ref table sym))))

  (define integer->symbol
    (let ((table (make-hash-table)))
      (for-each (lambda (item)
                  (hash-set! table (cdr item) (car item)))
                enum-alist)
      (lambda (int)
        (hash-ref table int))))

  (define (enum-error type value)
    (raise (condition
            (&xdr-enumeration-error (type type)
                                    (value value)
                                    (enumeration-alist enum-alist)))))

  (define (enum-symbol type value)
    (let ((sym (integer->symbol value)))
      (if (not sym)
          (enum-error type value)
          sym)))

  (define (decode-enum type port)
    (let* ((bv (get-bytevector-n port 4)))
      (enum-symbol type (bytevector-u32-ref bv 0 %xdr-endianness))))

  (make-xdr-basic-type name 4
                       (lambda (value)
                         (and (symbol? value)
                              (symbol->integer value)))
                       (lambda (type value bv index)
                         (let ((value (symbol->integer value)))
                           (bytevector-u32-set! bv index value
                                                %xdr-endianness)))
                       decode-enum

                       (lambda (type value bv index) ;; vector-encode
                         (array-for-each/index
                          (lambda (value index)
                            (let ((value (symbol->integer value)))
                              (bytevector-u32-set! bv index value
                                                   %xdr-endianness)))
                          value index 4))
                       (lambda (type count port)     ;; vector-decode
                         (let ((bv (get-bytevector-n port (* 4 count))))
                           (list->vector
                            (map (lambda (value)
                                   (enum-symbol type value))
                                 (bytevector->uint-list bv
                                                        %xdr-endianness
                                                        4)))))))

(define xdr-boolean
  ;; Section 4.4.
  (make-xdr-enumeration 'bool '((FALSE . 0) (TRUE . 1))))

(define xdr-hyper-integer
  ;; Section 4.5.
  (make-xdr-basic-type 'int64 8
                       integer?
                       (lambda (type value bv index)
                         (bytevector-s64-set! bv index value
                                              %xdr-endianness))
                       (lambda (type port)
                         (bytevector-s64-ref (get-bytevector-n port 8)
                                             0 %xdr-endianness))

                       (lambda (type value bv index) ;; vector-encode
                         (array-for-each/index
                          (lambda (value index)
                            (bytevector-s64-set! bv index value
                                                 %xdr-endianness))
                          value index 8))
                       (lambda (type count port)     ;; vector-decode
                         (let ((bv (get-bytevector-n port (* 8 count))))
                           (list->vector
                            (bytevector->sint-list bv %xdr-endianness 8))))))

(define xdr-unsigned-hyper-integer
  ;; Section 4.5.
  (make-xdr-basic-type 'uint64 8
                       (lambda (value)
                         (and (integer? value)
                              (>= value 0)))
                       (lambda (type value bv index)
                         (bytevector-u64-set! bv index value
                                              %xdr-endianness))
                       (lambda (type port)
                         (bytevector-u64-ref (get-bytevector-n port 8)
                                             0 %xdr-endianness))

                       (lambda (type value bv index) ;; vector-encode
                         (array-for-each/index
                          (lambda (value index)
                            (bytevector-u64-set! bv index value
                                                 %xdr-endianness))
                          value index 8))
                       (lambda (type count port)     ;; vector-decode
                         (let ((bv (get-bytevector-n port (* 8 count))))
                           (list->vector
                            (bytevector->uint-list bv %xdr-endianness 8))))))

(define xdr-float
  ;; Single-precision floating point (Section 4.6).
  (make-xdr-basic-type 'float 4
                       (lambda (value)
                         (and (number? value)
                              (inexact? value)))
                       (lambda (type value bv index)
                         (bytevector-ieee-single-set! bv index value
                                                      %xdr-endianness))
                       (lambda (type port)
                         (bytevector-ieee-single-ref
                          (get-bytevector-n port 4) 0 %xdr-endianness))))

(define xdr-double
  ;; Double-precision floating point (Section 4.7).
  (make-xdr-basic-type 'double 8
                       (lambda (value)
                         (and (number? value)
                              (inexact? value)))
                       (lambda (type value bv index)
                         (bytevector-ieee-double-set! bv index value
                                                      %xdr-endianness))
                       (lambda (type port)
                         (bytevector-ieee-double-ref
                          (get-bytevector-n port 8) 0 %xdr-endianness))))

(define xdr-single-opaque
  ;; The XDR `opaque' type cannot be used on its own.
  (make-xdr-basic-type 'opaque 1
                       (lambda (value)
                         (and (integer? value)
                              (>= value 0)
                              (<= value 255)))
                       (lambda (type value bv index)
                         (bytevector-u8-set! bv index value))
                       (lambda (type port)
                         (let ((c (get-u8 port)))
                           (if (eof-object? c)
                               (error "input shallow" c) ;; FIXME: raise
                               c)))

                       (lambda (type value bv index) ;; vector-encode
                         (array-for-each/index
                          (lambda (value index)
                            (bytevector-u8-set! bv index
                                                value))
                          value index 1))
                       (lambda (type count port)     ;; vector-decode
                         (let ((bv (get-bytevector-n port count)))
                           (list->vector (bytevector->u8-list bv))))))

(define (make-xdr-fixed-length-opaque-array size)
  ;; Section 4.9.
  (make-xdr-struct-type (make-list size xdr-single-opaque)))

(define (make-xdr-variable-length-opaque-array limit)
  ;; Section 4.10.
  (make-xdr-vector-type xdr-single-opaque limit))

(define xdr-variable-length-opaque-array
  ;; Section 4.10.
  (make-xdr-vector-type xdr-single-opaque #f))

;; Section 4.11.
;; XXX: Strings normally require zeroed padding.
(define make-xdr-string make-xdr-variable-length-opaque-array)
(define xdr-string      xdr-variable-length-opaque-array)

(define %void
  ;; The `void' value, obtained when deserializing a `void' object.
  (list 'void))

(define xdr-void
  ;; Section 4.16.
  (make-xdr-basic-type 'void 0
                       (lambda (value) #t)
                       (lambda (type value bv index) #t)
                       (lambda (type port) %void)))


;;; types.scm ends here
