;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2019
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis security nss)
  #:use-module (artanis utils)
  #:use-module (system foreign)
  #:use-module (rnrs)
  #:use-module (ice-9 match)
  #:export (nss:base64-decode
            nss:base64-encode))

(define *nss-error-msg*
  "
Please visit for more information:
https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS/SSL_functions/sslerr.html
")

(eval-when (eval load compile)
  (ffi-binding "libnss3"
    (define-c-function int NSS_NoDB_Init ('*))
    (define-c-function int NSS_Init ('*))
    (define-c-function int NSS_InitReadWrite ('*))
    (define-c-function int NSS_IsInitialized)
    (define-c-function int NSS_Initialize ('* '* '* '* uint32))
    (define-c-function int NSS_Shutdown)
    (define-c-function '* NSS_InitContext ('* '* '* '* '* uint32))
    (define-c-function '* NSSBase64_DecodeBuffer ('* '* '* unsigned-int))
    (define-c-function '* NSSBase64_EncodeItem ('* '* unsigned-int '*))
    ))

(define (no-check x) #f)
(define (<0 x) (< x 0))
(define (not-nullptr? x) (not (null-pointer? x)))
(define (->nss-boolean x)
  (match x
    (1 #t)
    (0 #f)
    (else (throw 'artanis-err 500 ->nss-boolean "Invalid value `~a'" x))))

(define-syntax-rule (gen-nss-api checker caster expr ...)
  (call-with-values (lambda () expr ...)
    (lambda (ret errno)
      (cond
       ((checker ret)
        (throw 'artanis-err 500 "NSS error: ~a" errno *nss-error-msg*))
       (else (caster ret))))))

(define-syntax-rule (gen-common-api expr ...)
  (gen-nss-api <0 identity expr ...))

(define (nss:no-db-init config-dir)
  (gen-common-api (%NSS_NoDB_Init (string->pointer config-dir))))

(define (nss:init config-dir)
  (gen-common-api (%NSS_Init (string->pointer config-dir))))

(define (nss:init-rw config-dir)
  (gen-common-api (%NSS_InitReadWrite (string->pointer config-dir))))

(define (nss:is-initialized?)
  (gen-nss-api no-check ->nss-boolean (%NSS_IsInitialized)))

(define (nss:shutdown)
  (gen-common-api (%NSS_Shutdown)))

(define (nss:init-context config-dir cert-prefix key-prefix sec-mod-name flags)
  (gen-nss-api not-nullptr? identity
               (%NSS_InitContext (string->pointer config-dir)
                                 (string->pointer cert-prefix)
                                 (string->pointer key-prefix)
                                 (string->pointer sec-mod-name)
                                 %null-pointer flags)))

(define* (make-sec-item #:optional (type 0) (data 0) (len 0))
  (make-c-struct (list int uint64 unsigned-int)
                 (list type data len)))
(define (nss:base64-decode b64-str)
  (let* ((ibuf (string->pointer b64-str))
         (len (string-utf8-length b64-str))
         (item (make-sec-item))
         (ret (%NSSBase64_DecodeBuffer %null-pointer item ibuf len)))
    (cond
     ((eq? ret %null-pointer)
      (throw 'artanis-err 500 nss:base64-decode
             "Invalid string to be decode to Base64!"))
     (else (pointer->string ret)))))

(define (nss:base64-encode str)
  (let* ((ibuf (pointer-address (string->pointer str)))
         (len (string-utf8-length str))
         (item (make-sec-item 0 ibuf len))
         (ret (%NSSBase64_EncodeItem %null-pointer %null-pointer
                                     0 item)))
    (cond
     ((eq? ret %null-pointer)
      (throw 'artanis-err 500 nss:base64-encode
             "Invalid string to be encoded to Base64!"))
     (else (pointer->string ret)))))
