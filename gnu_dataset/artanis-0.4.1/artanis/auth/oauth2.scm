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

(define-module (artanis auth oauth2)
  #:use-module (artanis route)
  #:use-module ((rnrs) #:select (define-record-type))

  )

(define (implicit-grant rc) #t)

(define (gen-user-consent rc content) #t)

(define-record-type client-credential
  (fields id secret grant-type response-type))

#;
(define (get-client-cridential rc)
(let ((fields (get-from-qstr rc "client_fields"))
(id (get-from-qstr rc "client_id"))
(secret (get-from-qstr "client_secret"))))

(make-client-credential )
)
;; This OAuth2 is implemented according to RFC6749
