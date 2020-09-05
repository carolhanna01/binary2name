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

(define-module (artanis auth)
  #:use-module (artanis auth oauth2)
  #:use-module ((rnrs) #:select (define-record-type))
  )

;; * Authentication:
;;   The process of validating whether the client is actually who they are.
;;
;; * Authorization:
;;   The process of demimining what actions you're allowed to perform once
;;   you have been authenticated.
;;
;; * Federated identity
;;   Login with an outside account.
;;
;; * Delegated authority
;;   Allowing an outside service to access resources.
;;
;; * Trusted == Confidential
;; * Untrusted == Public
