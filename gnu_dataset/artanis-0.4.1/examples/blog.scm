#! /usr/bin/env guile
!#

;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2018
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

;; This is a very simple blog example for artanis

(use-modules (artanis artanis) (artanis utils) (ice-9 local-eval) (srfi srfi-1)
             (artanis irregex))

(init-server) ;; make sure call init-server at beginning

(define blog-title "Colt blog-engine")
(define footer
  (tpl->html
   `(div (@ (id "footer"))
         (p "Colt blog-engine based on "
            (a (@ (href "https://github.com/NalaGinrut/artanis")) "Artanis")
            "."))))

(get "/admin" #:with-auth #t
     (lambda (rc)
       (tpl->response "admin.tpl" (the-environment))))

;; NOTE: The default username is admin
;;       and the default passwd is 123
(get "/login" #:session #t
     (lambda (rc)
       (let ((failed (params rc "login_failed")))
         (tpl->response "login.tpl" (the-environment)))))

(post "/auth" #:auth '(table user "user" "passwd") #:session #t #:from-post #t
      (lambda (rc)
        (cond
         ((or (:session rc 'check) (:auth rc))
          (let ((referer (get-referer rc #:except "/login*"))) ;
            (if referer
                (redirect-to rc referer)
                (redirect-to rc "/admin"))))
         (else (redirect-to rc "/login?login_failed=true")))))

(define (show-all-articles articles)
  (fold (lambda (x prev)
          (let ((title (result-ref x "title"))
                (content (result-ref x "content"))
                (date (result-ref x "date")))
            (cons `(div (@ (class "post")) (h2 ,title)
                        (p (@ (class "post-date")) ,date)
                        (p ,content))
                  ;;(div (@ (class "post-meta")) ,meta)
                  prev)))
        '() articles))

(get "/search" "waiting, it's underconstruction!")

(get "/" #:raw-sql "select * from article"
     (lambda (rc)
       (let* ((articles (:raw-sql rc 'all))
              (all-posts (tpl->html (show-all-articles articles))))
         (tpl->response "index.tpl" (the-environment)))))

(post "/new_post"
  #:sql-mapping '(add new-article "insert into article (title,content,date) values (${@title},${@content},${date})")
  (lambda (rc)
    (:sql-mapping rc 'new-article #:date (strftime "%D" (localtime (current-time))))
    (redirect-to rc "/")))

(run #:use-db? #t #:dbd 'mysql #:db-username "nalaginrut" #:db-name "mmr_blog" #:db-passwd "" #:debug #f)
