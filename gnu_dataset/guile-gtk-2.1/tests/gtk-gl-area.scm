;; Copyright (C) 2006 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(load "tests.scm")
(or (getenv "DISPLAY")
    (begin
      (display "Cannot run gtk-gl-area tests without an X display\n")
      (exit 77)))

;; the source tree doesn't have gtk-gl-area.scm under the gtk-2.0 directory,
;; so can't use-modules it, instead load it directly
(primitive-load (string-append (getenv "srcdir")
			       "/../gtk-gl/gtk-gl-area.scm"))
;; then switch back to the normal guile-user module
(define-module (guile-user))

(use-modules (gtk-2.0 gtk)
	     (gtk-2.0 gtk-gl-area))



;;
;; gtk-gl-area-new
;;

(malloced-steady
 (lambda ()
   (let* ((attrs  '())
	  (widget (gtk-gl-area-new attrs))
	  (want   #t)
	  (got    (or (eq? #f widget) ;; when can't create, or something
		      (gtk-widget? widget))))
     (test "gtk-gl-area-new" attrs want got))))


(tests-end)
