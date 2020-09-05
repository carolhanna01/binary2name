;;; ---------------------->  Emacs Lisp - sourcefile  <----------------------
;;; Copyright (C) 1995/96 by International Computer Science Institute
;;; This file is part of the GNU Sather package. It is free software; you may
;;; redistribute  and/or modify it under the terms of the  GNU General Public
;;; License (GPL)  as  published  by the  Free  Software  Foundation;  either
;;; version 3 of the license, or (at your option) any later version.
;;; This  program  is distributed  in the  hope that it will  be  useful, but
;;; WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.
;;; The license text is also available from:  Free Software Foundation, Inc.,
;;; 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;; ----------->  Please email comments to <bug-sather@gnu.org>  <-----------

;;; sather-lib.el -- Emacs 19 Sather mode helper forms.

;;; Author:          1995 Kevin K. Lewis <lewikk@aud.alcatel.com>
;;; Maintainer:      <lewikk@aud.alcatel.com>
;;; Version:         1.12
;;; Last Modified:   1996/02/12 22:31:35
;;; Bugs, Comments:  Please read the documentation in sather.el on
;;;                  sending bug reports and making comments.

;;; Commentary:

;; Library-type code to complement sather-mode.  You'll have to read
;; through the file to find all the things in here.  Each section
;; begins with a `;;;' comment, so you can use outline minor mode, or
;; just search for lines beginning with three simi-colons to find
;; them.
;;
;; To include this file when you enter Sather mode, require
;; `sather-lib' in your `sather-mode-hook' in your startup (eg, in
;; your ".emacs"):
;;
;; (setq sather-mode-hook
;;       '(lambda ()
;; 	 :
;; 	 (require 'sather-lib)
;; 	 :
;; 	 ))

;;; Code:

;; Not much good without it.
(require 'sather)

;;; Documentation stuff:

(define-key sather-mode-map "\C-c\C-m" 'doc-modification)
(defun doc-modification (EXPLICIT)
  "Insert a brief modification log at the top of the buffer. Looks for
an occurrence of the RCS header \$Log."
  (interactive "P")
  (beginning-of-buffer)
  (if (re-search-forward "\$Log")
      (progn
	(end-of-line)
	(insert "\n--*"
		(brief-current-time-string)
		"(" (user-login-name) "):"))
    (message "No \$Log entry found."))
  )

(defun get-decoded-time ()
  "Return the current time as a list of strings representing:
second, minute, hour, date, month, year, day-of-week, and the
full time-string."
  ;;"Sat Jan 12 18:22:40 1991"
  ;; 012345678901234567890123
  ;;           1         2
  (let ((time-string (current-time-string)))
    (list (substring time-string 17 19)
	  (substring time-string 14 16)
	  (substring time-string 11 13)

	  (substring time-string 8 10)
	  (substring time-string 4 7)
	  (substring time-string 20 24)

	  (substring time-string 0 3)
	  time-string)))

(defun brief-current-time-string ()
  (let ((decoded-time (get-decoded-time)))
    (format "%s%s %s:%s %s"
	    (nth 4 decoded-time)	;month
	    (nth 3 decoded-time)	;date
	    (nth 2 decoded-time)	;hour
	    (nth 1 decoded-time)	;minute
	    (nth 5 decoded-time)	;year
	    )))

;;; Uncommenting regions:

;; Only define the stuff if it's not already there.  Written by Heinz
;; Schmidt of ICSI.
(if (not (fboundp 'uncomment-region))
    (progn

      (defun comment-start-or-prompt ()
	"Return the value of `comment-start', or user input."
	(or comment-start (read-input "Comment start: ")))

      (defun delete-if-looking-at (string)
	"Conditionally delete `string' if it's at point."
	(let ((len (length string))
	      (p (point)))
	  (if (string-equal (buffer-substring p (+ p len))
			    string)
	      (delete-region p (+ p len)))
	  ))

      ;; This works well for line-style comments, but I have problems
      ;; using it in c-mode.  If anyone feels like tracking down the
      ;; problem, please do.
      (defun uncomment-region (from to)
	"Uncomment lines commented by comment-region.
Do this using the value of the variables comment-start and
comment-end. Optionally insert n spaces defined by prefix arg (or
third argument if called from program)."
	(interactive "r")

	;; Extend region to include full lines.
	(save-excursion
	  (let* ((comment-start (comment-start-or-prompt))
		 (closing-comment (and comment-end
				       (not (equal comment-end ""))))
		 (next-cs (if closing-comment " *" comment-start)))

	    ;; Treat first and last line specially, but do it from
	    ;; bottom to top.  Otherwise our positions `from' `to' may
	    ;; be invalidated.
	    (goto-char from) (beginning-of-line) (setq from (point))
	    (goto-char to) (beginning-of-line) (setq to (point))

	    (let ((done nil))

	      (beginning-of-line)
	      (while (and (not done) (>= (point) from))

		;; Don't loop forever if from = top.
		(if (<= (point) from) (setq done t))

		(cond ((= (point) from) (delete-if-looking-at comment-start))
		      ((= (point) to) (delete-if-looking-at next-cs)
		       (cond (closing-comment
			      (end-of-line)
			      (backward-char (length comment-end))
			      (delete-if-looking-at comment-end))))

		      ;; Point is behind comment-start.
		      (t (delete-if-looking-at next-cs)))

		(beginning-of-line 0)) ; Move to beginning of previous line.
	      )	; let
	    ) ; let*
	  ))
      ))

;;; Fix for Emacs VC:

;; Emacs VC doesn't handle Sather comments (--).  In order to
;; conveniently use VC with Sather programs, we need to fix it so that
;; when we register the file, we tell it how Sather comments files.
;; Caveats: Right now, this only fixes VC using RCS, not SCCS.  Also,
;; this will break VC for languages with different comment styles, and
;; won't fix files already registered.  You should only load this if
;; you know you'll only VC'ing Sather files for that Emacs session.  I
;; put it in my `sather-mode-hook'.

;; NOTE: THIS FUNCTION IS OBSOLETE WITH THE RELEASE OF RCS 5.7.  It is
;; available at many sites, including `prep.ai.mit.edu', and its
;; mirrors.  It will be removed from this library soon.

;(defun sather-fix-version-control ()
;  "Fix version control for Sather programs."
;  (interactive)

;  (require 'vc)
;  (fmakunbound 'vc-backend-admin)

;  ;; This code is right out of vc.el, from the GNU Emacs distribution,
;  ;; which is subject to the GNU General Public License.
;  (defun vc-backend-admin (file &optional rev comment)
;    ;; Register a file into the version-control system
;    ;; Automatically retrieves a read-only version of the file with
;    ;; keywords expanded if vc-keep-workfiles is non-nil, otherwise
;    ;; it deletes the workfile.
;    (vc-file-clearprops file)
;    (or vc-default-back-end
;	(setq vc-default-back-end (if (vc-find-binary "rcs") 'RCS 'SCCS)))
;    (message "Registering %s..." file)
;    (let ((backend
;	   (cond
;	    ((file-exists-p (vc-backend-subdirectory-name))
;	     vc-default-back-end)
;	    ((file-exists-p "RCS") 'RCS)
;	    ((file-exists-p "SCCS") 'SCCS)
;	    (t vc-default-back-end))))
;      (cond ((eq backend 'SCCS)
;	     (vc-do-command 0 "admin" file;; SCCS
;			    (and rev (concat "-r" rev))
;			    "-fb"
;			    (concat "-i" file)
;			    (and comment (concat "-y" comment))
;			    (format
;			     (car (rassq 'SCCS vc-master-templates))
;			     (or (file-name-directory file) "")
;			     (file-name-nondirectory file)))
;	     (delete-file file)
;	     (if vc-keep-workfiles
;		 (vc-do-command 0 "get" file)))
;	    ((eq backend 'RCS)

;	     ;; This line is the whole fix.
;	     (vc-do-command 0 "rcs" file "-c-- " "-i" file)

;	     (vc-do-command 0 "ci" file;; RCS
;			    (concat (if vc-keep-workfiles "-u" "-r") rev)
;			    (and comment (concat "-t-" comment))
;			    file)
;	     )))
;    (message "Registering %s...done" file)
;    )

;  ) ; defun sather-fix-version-control

;;; More hiding stuff:

(defun sather-hide-active-region (from to)
  "Hide the marked region."
  (interactive "r")
  (sather-hide-region from to))

(defun sather-show-active-region (from to)
  "Show (unhide) the marked region."
  (interactive "r")
  (sather-show-region from to))

;;; Stubby stuff:

(defun sather-create-stubs ()
  "Create stub routines from an abstract type definition."
  (interactive)
  (query-replace-regexp
   "\\([a-zA-Z].*\\);" "\\1 is \n \traise(\"stub routine: \\1\"); end;"))

;;; Tags stuff:

;; This requires a system to build tags tables from Sather files.  If
;; you don't have a method to do this, this code may not be worth much
;; to you.  There should be a method for doing this included in the
;; Sather distribution in the "near" future.  If you can't wait ('cuz
;; it's really cool! ;-), send me mail and I might be able to help.

;; Add some keys and menus.
(define-key sather-mode-map "\C-c<" 'sather-find-base-class)
(define-key sather-mode-map "\C-c>" 'sather-find-next-base-class)
;(if (not menu-bar-tags-menu)
;    (defvar menu-bar-tags-menu (make-sparse-keymap "Tags")))
;(define-key menu-bar-tags-menu [sather-find-next-base-class]
;  '("Find Next Base Class" . sather-find-next-base-class))
;(define-key menu-bar-tags-menu [sather-find-base-class]
;  '("Find Base Class" . sather-find-base-class))

(defvar sather-base-class-list nil
  "Holds the list of base-classes for the current class.
Used in `sather-find-base-class', and `sather-find-next-base-class'.")

;; Build the list of base-classes from the list of super-types and the
;; included classes.
(defun sather-build-base-class-list ()
  "Build the list of base-classes for the current class."
  (save-excursion
    (message "Building base class list...")
    (setq sather-base-class-list nil)
    (if (or (looking-at sather-class-regexp)
	    (re-search-backward sather-class-regexp nil t))
	(progn
	  (goto-char (match-end 0))
	  (let ((end (save-excursion (sather-next-class-end) (point))))
	    (while
		(looking-at
		 "\\s-*\\($?[A-Z][A-Z0-9_]*\\)\\({$?[A-Z][A-Z0-9_]*}\\)?,?")
	      (setq sather-base-class-list
		    (cons (buffer-substring (match-beginning 1)
					    (match-end 1))
			  sather-base-class-list))
	      (goto-char (match-end 0))
	      )
	    (while (re-search-forward
		    "include\\s-+\\([A-Z][A-Z0-9_]*\\)" end t)
	      (setq sather-base-class-list
		    (cons (buffer-substring (match-beginning 1) (match-end 1))
			  sather-base-class-list))
	      )
	    ;; More logical order -- super-types are first.
	    (setq sather-base-class-list (reverse sather-base-class-list))
	    )
	  ))
    (message "Building base class list... done")
    ))

(defun sather-find-base-class ()
  "Find the first base-class of the current class."
  (interactive)
  (sather-build-base-class-list)
  (if (car sather-base-class-list)
      (if (find-tag (car sather-base-class-list))
	  (setq sather-base-class-list (cdr sather-base-class-list)))
    (message "Class has no base-class."))
  )

(defun sather-find-next-base-class ()
  "Find the next base-class of the current class."
  (interactive)
  (if (car sather-base-class-list)
      (if (find-tag (car sather-base-class-list))
	  (setq sather-base-class-list (cdr sather-base-class-list)))
    (message "Class has no other base-class."))
  )

;;; Server stuff.

(defun sather-server-buffer-undone (buffer)
  "Mark BUFFER as \"un-done\" for its client(s).
Tells the client not to hang around waiting for a `done' signal for
this buffer."
  (let ((running (eq (process-status server-process) 'run))
	(old-clients server-clients))
    (while old-clients
      (let ((client (car old-clients)))
	(delq buffer client)
	;; If client now has no pending buffers,
	;; tell it that it is done, and forget it entirely.
	(if running
	    (progn
	      (send-string server-process 
			   (format "Close: %s Done\n" (car client)))
	      (server-log (format "Close: %s Done\n" (car client)))))
	(setq server-clients (delq client server-clients)))
      (setq old-clients (cdr old-clients)))
    ))

;;; Formatting code for interface documents (contributed by Mark
;;; Bolstad):

(defvar sather-private-attr-regexp "private\\s-+attr\\s-+.+;"
  "Regular expression to find a `private attr'.")

(defun sather-make-interface-document ()
  "Creates a new buffer containing the public interface of a Sather file."
  (interactive)
  (let ((doc-buf-name (concat (buffer-name) ".doc")))
    (save-excursion
      (copy-to-buffer doc-buf-name (point-min) (point-max))
      (switch-to-buffer doc-buf-name)
      (sather-mode)
      (beginning-of-buffer)
      (while (and (not (eobp))
		  (re-search-forward sather-private-attr-regexp nil t))
	(beginning-of-line)
	(let ((beg (point)))
	  (forward-line 1)
	  (delete-region beg (point))))
      
      (beginning-of-buffer)
      (while (and (not (eobp))
		  (re-search-forward sather-routine-regexp nil t))
	(if (match-beginning 1)		; `\\(\\s-+private\\)?'
	    (let ((beg (match-beginning 1)))
	      (goto-char (sather-find-matching-end
			  (match-end sather-routine-regexp-is-match)))
	      (forward-line 1)
	      (delete-region beg (point)))
	  (progn
	    (goto-char (match-beginning 5)) ; `\\($\\|--\\|\r\\)'
	    (delete-region (point)
			   (- (sather-find-matching-end
			       (match-end sather-routine-regexp-is-match))
			      4))
	    (insert-char ?\n 2)
	    (sather-indent-command)))
	(end-of-line)
	(if (not (eobp))
	    (forward-char 1))))
    (beginning-of-line)))

;;; Using the output of -PO from the Sather compiler:

(defun sather-po-goto-location ()
  "Go to the next location specified in PO form.
Useful when running the executable in an Emacs shell window."
  (interactive)
  (if (and (search-forward "<" nil t)
	   (looking-at "\\([A-Za-z0-9._-]+\\):\\([0-9]+\\)"))
      (let ((buffername (buffer-substring (match-beginning 1) (match-end 1)))
	    (linenum (buffer-substring (match-beginning 2) (match-end 2))))
	(if (not (get-buffer buffername))
	    (error "Buffer `%s' does not exist." buffername)
	  (message "Switching to: %s:%s" buffername linenum)
	  (pop-to-buffer buffername)
	  (goto-line (string-to-int linenum)))
        )
    (error "No matching PO form."))
  )

;; Give the previous function a key.
(define-key sather-mode-map "\C-c\C-g" 'sather-po-goto-location)

;;; Provide the lib:
(provide 'sather-lib)