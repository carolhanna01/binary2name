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

;;; sather-module.el -- Emacs 19 mode for editing Sather Module files.

;;; Author:          Kevin K. Lewis <lewikk@aud.alcatel.com>
;;; Maintainer:      <lewikk@aud.alcatel.com>
;;; Version:         1.4
;;; Last Modified:   1996/01/22 13:48:39
;;; Bugs, Comments:  Please read the documentation below on sending
;;;                  bug reports and making comments.

;;; Commentary:

;; Major mode for editing Sather Module files for use with the Sather
;; Compiler, from ICSI.

;;; Dependencies:

(require 'sather)
(require 'compile)
(require 'easymenu)

;;; User variables:

(defvar sather-module-comment-col (or sather-comment-col 32)
  "*The desired column for comments that begin to the right of code.")

(defvar sather-module-compile-command nil
  "*The command and args to use to compile Sather programs.")

(defvar sather-module-browse-command nil
  "*The command and args to use to compile Sather programs.")

(defvar sather-module-program-name nil
  "*The command and args to use to run the compiled Sather program.")

(defvar sather-module-module-file-name-face-regexp "[A-Z0-9a-z_-]+\\.p?module"
  "*Highlight pattern for source file names.")

(defvar sather-module-sather-file-name-face-regexp "[A-Z0-9a-z_-]+\\.p?sa"
  "*Highlight pattern for source file names.")

(defvar sather-module-exception-face-regexp "-\\(arith\\|assert\\|bounds\\|check\\|destroy\\|invariant\\|no_check\\|p\\(ost\\|re\\|sather_chk\\)\\|return\\)"
  "*Highlight pattern for Sather Module exceptions, which are checking options.")

(defvar sather-module-keyword-face-regexp "-\\(C_flag\\|O\\(_\\(debug\\|verbose\\)\\)?\\|com\\|de\\(bug\\|terministic\\)\\|e\\(nd\\|xternal\\)\\|fa\\(ctor_debug\\|st\\)\\|g\\|h\\(as\\|ome\\)\\|hoist_\\(const\\|iter_init\\)\\|inline\\(_\\(iters\\|routines\\)\\)?\\|m\\(ain\\|ove_while\\)\\|no_\\(\\(hoist_\\(const\\|iter_init\\)\\)\\|factor_invariants\\|inline\\|move_while\\|replace_iters\\|side_effects\\)\\|o\\(\\(nly_\\(C\\|check\\|parse\\|reachable\\)\\)\\|ptimise\\|utput_C\\)?\\|p\\(\\(r\\(etty\\|olix\\)\\)\\|sather\\(_stats\\)?\\)\\|replace_iters\\|side_\\(effects\\|debug\\)\\|v\\(erbose\\|oid\\)\\|when\\)"
  "*Highlight pattern for Sather Module keywords, which are compiler options.")

;;; Keymap:

(defvar sather-module-mode-map nil
  "Keymap for Sather Module mode.")

(if sather-module-mode-map
    ()
  (setq sather-module-mode-map (make-sparse-keymap))
  (define-key sather-module-mode-map "\C-c\C-bm"
    'sather-module-mode-submit-bug-report)
  (define-key sather-module-mode-map "\C-c\C-c" 'compile)
  (define-key sather-module-mode-map "\C-c\C-r" 'recompile)
  (define-key sather-module-mode-map "\C-c\C-k" 'kill-compilation)
  (define-key sather-module-mode-map "\eq" 'sather-fill-paragraph)
  (define-key sather-module-mode-map "\e;" 'sather-comment)

  ;; Add menus:
  (easy-menu-define
   sather-module-menu sather-module-mode-map
   "General menu for `sather-module-mode'."
   '("Sather Module"
     ["Compile Now!" recompile t]
     ["Compile" compile t]
     ["Run Program Now!"
      sather-module-run-program-now
      sather-module-program-name]
     ["Run Program" sather-module-run-program t]
     ["Run Browser Now!" sather-module-browse-now sather-module-browse-command]
     ["Run Browser" sather-module-browse t]
     "--"
     ["Comment Region" comment-region (mark)]
     ["Uncomment Region" uncomment-region
      (and (fboundp 'uncomment-region) (mark))]
     "--"
     ["Send Sather Mode Bug Report" sather-module-mode-submit-bug-report t])
   )
  )

(defvar sather-module-mode-syntax-table nil
  "Syntax table in use in sather-module-mode buffers.")

(if sather-module-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)

    ;; Make a formfeed be whitespace.
    (modify-syntax-entry ?\f " " table)

    ;; Here's your Sather Module comment starter.  We don't support
    ;; Sather Module block comments because Emacs can't handle two
    ;; different comment styles that start with different characters.
    (modify-syntax-entry ?- "< 12" table)
    ;; All these end comments.
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    (modify-syntax-entry ?\f ">" table)

    ;; These are parts of symbols.
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?# "_" table)

    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq sather-module-mode-syntax-table table)
    ))

;;; Highlighting support for use with hl319, hilit19, or font-lock:

(defvar sather-module-highlight-type nil
  "The type of highlighting being used.")

(defvar sather-module-font-lock-keywords nil
  "Keywords for highlighting with font-lock.")

(defun sather-module-add-highlighting ()
  (cond

   ;; Has hl319 (hilit R3.19) been loaded?
   ((featurep 'hl319)
    (setq sather-module-highlight-type 'hl319)
    (hilit-set-mode-patterns
     'sather-module-mode
     '(("--" "$" comment)
       ("(\\*" "\\*)" comment)
       (:buildme:
	(string "\"[^\n\"]*\"")
	(keyword sather-module-keyword-face-regexp)
	(defun sather-module-module-file-name-face-regexp)
	(label sather-module-sather-file-name-face-regexp))
       ))
    )

   ;; Else, has hilit19 (hilit R2.19) been loaded?
   ((featurep 'hilit19)
    (setq sather-module-highlight-type 'hilit19)
    (hilit-set-mode-patterns
     'sather-module-mode
     (` (("--" "$" comment)
	 ("(\\*" "\\*)" comment)
	 ("\"[^\n\"]*\"" nil string)
	 (( ,sather-module-keyword-face-regexp) 1 keyword)))
     )
    )

   ;; Else, check for font-lock.
   ((featurep 'font-lock)
    (if (not sather-module-font-lock-keywords)
	(progn
	  (if (and (not sather-xemacs-p)
		   (> emacs-minor-version 28)
		   font-lock-maximum-decoration)
	      (setq sather-module-font-lock-keywords
		    (` (((, sather-module-module-file-name-face-regexp)
			 0 font-lock-function-name-face)
			((, sather-module-sather-file-name-face-regexp)
			 0 font-lock-reference-face)
			)))
	    )
	  (setq sather-module-font-lock-keywords
		(append sather-module-font-lock-keywords
			(` (((, sather-module-exception-face-regexp)
			     1 font-lock-exception-face)
			    ((, sather-module-keyword-face-regexp)
			     1 font-lock-keyword-face)
			    )))
		)
	  ))
    ;; Set the `font-lock-keywords' to the list of patterns that have
    ;; non-nil faces.  The function is in `sather.el'.
    (setq font-lock-keywords
	  (sather-build-non-nil-face-list sather-module-font-lock-keywords))
    )
   ))

;;;### autoload
(defun sather-module-mode ()
  "A major mode for writting Sather Module files for the Sather Compiler.
\\<sather-module-mode-map>
Comments are begun with `--'.
You can use `(* ... *)' comments, but `sather-module-mode' doesn't
know how to handle them."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sather-module-mode-map)
  (setq major-mode 'sather-module-mode)
  (setq mode-name "Sather Module")
  (set-syntax-table sather-module-mode-syntax-table)

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--+\\s-*")
  (make-local-variable 'comment-start)
  (setq comment-start "--")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'font-lock-keywords)
  (sather-module-add-highlighting)

  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist (list sather-error-regexp))
  (setq sather-module-compile-command
	(concat "sacomp " (file-name-nondirectory (buffer-file-name))))
  (setq sather-module-browse-command
	(concat "sabrowse " (file-name-nondirectory (buffer-file-name))))
  (make-local-variable 'compile-command)
  (setq compile-command sather-module-compile-command)

  (easy-menu-add sather-module-menu)
  (run-hooks 'sather-module-mode-hook))

;;; Processes:

(defun sather-module-run-program (command)
  "Run the working Sather program."
  (interactive "sRun program: ")
  (shell-command (concat command "&")))

(defun sather-module-run-program-now ()
  "Run the working Sather program."
  (interactive)
  (shell-command (concat sather-module-program-name "&")))

(defun sather-module-browse (command)
  "Run the browser with the current sather-module-browse-command."
  (interactive "sBrowse command: ")
  (shell-command (concat command "&")))

(defun sather-module-browse-now ()
  "Run the browser with the current sather-module-browse-command."
  (interactive)
  (shell-command (concat sather-module-browse-command "&")))

;;; Submitting reports:

(defconst sather-module-version "1.4"
  "The sather-module-mode version number.")

(defconst sather-module-mode-help-address "lewikk@grasshopper.aud.alcatel.com"
  "Address accepting submission of sather-module-mode bug reports.")

(defun sather-module-mode-submit-bug-report ()
  "Submit via mail a bug report on sather-module-mode."
  (interactive)
  ;; Load in reporter.
  (let ((reporter-prompt-for-summary-p t))
    (and
     (y-or-n-p "Do you want to submit a report on sather-module-mode? ")
     (require 'reporter)
     (reporter-submit-bug-report
      sather-module-mode-help-address
      (concat "Report on sather-module-mode v" sather-module-version)

      ;; Significant variables.
      (list 'sather-module-comment-col
	    'sather-module-compile-command
	    'sather-module-browse-command
	    'sather-module-program-name
	    'tab-width
	    'sather-module-highlight-type)
      nil
      nil
      "Dear Kevin, nice try, but: "
      )
     )					; and
    ))

(provide 'sather-module)