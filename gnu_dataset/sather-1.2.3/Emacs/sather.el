;;; ---------------------->  Emacs Lisp - sourcefile  <----------------------
;;; Copyright (C) 1990-96 by International Computer Science Institute
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

;;; sather.el -- Emacs 19 mode for editing Sather 1.0 and 1.1 programs.

;;; Authors:         1994-1996 Kevin K. Lewis <lewikk@aud.alcatel.com>
;;;                  1990-1994 Stephen M. Omohundro <om@icsi.berkeley.edu>
;;; Maintainer:      <lewikk@aud.alcatel.com>
;;; Version:         1.71
;;; Last Modified:   1996/06/03 22:01:45
;;; Bugs, Comments:  Please read the documentation below on sending
;;;                  bug reports and making comments.

;;; Commentary:

;; Major mode for editing Sather programs.  sather-mode is based on:
;; an earlier Eiffel mode including modifications made by Bob Weiner
;; of Motorola; pieces of eiffel3.el, distributed by Tower Technology
;; Corporation; pieces of cc-mode, by Barry A. Warsaw; various and
;; sundry ideas found in the dark crevices of the Elisp world.
;;
;; The following statements, placed in a `.emacs' file or
;; `site-start.el', will cause this file to be autoloaded, and
;; sather-mode invoked when visiting `*.sa' files:
;;
;;	(autoload 'sather-mode "sather" "Sather Mode" t)
;;	(setq auto-mode-alist
;;	      (append '(("\\.sa$" . sather-mode)
;;			) auto-mode-alist))
;;
;; sather-mode is developed and tested using GNU Emacs 19.xx on Linux
;; and SunOS systems.  Thanks to Peter Arius for porting it to GNU
;; XEmacs.  It has not been tested on any non-*ix system.  If someone
;; would like to volunteer to do this, I'd be happy to include any
;; necessary modifications.  It would be really cool if the same
;; someone could provide those modifications.
;;
;; NOTE FOR XEMACS USERS: You must recompile all the `.el' files that
;; come with the Sather distribution, as those files were compiled for
;; GNU Emacs.
;;
;; BUGS AND COMMENTS: Submit bug reports and comments for sather-mode
;; using C-c C-r m (or M-x sather-mode-submit-bug-report).  Please
;; describe what you did to cause the problem, and what the problem
;; is.  A small amount of code that duplicates the problem is helpful.
;;
;; sather-mode is documented in `sather-mode.texinfo', which is
;; distributed with the current version of the Sather Compiler (as is
;; sather-mode, itself).  Use `makeinfo' to create a GNU Info version
;; of that document (if one doesn't already exist).  Printed copies
;; may be created using texi2dvi (which requires the TeX typesetting
;; system).

;;; Code:

;;; User variables:

(defvar sather-indent 3
  "*This variable gives the standard block indentation.
This value needs to be either 3, 4, or 5.")

(defvar sather-continued-line-indent (* 2 sather-indent)
  "*Extra indentation for continued lines.
Set it to 0 if you don't want any extra indentation.")

(defvar sather-keep-left-comments nil
  "*If non-nil, keep comments in the first column in the first column.")

(defvar sather-indent-comments-with-region t
  "*If non-nil, indent comments when indenting an entire region.")

(defvar sather-indent-design-comments-correctly nil
  "*If non-nil, indent comments in the design region correctly.
This is generally very expensive, so it is nil by default.")

(defvar sather-progress-interval 5
  "*Interval used to update progress status during long re-indentation.
If non-nil, must be an integer, and indicates the number of seconds
between updates of the status of the indentation.")

(defvar sather-insert-file-header-with-class-stub t
  "*If non-nil, insert a file header when inserting a class stub.")

(defvar sather-auto-indent nil
  "*If non-nil, \\[return] indents for the next line.")

(defvar sather-auto-newline nil
  "*If non-nil, `;' inserts a newline.")

(defvar sather-hungry-delete-key nil
  "*If non-nil, delete-key deletes all preceding white space.")

(defvar sather-delete-function 'backward-delete-char-untabify
  "Function called by `sather-electric-delete' when deleting a single char.")

(defvar sather-newline-prefix-indent t
  "*If non-nil, \\[return] indents the line before adding a newline.")

(defvar sather-comment-col 32
  "*The desired column for comments that begin to the right of code.")

(defvar sather-compile-command "sacomp .module"
  "*The command and args to use to compile Sather programs.")

(defvar sather-browse-command "sabrowse .module"
  "*The command and args to use to compile Sather programs.")

(defvar sather-program-name nil
  "*The command and args to use to run the compiled Sather program.")

(defvar sather-program-uptodate-check nil
  "*The command for checking whether the working program is up to date.")

;; Some regexps for highlighting Sather code.  User's may want to
;; redefine these if they have something that works better, or don't
;; want everything highlighted.
(defvar sather-builtin-face-regexp
  "\\<\\(\\$OB\\|ARRAY\\|AREF\\|AVAL\\|BOOL\\|CHAR\\|EXT_OB\\|FLT\\|FLTD\\|FLTX\\|FLTDX\\|FLTI\\|INT\\|INTI\\|\\$REHASH\\|SAME\\|STR\\|SYS\\)\\>"
  "*Highlight pattern for builtin classes.")

(defvar sather-type-face-regexp "\\<[#$]?[A-Z][A-Z0-9_]*\\>"
  "*Highlight pattern for Sather types and classes.")

(defvar sather-class-face-regexp "^\\(\\(type\\)\\|\\(\\(abstract\\|value\\|external\\(\\s-+[A-Z][A-Z0-9_]*\\)?\\|immutable\\|partial\\|spread\\)\\s-+\\)?class\\)\\s-+\\($?[A-Z][A-Z0-9_]*\\)"
  "*Highlight pattern for class definitions.")

;; Note, this only matches routines indented 3, 4, or 5 spaces.  Icky
;; but effective.  This really requires that keywords be highlighted
;; so things like routine `end's aren't highlighted as routines.
(defvar sather-routine-face-regexp "^   \\(\\s-\\|\\s-\\s-\\)?\\(private\\s-+\\)?\\([a-zA-Z_][a-zA-Z0-9_]*!?\\)"
  "*Highlight pattern four routine definitions.")

(defvar sather-constant-numeric-face-regexp-a
  "[-+]?\\<[0-9_]+\\(\\.[0-9_]+\\([Ee][+-]?[0-9_]+\\)?\\)?[dix]?\\>"
  "*Highlight pattern for Sather numeric constants.")

(defvar sather-constant-numeric-face-regexp-b
  "\\<0[box][0-9A-Fa-f_]+\\>"
  "*Highlight pattern for Sather numeric constants.")

(defvar sather-constant-value-face-regexp
  "\\('\\\\?.'\\|\\<nil\\>\\|\\<self\\>\\)"
  "*Highlight pattern for Sather constant values.")

(defvar sather-iterator-face-regexp "[A-Za-z_][A-Za-z0-9_]*!"
  "*Highlight pattern for Sather iterators.")

(defvar sather-sugar-face-regexp
  "\\<\\(aget\\|aset\\|div\\|is_\\(eq\\|geq\\|gt\\|leq\\|lt\\|neq\\)\\|m\\(inus\\|od\\)\\|n\\(egate\\|ot\\)\\|p\\(lus\\|ow\\)\\|times\\)\\>"
  "*Highlight pattern for Sather sugar routines.")

(defvar sather-exception-face-regexp "\\<\\(assert\\|exception\\|initial\\|post\\|pre\\|protect\\|raise\\|result\\)\\>"
  "*Highlight pattern for Sather exception expressions.")

(defvar sather-private-face-regexp "\\<private\\>"
  "*Highlight pattern for the `private' keyword.")

(defvar sather-attr-face-regexp
  "\\(attr\\s-+\\)\\([A-Za-z0-9_]+\\(\\(\\s-*,\\s-*[A-Za-z0-9_]+\\)+\\)?\\):"
  "*Highlight pattern for the variables in an `attr' declaration statement.")

(defvar sather-shared-face-regexp
  "\\(shared\\s-+\\)\\([A-Za-z0-9_]+\\(\\(\\s-*,\\s-*[A-Za-z0-9_]+\\)+\\)?\\):"
  "*Highlight pattern for the variables in a `shared' declaration statement.")

(defvar sather-include-face-regexp "\\(include\\s-+\\)\\([A-Z]+[A-Z0-9_]*\\)"
  "*Highlight pattern for the class in an `include' statement.")

(defvar sather-keyword-face-regexp "\\<\\(ITER\\|ROUT\\|SAME\\|a\\(bstract\\|n[dy]\\|ssert\\|ttr\\)\\|break!\\|c\\(ase\\|lass\\|onst\\|reate\\)\\|do@?\\|e\\(ls\\(e\\|if\\)\\|nd\\|x\\(ception\\|ternal\\)\\)\\|f\\(alse\\|ork\\)\\|guard\\|i\\([fs]\\|mmutable\\|n\\(clude\\|itial\\|out\\|variant\\)\\)\\|lo\\(ck\\|op\\)\\|main\\|new\\|o\\(nce\\|r\\|ut\\)\\|p\\(ar\\(loop\\|tial\\)?\\|ost\\|r\\(e\\|ivate\\|otect\\)\\)\\|quit\\|r\\(aise\\|e\\(adonly\\|sult\\|turn\\)\\)\\|s\\(elf\\|hared\\|pread\\|tub\\)\\|t\\(hen\\|rue\\|ype\\(case\\)?\\)\\|until!\\|v\\(alue\\|oid\\)\\|wh\\(en\\|ile!\\)\\|yield\\)\\>"
  "*Highlight pattern for Sather keywords.")

;; Things for documenting code ...

(defvar sather-site
  (concat "@"
	  (cond ((and (boundp 'user-mail-address)
		      (string-match "@\\([a-z0-9A-Z.-]+\\)" user-mail-address))
		 (substring user-mail-address 
			    (match-beginning 1)
			    (match-end 1)))
		((boundp 'gnus-local-domain) 
		 gnus-local-domain)
		(t (system-name))))
  "*Mailing address of site where mode is being used.
Should include initial \@ sign. Use nil for none.")

(defvar sather-short-copyright
  "-- Copyright (C) 1995 by International Computer Science Institute\n"
  "*Short copyright notice to be inserted in the header.
Should be commented and include trailing newline.  Use nil for none.")

(defvar sather-long-copyright
  "-- COPYRIGHT NOTICE: This code is provided WITHOUT ANY WARRANTY
-- and is subject to the terms of the GNU GENERAL PUBLIC
-- LICENSE contained in the file: Sather/Doc/GPL of the
-- Sather distribution. The license is also available from ICSI,
-- 1947 Center St., Suite 600, Berkeley CA 94704, USA.\n"
  "*Long copyright notice to be inserted in the header.
Should be commented and have trailing newlines.  Use nil for none.")

(defvar sather-manual-name "sather"
  "*The base-name of the Info version of the Sather Manual.")

(defvar sather-tutorial-name "sather-tutorial"
  "*The base-name of the Info version of the Sather Eclectic Tutorial.")

;;; No user definable variables beyond this point:

;; This currently matches classes from 1.0 and 1.1.  The 1.0 stuff
;; should go away eventually ...
(defconst sather-class-regexp-mod-match 1)
(defconst sather-class-regexp-name-match 6)
(defconst sather-class-regexp
  "^\\(\\(type\\)\\|\\(\\(abstract\\|value\\|external\\(\\s-+[A-Z][A-Z0-9_]*\\)?\\|immutable\\|partial\\|spread\\)\\s-+\\)?class\\)\\s-+\\($?[A-Z][A-Z0-9_]*\\)"
  "Matches the first line of a class definition.")

(defconst sather-class-end-regexp "^end;"
  "Matches the last line of a class definition.")

;; For some reason, using `^M' works here, but `?\r' doesn't.  If
;; anyone knows why, please tell me.  It seems to have something to do
;; with the \\(...\\) grouping.
(defconst sather-routine-regexp-private-match 1)
(defconst sather-routine-regexp-name-match 2)
(defconst sather-routine-regexp-is-match 3)
(defconst sather-routine-regexp-terminate-match 4)
(defconst sather-routine-regexp
  "^\\(\\s-+private\\)?\\s-+\\([A-Za-z0-9_]+!?\\)[^;]*\\s-\\(is\\)\\(\\s-+\\|$\\|--\\|\\)"
  "Matches the first line of a class routine.")

(defconst sather-block-regexp-name-match 2)
(defconst sather-block-regexp
  "\\(^\\|\\s-+\\)\\(fork\\|if\\|is\\|lock\\|loop\\|par\\(loop\\)?\\|protect\\|\\(type\\)?case\\|end;?\\)\\($\\|\\s-\\)"
  "Matches an expression that opens or closes a block.")

(defconst sather-midblock-regexp-name-match 2)
(defconst sather-midblock-regexp
  "\\(^\\|\\s-+\\)\\(do\\(@[A-Za-z_][A-Za-z0-9_]+!\\)?\\|els\\(e\\|if\\)\\|then\\|when\\)\\($\\|\\s-\\)"
  "Matches a midblock clause, e.g., `do', `else', `then', `when'.")

(defconst sather-error-regexp
  '("^\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)
  "The regexp to process Sather Compiler errors.")

(defvar sather-feature-string nil
  "Feature string to display as minor mode.")

(make-variable-buffer-local 'sather-auto-newline)
(make-variable-buffer-local 'sather-auto-indent)
(make-variable-buffer-local 'sather-hungry-delete-key)
(make-variable-buffer-local 'sather-newline-prefix-indent)
(make-variable-buffer-local 'sather-feature-string)

;;; Portability:

(defconst sather-xemacs-p (string-match "XEmacs" emacs-version)
  "Non-nil, if running under GNU XEmacs.")

(defconst sather-language-version 'sather-1.1
  "The version of the Sather Language (Compiler) being used.
Valid values for this are `sather-1.1', `sather-1.0', etc.
Do *not* use something like `1.0'.")

;;; Dependencies:

(require 'info)
(require 'compile)
(require 'easymenu)

;;; Keymap:

(defvar sather-mode-map nil
  "Keymap for Sather mode.")

(if sather-mode-map
    ()
  (setq sather-mode-map (make-sparse-keymap))
  (define-key sather-mode-map "\C-c\C-rc" 'sather-compiler-submit-bug-report)
  (define-key sather-mode-map "\C-c\C-rm" 'sather-mode-submit-bug-report)
  (define-key sather-mode-map "\C-c\C-c" 'compile)
  (define-key sather-mode-map "\C-c\C-r" 'recompile)
  (define-key sather-mode-map "\C-c\C-k" 'kill-compilation)
  (define-key sather-mode-map "\C-c\C-nc" 'sather-new-class)
  (define-key sather-mode-map "\C-c\C-na" 'sather-new-abstract-class)
  (define-key sather-mode-map "\C-c\C-nt" 'sather-new-abstract-class)
  (define-key sather-mode-map "\C-c\C-w" 'sather-which-class)
  (define-key sather-mode-map "\C-c\C-l" 'sather-topic-lookup)
  (define-key sather-mode-map "\eq" 'sather-fill-paragraph)
  (define-key sather-mode-map "\e\C-q" 'sather-indent-block)

  (define-key sather-mode-map "\C-c\C-hc" 'sather-hide-current-class)
  (define-key sather-mode-map "\C-c\C-hl" 'sather-hide-classes)
  (define-key sather-mode-map "\C-c\C-hr" 'sather-hide-current-routine)
  (define-key sather-mode-map "\C-c\C-hs"
    'sather-hide-current-class-routines)
  (define-key sather-mode-map "\C-c\C-ha" 'sather-hide-routines)
  (define-key sather-mode-map "\C-c\C-h-" 'sather-hide-comments)
  (define-key sather-mode-map "\C-c\C-sc" 'sather-show-current-class)
  (define-key sather-mode-map "\C-c\C-sl" 'sather-show-classes)
  (define-key sather-mode-map "\C-c\C-sr" 'sather-show-current-routine)
  (define-key sather-mode-map "\C-c\C-ss"
    'sather-show-current-class-routines)
  (define-key sather-mode-map "\C-c\C-sa" 'sather-show-routines)
  (define-key sather-mode-map "\C-c\C-s=" 'sather-show-comments)
  (define-key sather-mode-map "\C-c\C-sA" 'sather-show-all)

  (define-key sather-mode-map "\C-c\C-fc" 'sather-forward-class)
  (define-key sather-mode-map "\C-c\C-fr" 'sather-forward-routine)
  (define-key sather-mode-map "\C-c\C-bc" 'sather-backward-class)
  (define-key sather-mode-map "\C-c\C-br" 'sather-backward-routine)

  (if sather-xemacs-p
      (progn
	(define-key sather-mode-map 'tab 'sather-indent-command)
	(define-key sather-mode-map '(control tab) 'sather-indent-command)
	(define-key sather-mode-map 'return 'sather-return)
	(define-key sather-mode-map 'delete 'sather-electric-delete))
    (define-key sather-mode-map "\t" 'sather-indent-command)
    (define-key sather-mode-map [C-tab] 'sather-indent-command)
    (define-key sather-mode-map "\r" 'sather-return)
    (define-key sather-mode-map "\177" 'sather-electric-delete))

  (define-key sather-mode-map "\e;" 'sather-comment)
  (define-key sather-mode-map ";" 'sather-electric-semi)

  ;; Mouse bindings.
  (if sather-xemacs-p
      (define-key sather-mode-map '(shift button2)
	'sather-mouse-hide-something)
    (define-key sather-mode-map [S-mouse-2] 'sather-mouse-hide-something))

  ;; Add menus:

  ;; Menu to show hidden things.
  (easy-menu-define 
   sather-show-menu sather-mode-map "\"Show\" menu for `sather-mode'."
   '("Sa/Show"
     ["Show Current Class" sather-show-current-class t]
     ["Show All Classes" sather-show-classes t]
     ["Show Current Routine" sather-show-current-routine t]
     ["Show Current Class Routines" sather-show-current-class-routines t]
     ["Show All Routines" sather-show-routines t]
     ["Show All Comments" sather-show-comments t]
     ["Show All" sather-show-all t]
     ))

  ;; Menu to hide things.
  (easy-menu-define 
   sather-hide-menu sather-mode-map "\"Hide\" menu for `sather-mode'."
   '("Sa/Hide"
     ["Hide Current Class" sather-hide-current-class t]
     ["Hide All Classes" sather-hide-classes t]
     ["Hide Current Routine" sather-hide-current-routine t]
     ["Hide Current Class Routines" sather-hide-current-class-routines t]
     ["Hide All Comments" sather-hide-comments t]
     ["Hide All Routines" sather-hide-routines t]
     ))

  ;; Menu for general Sather things.
  (easy-menu-define 
   sather-menu sather-mode-map "General menu for `sather-mode'."
   '("Sather"
     ["Compile Now!" recompile t]
     ["Compile" compile t]
     ["Run Program Now!" sather-run-program-now sather-program-name]
     ["Run Program" sather-run-program t]
     ["Run Browser Now!" sather-browse-now sather-browse-command]
     ["Run Browser" sather-browse t]
     "--"
     ["Fill Comment Paragraph" sather-fill-paragraph t]
     ["Comment Region" comment-region (mark)]
     ["Uncomment Region" uncomment-region
      (and (fboundp 'uncomment-region) (mark))]
     ["Indent Region" indent-region (mark)]
     ["New Class" sather-new-class t]
     ["New Abstract Class" sather-new-abstract-class t]
     ["Which Class?" sather-which-class t]
     ["Lookup Topic" sather-topic-lookup t]
     "--"
     ("Documentation"
      ["Browse Sather Manual" sather-browse-manual t]
      ["Browse Eclectic Tutorial" sather-browse-tutorial t]
      ["Describe Sather Mode" sather-describe-mode t])
     "--"
     ("Options"
      ["Auto-Indent" sather-toggle-indent-state t]
      ["Auto-Newline" sather-toggle-newline-state t]
      ["Hungry Delete Key" sather-toggle-hungry-state t]
      ["Newline Prefix Indent" sather-toggle-prefix-state t])
     "--"
     ("Send Bug Report"
      ["Send Sather Compiler Bug Report" sather-compiler-submit-bug-report t]
      ["Send Sather Mode Bug Report" sather-mode-submit-bug-report t])
     ))
  )

;;; Syntax:

(defvar sather-mode-syntax-table nil
  "Syntax table in use in sather-mode buffers.")

(if sather-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)

    ;; Make a formfeed be whitespace.
    (modify-syntax-entry ?\f " " table)

    ;; Here's your Sather comment starter.
    (modify-syntax-entry ?- ". 12" table)
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
    (modify-syntax-entry ?| "(|" table) ; A bit of a kludge for pretty arrays.
    (modify-syntax-entry ?\' "\"" table)
    (setq sather-mode-syntax-table table)
    ))

;;; Abbrevs:

(defvar sather-mode-abbrev-table nil
  "Abbrev table in use in sather-mode buffers.")
(define-abbrev-table 'sather-mode-abbrev-table ())

;;; Highlighting support for use with hl319, hilit19, or font-lock:

(defvar sather-highlight-type nil
  "The highlighting support being used.")

(defvar sather-font-lock-keywords nil
  "Keywords for highlighting with font-lock.")

;; These are faces added for Sather syntax:

(defvar font-lock-class-face 'font-lock-class-face
  "Face to use for built-in classes.")

(defvar font-lock-builtin-face 'font-lock-builtin-face
  "Face to use for built-in classes.")

(defvar font-lock-sugar-face 'font-lock-sugar-face
  "Face to use for Sather sugar expressions.")

(defvar font-lock-exception-face 'font-lock-exception-face
  "Face to use for exception clauses.")

(defvar font-lock-iterator-face 'font-lock-iterator-face
  "Face to use for iterator identifiers.")

(defvar font-lock-constant-face 'font-lock-constant-face
  "Face to use for numeric and symbolic constants.")

(defvar font-lock-private-face 'font-lock-private-face
  "Face to use for the private keyword.")

(defvar font-lock-attr-face 'font-lock-attr-face
  "Face to use for the attr keyword.")

(defvar font-lock-shared-face 'font-lock-shared-face
  "Face to use for the shared keyword.")

(defvar font-lock-include-face 'font-lock-include-face
  "Face to use for the include keyword.")

;; XEmacs requires `find-face', while GNU Emacs can associated symbols
;; with faces.
(if (not sather-xemacs-p)
    (defun find-face (face)
      face))

;; Rebuild the `sather-font-lock-keywords' so that it doesn't include
;; patterns that have a face-variable set to `nil', or a symbol that
;; is not defined as a face.
(defun sather-build-non-nil-face-list (ls)
  (let ((el (car ls)))
    (if (null el)
	nil
      (if (or (null (nth 2 el))
	      (not (facep (find-face (nth 2 el)))))
	  (sather-build-non-nil-face-list (cdr ls))
	(cons el (sather-build-non-nil-face-list (cdr ls))))
      )
    ))

(defun sather-add-highlighting ()
  (cond

   ;; Has hl319 (hilit R3.19) been loaded?
   ((featurep 'hl319)
    (setq sather-highlight-type 'hl319)
    (hilit-set-mode-patterns
     'sather-mode
     '(("--" "$" comment)
       (:buildme:
	(string "\"[^\n\"]*\"")
	(:and: "^   \\(\\s-\\|\\s-\\s-\\)?\\(private\\s-+\\)?"
	 (defun "\\([a-zA-Z_][a-zA-Z0-9_]*!?\\)"))
	(builtin sather-builtin-face-regexp)
	(constant sather-constant-numeric-face-regexp-a)
	(constant sather-constant-numeric-face-regexp-b)
	(constant sather-constant-value-face-regexp)
	(private sather-private-face-regexp)
	(attr sather-attr-face-regexp)
	(shared sather-shared-face-regexp)
	(include sather-include-face-regexp)
	(type sather-type-face-regexp)
	(iterator sather-iterator-face-regexp)
	(exception sather-exception-face-regexp)
	(sugar sather-sugar-face-regexp)
	(keyword sather-keyword-face-regexp))
       ))
    )

   ;; Else, has hilit19 (hilit R2.19) been loaded?
   ((featurep 'hilit19)
    (setq sather-highlight-type 'hilit19)
    (hilit-set-mode-patterns
     'sather-mode
     (` (("--" "$" comment)
	 ("\"[^\n\"]*\"" nil string)
	 (( ,sather-builtin-face-regexp) nil builtin)
	 (( ,sather-type-face-regexp) nil type)
	 (( ,sather-constant-numeric-face-regexp-a) nil constant)
	 (( ,sather-constant-value-face-regexp) nil constant)
	 (( ,sather-iterator-face-regexp) nil iterator)
	 (( ,sather-sugar-face-regexp) nil sugar)
	 (( ,sather-keyword-face-regexp) 1 keyword)))
     )
    )

   ;; Else, check for font-lock.
   ((featurep 'font-lock)
    (setq sather-highlight-type 'font-lock)
    (if (not sather-font-lock-keywords)
	(let ((fl-level))

	  ;; Get the value of `fl-level'.
	  (if sather-xemacs-p
	      (setq fl-level font-lock-use-maximal-decoration)
	    (setq fl-level font-lock-maximum-decoration))

	  ;; Get the base-line keywords.
	  (setq sather-font-lock-keywords
		(` (
		    ((, sather-type-face-regexp)
		     0 font-lock-type-face)
		    ((, sather-keyword-face-regexp)
		     1 font-lock-keyword-face)
		    ((, sather-routine-face-regexp)
		     3 font-lock-function-name-face)
		    )))

	  ;; Do we want additional patterns?
	  (if (not (null fl-level))
	      (progn

		;; Level-2+ keywords.
		(if (or (eq fl-level t) (> fl-level 1))
		    (setq sather-font-lock-keywords
			  (append sather-font-lock-keywords
			   (` (
			       ((, sather-exception-face-regexp)
				1 font-lock-exception-face t)
			       ((, sather-private-face-regexp)
				0 font-lock-private-face t)
			       ((, sather-attr-face-regexp)
				2 font-lock-attr-face t)
			       ((, sather-shared-face-regexp)
				2 font-lock-shared-face t)
			       ((, sather-include-face-regexp)
				2 font-lock-include-face t)
			       ((, sather-iterator-face-regexp)
				0 font-lock-iterator-face t)
			       ))
			   ))
		  )

		;; Level-3+ keywords.
		(if (or (eq fl-level t) (> fl-level 2))
		    (setq sather-font-lock-keywords
			  (append sather-font-lock-keywords
			   (` (
			       ((, sather-class-face-regexp)
				6 font-lock-class-face t)
			       ((, sather-builtin-face-regexp)
				1 font-lock-builtin-face t)
			       ((, sather-constant-numeric-face-regexp-a)
				0 font-lock-constant-face t)
			       ((, sather-constant-numeric-face-regexp-a)
				0 font-lock-constant-face t)
			       ((, sather-constant-value-face-regexp)
				1 font-lock-constant-face t)
			       ((, sather-sugar-face-regexp)
				1 font-lock-sugar-face t)
			       ))
			   ))
		  )
		))			; if (not (null fl-level))
	  ))
    ;; Set the `font-lock-keywords' to the list of patterns that have
    ;; non-nil faces.
    (setq sather-font-lock-keywords
	  (sather-build-non-nil-face-list sather-font-lock-keywords))
    (setq font-lock-keywords sather-font-lock-keywords)
    (setq font-lock-defaults '(sather-font-lock-keywords nil nil nil))
    (put 'sather-mode 'font-lock-defaults '(sather-font-lock-keywords))
    )
   ))

;; This adds variables where we can place the `/hinp' string to
;; display the current active feature list.
(defun sather-adjust-modeline ()
  (or (assq 'sather-feature-string minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(sather-feature-string sather-feature-string)
		  minor-mode-alist)))) 

;;;###autoload
(defun sather-mode ()
  "A major mode for developing programs using the Sather language.
\\<sather-mode-map>
Comments are begun with `--'.
Paragraphs are separated by blank lines.
Delete converts tabs to spaces as it moves back.
Tab anywhere on a line indents it according to Sather conventions.

\\[sather-comment] inserts and indents a comment on the line, or indents an
existing comment if there is one.

\\[sather-mouse-hide-something] will toggle the hidden state of a
class or routine.

A new class skeleton is inserted (along with a file header if
neccessary) with:

 \\[sather-new-class] <class>
 \\[sather-new-abstract-class] <class>

Variables controlling style:
   sather-indent                Indentation of Sather statements.
   sather-continued-line-indent Indentation of continued statements.
   sather-comment-col           Goal column for inline comments.
   sather-keep-left-comments    Don't indent comments in the first column.
   sather-indent-comments-with-region Indent comments when indentiong regions.
   sather-auto-indent           Automatically indent the next line on <return>.
   sather-auto-newline          Automatically insert a newline on `;'.
   sather-hungry-delete-key     Delete key consumes all previous whitespace.
   sather-newline-prefix-indent Automatically indent line before newline.
   sather-site                  Mailing address of site for header.
   sather-short-copyright       Short copyright message for header.
   sather-long-copyright        Long copyright message for header.

Turning on Sather mode calls the value of the variable sather-mode-hook
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sather-mode-map)
  (setq major-mode 'sather-mode)
  (setq mode-name "Sather")

  ;; For class/routine hiding.
  (setq selective-display t)
  (setq selective-display-ellipses t)

  (set-syntax-table sather-mode-syntax-table)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sather-indent-command)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'sather-indent-region)
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
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist (list sather-error-regexp))
  (make-local-variable 'compile-command)
  (setq compile-command sather-compile-command)
  (make-local-variable 'font-lock-keywords)
  (make-local-variable 'font-lock-defaults)
  (setq local-abbrev-table 'sather-mode-abbrev-table)
  (sather-adjust-modeline)
  (sather-update-modeline)
  (sather-add-highlighting)
  (easy-menu-add sather-menu)
  (easy-menu-add sather-hide-menu)
  (easy-menu-add sather-show-menu)
  (run-hooks 'sather-mode-hook))

;; Insert the file header at point.
(defun sather-header ()
  (let ((header (read-string "File header: "
			     (concat "-- " (buffer-name) ": "))))
    (if (eq sather-language-version 'sather-1.1)
	(insert "---------------------------> Sather 1.1 source file <--------------------------\n"))
    (insert
     header "\n"
     "-- Author: " (user-full-name)
     " <" (user-login-name) sather-site ">\n"

     ;; Had to breakup the RCS headers so that they wouldn't be
     ;; considered active.
     sather-short-copyright "-- $"
     "Id"
     "$\n--\n" sather-long-copyright
     "-------------------------------------------------------------------\n"
     "-- $"
     "Log"
     "$\n"
     "-------------------------------------------------------------------\n"
     )
    ))

(defun sather-new-class (&optional sig)
  "Insert a `class' template."
  (interactive)

  (let ((name (read-string "Class name: ")))

    ;; Make sure we don't start on top of an existing line.
    (beginning-of-line)
    (if (not (sather-empty-line-p))
	(newline))

    ;; See if we need to insert a file header.
    (if (and sather-insert-file-header-with-class-stub
	     (not (sather-prev-class-p)))
	(sather-header))

    ;; Is the name entered an abstract identifier?
    (if (string= (substring name 0 1) "$")
	(if (eq sather-language-version 'sather-1.0)
	    (setq sig "type ")
	  (setq sig "abstract ")))

    ;; Find out what sort of class is desired.
    (if sig
	()
      (setq sig (concat
		 (or (if (y-or-n-p "Abstract? ")
			 (if (eq sather-language-version 'sather-1.0)
			     "type "
			   "abstract ")
		       nil)
		     (if (y-or-n-p "External? ")
			 (concat "external "
				 (upcase (read-string "Language? ")) " ")
		       nil)
		     (if (y-or-n-p "Partial? ") "partial " nil)
		     (if (eq sather-language-version 'sather-1.0)
			 (if (y-or-n-p "Value? ")
			     "value " nil)
		       (if (y-or-n-p "Immutable? ")
			   "immutable " nil)
		       )
		     ))))

    ;; Add a `$' if the class is abstract and there's not one.
    (if (or (string= sig "abstract ") (string= sig "type "))
	(if (not (string-match "$" name))
	    (setq name (concat "$" name))))

    (insert
     (if (eq sather-language-version 'sather-1.0)
	 (if sig
	     sig
	   "class ")
       (concat sig "class "))
     (upcase name) " is\n\n"
     "end; -- class " (upcase name) "\n"
     "-------------------------------------------------------------------\n\n")
    )

  (re-search-backward sather-class-end-regexp)
  (sather-indent-line))

(defun sather-new-abstract-class ()
  "Insert an `abstract class' template.  A shortcut from \\[sather-new-class]."
  (interactive)
  (if (= sather-language-version 'sather-1.0)
      (sather-new-class "type ")
    (sather-new-class "abstract ")))

;;; Electric keys:

(defun sather-return (arg)
  "Return and conditionally indent the new line."
  (interactive "p")
  (save-excursion
    (if (and sather-newline-prefix-indent
	     (not (sather-empty-line-p)))
	(sather-indent-command)))
  (newline arg)
  (if sather-auto-indent
      (progn
	(sather-indent-line)
	(skip-chars-forward " \t"))))

;; Stolen from cc-mode (GPL).
(defun sather-electric-delete (arg)
  "Deletes preceding character or whitespace.
If `sather-hungry-delete-key' is non-nil, then all preceding
whitespace is deleted.  However, if an ARG is supplied, or
`sather-hungry-delete-key' is nil, or point is inside a string, then
the function in `sather-delete-function' is called."
  (interactive "P")
  (if (or (not sather-hungry-delete-key)
	  arg
	  (sather-in-quoted-string-p))
      (funcall sather-delete-function (prefix-numeric-value arg))
    (let ((p0 (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) p0)
	  (delete-region (point) p0)
	(funcall sather-delete-function 1))
      )))

(defun sather-electric-semi (arg)
  "Insert a semi-colon, indent, and conditionally add a newline."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (sather-indent-line)
  (if sather-auto-newline
      (progn
	(newline)
	(if sather-auto-indent
	    (progn
	      (sather-indent-line)
	      (skip-chars-forward " \t")))
	))
  )

;;; Indentation:

(defun sather-indent-line ()
  "Indent the current line as Sather code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (sather-calc-indent)))
  (skip-chars-forward " \t"))

(defun sather-indent-command ()
  "Conditionally indent the current line and position the point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((in-to (sather-calc-indent)))
      (if (/= (current-column) in-to)
	  (progn (delete-horizontal-space)
		 (indent-to in-to))
	)))
  (skip-chars-forward " \t"))

;; Get the current block indentation.  We do this by looking for the
;; previous block open keyword, and finding out how much it is
;; indented.
(defun sather-get-block-indent ()
  (save-excursion
    (let ((depth 1)
	  (mb)
	  (area nil)
	  (found nil))
      (while (not found)
	(if (re-search-backward sather-block-regexp nil t)
	    (progn
	      (setq mb (match-beginning sather-block-regexp-name-match))
	      (goto-char mb)
	      (if (and (not (sather-in-quoted-string-p mb))
		       (not (sather-in-comment-p mb)))
		  (progn
		    ;; Look for something we can be sure about.
		    ;; Because we're going backwards, `end's increase
		    ;; our depth, while open clauses decrease it.
		    (cond ((looking-at "^end") ; We found a class end.
			   (setq area 'none)) ; We're outside of everything.
			  ((looking-at "end")
			   (setq depth (1+ depth)))
			  ((looking-at "is")
			   ;; Save the context of the `is'.
			   (if (sather-routine-is-p)
			       (setq area 'routine)
			     (setq area 'class))
			   (setq depth (1- depth))
			   (setq found t))
			  (t
			   (setq depth (1- depth))))

		    ;; If we're back at our current level (1), or the
		    ;; surrounding block level, we know enough.
		    (if (or (= depth 0) (= depth 1))
			(setq found t)))
		))
	  ;; We found out that we don't have anything above us.
	  (setq found t)
	  (setq area 'none)
	  ))

      ;; Find out where we stopped, and calculate the indentation
      ;; accordingly.
      (cond ((eq area 'none); Nothing above us.
	     0)
	    ((eq area 'class)
	     ;; We found a class.  If the depth is zero, we were
	     ;; somewhere upto (and including) the opening of the
	     ;; first routine in the class.  Otherwise, we're
	     ;; indenting a new class which follows a class with an
	     ;; `end' not in the first column.
	     (if (= depth 0)
		 sather-indent
	       0))
	    ((eq area 'routine)
	     ;; We found a routine.  If the depth is zero, we were at
	     ;; the beginning of some later routine.  Otherwise, we
	     ;; were somewhere inside the routine.
	     (if (= depth 0)
		 (* sather-indent 2)
	       (* sather-indent depth)))
	    ((= depth 1)
	     ;; We found something at our own level above us.
	     (current-indentation))
	    (t
	     ;; This should just mean that (= depth 0), and we found
	     ;; our enclosing block.
	     (+ (current-indentation) sather-indent)))
      )
    ))

;; Do standard line indentation.  First, we check for an enclosing
;; s-expression.  Next, we look for a continued statement.  Otherwise,
;; we just indent according to the block level.
(defun sather-standard-indent ()
  (cond ((sather-in-sexp-p))
	((sather-continued-line-p)
	 (+ sather-continued-line-indent (sather-get-block-indent)))
	(t
	 (sather-get-block-indent))
	))

;; A line is one of the following:
;;    blank
;;    just a comment
;;    a class opener
;;    a pre/post condition
;;    an `end' clause
;;    a block opener
;;    a midblock clause
;;    some general statement

;; Return the appropriate indentation for this line as an integer.
(defun sather-calc-indent ()
  (cond
   ((sather-empty-line-p)
    (sather-standard-indent))
   ((sather-comment-line-p)
    (sather-comment-indent))
   ((sather-starts-with-class-p)
    0)
   ((sather-starts-with-cond-p)
    sather-continued-line-indent)
   ((sather-starts-with-end-p)
    (- (sather-get-block-indent) sather-indent))
   ((sather-starts-with-block-p)
    (sather-get-block-indent))
   ((sather-starts-with-midblock-p)
    (- (sather-get-block-indent) sather-indent))
   (t
    (sather-standard-indent))
   ))

;;; Indentation for a region.  Shows progress status, and is
;;; configurable for the indentation of comments.  Much of this was
;;; stolen from cc-mode (GPL).

;; Hold information for the progress update.
(defvar sather-progress-info nil)

;; Initilize the status line for indenting the region.
(defun sather-progress-init (start end)
  (if sather-progress-info
      ()
    (setq sather-progress-info (vector start
				       (save-excursion
					 (goto-char end)
					 (point-marker))
				       (nth 1 (current-time))))
    (message "indenting region...")))

;; Update the progress indicator.
(defun sather-progress-update ()
  (if (not (and sather-progress-info sather-progress-interval))
      nil
    (let ((now (nth 1 (current-time)))
	  (start (aref sather-progress-info 0))
	  (end (aref sather-progress-info 1))
	  (then (aref sather-progress-info 2)))
      ;; Update the status if the time passed since the last update is
      ;; greater than the desired interval.
      (if (< sather-progress-interval (- now then))
	  (progn
	    (message "indenting region... (%d%% complete)"
		     (/ (* 100 (- (point) start)) (- end start)))
	    (aset sather-progress-info 2 now)))
      )))

;; Clean up the markers and variables, and print the completed
;; message.
(defun sather-progress-done ()
  (set-marker (aref sather-progress-info 1) nil)
  (setq sather-progress-info nil)
  (message "indenting region... done"))

;; Indent the specified region.  This is called from the
;; `indent-region' command, through the `indent-region-function'
;; variable.
(defun sather-indent-region (start end)
  (save-excursion
    (goto-char start)

    ;; Go to the first line that has something.
    (skip-chars-forward " \t\n")
    (beginning-of-line)

    ;; Indent the region ...
    (unwind-protect
	(progn
	  (sather-progress-init start end)
	  (while (not (eobp))
	    ;; Only indent lines with something in them.
	    (if (not (sather-empty-line-p))
		(progn
		  (sather-indent-command)
		  ;; We only try to indent comments if:
		  ;; 1.  we're allowed to indent comments with a region,
		  ;; 2.  this is not a comment-only line (already done), and
		  ;; 3.  there is a comment somewhere on this line.
		  (if (and sather-indent-comments-with-region
			   (not (sather-comment-line-p))
			   (sather-comment-on-line-p))
		      (sather-comment))
		  ))
	    (forward-line 1)
	    (sather-progress-update)))
      (sather-progress-done)) ; Protect form.
    ))

;;; Minor features:

;; Update the modeline to include activated features.
(defun sather-update-modeline ()
  (setq sather-feature-string
	(if sather-hungry-delete-key
	    "/h"
	  nil))
  (if sather-auto-indent
      (setq sather-feature-string
	    (if sather-feature-string
		(concat sather-feature-string "i")
	      "/i")))
  (if sather-auto-newline
      (setq sather-feature-string
	    (if sather-feature-string
		(concat sather-feature-string "n")
	      "/n")))
  (if sather-newline-prefix-indent
      (setq sather-feature-string
	    (if sather-feature-string
		(concat sather-feature-string "p")
	      "/p")))
  (force-mode-line-update))

;; This was mostly taken from cc-mode (GPL).
(defun sather-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on.
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

(defun sather-toggle-indent-state (arg)
  "Toggle auto-indent feature.
Optional numeric ARG turns auto-indent on when positive, off when
negative, and toggles when zero.

When the auto-indent feature is enabled (indicated by an `i' displayed
in the modeline following the `/' feature separator) lines are
automatically indented after a <return> is entered."
  (interactive "P")
  (setq sather-auto-indent (sather-calculate-state arg sather-auto-indent))
  (sather-update-modeline))

(defun sather-toggle-newline-state (arg)
  "Toggle auto-newline feature.
Optional numeric ARG turns auto-newline on when positive, off when
negative, and toggles when zero.

When the auto-newline feature is enabled (indicated by an `n'
displayed in the modeline following the `/' feature separator) some
special characters, like semicolon, will cause newlines to be
automatically inserted."
  (interactive "P")
  (setq sather-auto-newline (sather-calculate-state arg sather-auto-newline))
  (sather-update-modeline))

(defun sather-toggle-hungry-state (arg)
  "Toggle hungry-delete-key feature.
Optional numeric ARG turns hungry-delete on when positive, off when
negative, and toggles when zero.

When the hungry-delete-key feature is enabled (indicated by an `h'
displayed in the modeline following the `/' feature separator) the
delete key deletes any and all preceding whitespace."
  (interactive "P")
  (setq sather-hungry-delete-key
	(sather-calculate-state arg sather-hungry-delete-key))
  (sather-update-modeline))

(defun sather-toggle-prefix-state (arg)
  "Toggle newline-prefix-indent feature.
Optional numeric ARG turns newline-prefix-indent on when positive, off
when negative, and toggles when zero.

When the newline-prefix-indent feature is enabled (indicated by a `p'
displayed in the modeline following the `/' feature separator) the
<return> key indents the current line before adding the newline."
  (interactive "P")
  (setq sather-newline-prefix-indent
	(sather-calculate-state arg sather-newline-prefix-indent))
  (sather-update-modeline))

;;; Handling Comments:

;; The following two functions (with slight modifications) were taken
;; from eiffel3.el 1.73, distributed by Tower Technology Corporation,
;; freely available under the GNU General Public License (GPL).  It
;; seems to work very well.

;; Prefix that starts a comment that begins a line.  Empty comments
;; and comments that are not the only thing on a line return nil as
;; their prefix.

(defvar sather-comment-fill-across-blanks nil
  "*If non-nil, blank comments are absorbed into paragraph filling.")

(defvar sather-comment-fill-break-regexp nil
  "*If non-nil, use the regexp to break comment fills.")

(defun sather-comment-prefix ()
  (save-excursion
    (end-of-line)
    (let ((limit (point)))
      (beginning-of-line)
      (cond ((and (not sather-comment-fill-across-blanks)
		  (re-search-forward "^\\s-*--|?\\s-*$" limit t))
	     nil)
	    ((and sather-comment-fill-break-regexp
		  (re-search-forward sather-comment-fill-break-regexp
				     limit t))
	     nil)
	    ((re-search-forward "^\\s-*--|?\\s-*" limit t)
	     (buffer-substring (match-beginning 0) (match-end 0)))
	    (t nil))
      )
    ))

(defun sather-fill-paragraph ()
  "Textually fills Sather comments ala fill-paragraph."
  (interactive)
  (save-excursion
    (let ((current-point (point))
	  (last-point nil)
	  (para-begin nil)
	  (para-end   nil)
	  (fill-prefix (sather-comment-prefix)))
      (if fill-prefix
	  (progn
	    (setq last-point (point))
	    (forward-line -1)
	    (end-of-line)
	    (while (and (not (= (point) last-point))
			(sather-comment-prefix))
	      (setq last-point (point))
	      (forward-line -1)
	      (end-of-line))
	    (if (= (point) last-point)
		(setq para-begin (save-excursion (beginning-of-line) (point)))
	      (setq para-begin (1+ (point))))
	    (goto-char current-point)
	    (setq last-point (point))
	    (forward-line 1)
	    (end-of-line)
	    (while (and (not (= (point) last-point))
			(sather-comment-prefix))
	      (setq last-point (point))
	      (forward-line 1)
	      (end-of-line))
	    (if (= (point) last-point)
		(setq para-end (point))
	      (beginning-of-line)
	      (setq para-end (point)))
	    (fill-region para-begin para-end))
        ))
    ))

;; Point to beginning of comment on line.  Assumes line contains a
;; comment.
(defun sather-goto-comment-beg ()
  (beginning-of-line)
  (search-forward "--" nil t)
  (backward-char 2))

;; Return indentation for a comment line.
(defun sather-comment-indent ()
  (save-excursion
    (let ((p0 (point)))
      (beginning-of-line)
      (if (or (and sather-keep-left-comments
		   (looking-at "^--"))
	      (/= (forward-line -1) 0))
	  0
	;; If the prevous line is a comment, use the same
	;; indentation for the new comment.
	(if (sather-comment-line-p)
	    (sather-current-indentation)

	  ;; Else, we indent according to the block level.
	  (goto-char p0)
	  (let ((in (sather-get-block-indent)))
	    (cond
	     ((and sather-indent-design-comments-correctly
		   (sather-in-design-region-p))
	      (+ in sather-indent))
	     ((and (= (forward-line -1) 0)
		   (sather-empty-line-p)
		   (= in 0))
	      0)			;early comments start to the left
	     (t				;otherwise indent once
	      in))
	    ))
	))
    ))

(defun sather-comment ()
  "Edit a comment on the line. If one exists, reindents it and moves
to it, otherwise creates one. Gets rid of trailing blanks, puts one
space between comment header comment text, leaves point at front of
comment.  If comment is alone on a line it reindents relative to
surrounding text.  If it is before any code, it is put at the line
beginning.  Uses the variable sather-comment-col to set goal start on
lines after text."
  (interactive)
  (cond ((sather-comment-line-p)	;just a comment on the line
         (beginning-of-line)
         (delete-horizontal-space)
         (indent-to (sather-comment-indent))
         (forward-char 2)(delete-horizontal-space)(insert " "))
        ((sather-comment-on-line-p)	;comment already at end of line
         (cond ((sather-ends-with-end-p) ;end comments come immediately
                (sather-goto-comment-beg)(delete-horizontal-space)(insert " ")
                (forward-char 2)(delete-horizontal-space)(insert " "))
               (t
                (sather-goto-comment-beg)(delete-horizontal-space)
                (if (< (current-column) sather-comment-col)
                    (indent-to sather-comment-col)
                  (insert " "))
                (forward-char 2)
		(delete-horizontal-space)
		(insert " "))))
        ((sather-empty-line-p)		;put just a comment on line
         (beginning-of-line)
         (delete-horizontal-space)
         (indent-to (sather-comment-indent))
         (insert "-- "))
        ((sather-ends-with-end-p)	;end comments come immediately
         (end-of-line)(delete-horizontal-space)(insert " -- "))
        (t                              ;put comment at end of line
         (end-of-line)
         (delete-horizontal-space)
         (if (< (current-column) sather-comment-col)
             (indent-to sather-comment-col)
           (insert " "))
         (insert "-- "))
	)				; cond
  )

;;; Lots of Predicates:

;; True if there is a class definition before this one.
(defun sather-prev-class-p ()
  (save-excursion
    (re-search-backward sather-class-regexp nil t)))

;; True if line starts with some class.
(defun sather-starts-with-class-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at sather-class-regexp)))

;; True if line starts with the `end' keyword.
(defun sather-starts-with-end-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*end;?\\($\\|\\s-\\)")
    ))

;; True if line starts with a block keyword.
(defun sather-starts-with-block-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at sather-block-regexp)
    ))

;; True if line starts with a midblock keyword.
(defun sather-starts-with-midblock-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at sather-midblock-regexp)
    ))

;; True if the current `is' opens a routine.  We don't want to search
;; all the way back to an opening class, so we just search for the
;; previous block.  If we don't find a class in between here and
;; there, this must be a routine.
(defun sather-routine-is-p ()
  (save-excursion
    (let ((p0 (point))
	  (me-is)
	  (found))
      (while (and (not found)
		  (re-search-backward sather-block-regexp nil t))
	(setq me-is (match-end sather-routine-regexp-is-match))
	(setq found (and (not (sather-in-comment-p me-is))
			 (not (sather-in-quoted-string-p me-is)))))
      (and found
	   (not (re-search-forward sather-class-regexp p0 t))))
    ))

;; True if line starts with either pre or post.
(defun sather-starts-with-cond-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*\\(pre\\|post\\)[ \t\n]")
    ))

;; `nil' if we're not in an sexp; otherwise, return the location of
;; the inner-most open.
(defun sather-in-sexp-p ()
  (save-excursion
    (let ((p0 (point))
	  (ccb)
	  (state)
	  (last-open))

      ;; Find an enclosing block, or the first (previous) semicolon.
      (let ((block-statement (concat "\\(;\\)\\|" sather-block-regexp))
	    (mb)
	    (found nil))
	(while (not found)
	  (if (re-search-backward block-statement nil t)
	      (progn
		(setq mb (match-beginning 0))
		(goto-char mb)
		(if (and (not (sather-in-quoted-string-p mb))
			 (not (sather-in-comment-p mb)))
		    (progn
		      (setq ccb mb)
		      (setq found t)))
		)
	    (setq found t))
	  ))

      ;; Find the previous opening sexp, bounded by `ccb'.  This has
      ;; icky form.
      (if (not ccb)
	  nil
	(setq state (parse-partial-sexp ccb p0 nil nil nil nil))
	(setq last-open (car (cdr state)))
	(if last-open
	    (progn
	      (goto-char last-open)

	      ;; Indent to one greater than the position of the
	      ;; opening sexp character.
	      (1+ (+ (current-indentation)
		     (- (point) (progn (beginning-of-line)
				       (skip-chars-forward " \t")
				       (point)))))
	      )
	  nil))
      )
    ))

;; True if previous line does not end a statement.
(defun sather-continued-line-p ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    ;; Go to the previous non-blank and non-comment line.
    (while (and (or (looking-at "\\s-*--")
		    (looking-at "\\s-*$"))
		(not (= (point) (point-min))))
      (forward-line -1))

    ;; If we're at the top of the file, we're not a continued line.
    (if (= (point) (point-min))
	nil
      (progn
	;; If the previous expression is not a block opener, or a
	;; complete statement, return that the expression is
	;; continuing.
	(let ((bo (looking-at ".*\\(^\\|\\s-\\)\\(do\\(@[A-Za-z_][A-Za-z0-9_]+!\\)?\\|fork\\|is\\|then\\|lock\\|loop\\|par\\(loop\\)?\\|protect\\|else\\|elsif\\|end\\)\\($\\|\\s-\\)"))
	      (mb-bo (match-beginning 2))
	      (se (looking-at ".*\\(;\\)"))
	      (mb-se (match-beginning 1)))
	  (if (or (and bo
		       (not (sather-in-comment-p mb-bo))
		       (not (sather-in-quoted-string-p mb-bo)))
		  (and se
		       (not (sather-in-comment-p mb-se))
		       (not (sather-in-quoted-string-p mb-se))))
	      nil
	    t))
	))
    ))

;; True if line ends with 'end' or 'end;' and a comment.
(defun sather-ends-with-end-p ()
  (save-excursion
    (beginning-of-line)
    (and (looking-at "^\\(.*\\s-+\\)?\\(end\\);?\\($\\|\\s-\\)")
	 (not (sather-in-comment-p (match-beginning 2))))
    ))

;; True if current line is empty.
(defun sather-empty-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\s-*$")
    ))

;; True if current line is just a comment.
(defun sather-comment-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*--")
    ))

;; True if current line contains a comment.
(defun sather-comment-on-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[^\n]*--")
    ))

;; True if point is in a comment.
(defun sather-in-comment-p (&optional arg)
  (save-excursion
    (if arg (goto-char arg))
    (and (/= (point) (point-max)) (or (looking-at "$") (forward-char 1)))
    (search-backward "--" (save-excursion (beginning-of-line)
					  (point)) t)
    ))

;; True if current line ends with the keyword 'is' and an optional
;; comment.
(defun sather-ends-with-is-p ()
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (and (re-search-forward "\\(^\\|\\s-\\)is\\s-*\\($\\|--\\)" end t)
	   (not (sather-in-comment-p))))
    ))

;; True if point is in a quoted string.
(defun sather-in-quoted-string-p (&optional arg)
  (save-excursion
    (if arg (goto-char arg))
    (let ((p0 (point)))
      (beginning-of-line)
      ;; If there are an odd number of quotes between point and the
      ;; beginning of the line, we're in a quoted string.
      (let ((odd nil))
	(while (search-forward "\"" p0 t)
	  (setq odd (null odd)))
	odd)
      )
    ))

;; True if point is in a class or routine design region (after the
;; name, and before the `is').
(defun sather-in-design-region-p ()
  (save-excursion
    (let ((p0 (point))
	  (b-start)
	  (b-end)
	  (me-name)
	  (mb-is)
	  (found))

      ;; Find the ending boundry for the search.  It's the end of the
      ;; next `is'.
      (while (and (not b-end)
		  (re-search-forward
		   "\\(^\\|\\s-\\)\\(is\\)\\($\\|\\s-\\|\\)" nil t))
	(setq mb-is (match-beginning 2))
	(setq b-end (match-end 2))
	(if (or (sather-in-comment-p b-end)
		(sather-in-quoted-string-p b-end))
	    (setq b-end nil)))

      ;; Find the starting boundry for the search.  It's the end of
      ;; the previous block expression.
      (goto-char p0)
      (while (and (not b-start)
		  (re-search-backward sather-block-regexp nil t))
	(setq b-start (match-end sather-block-regexp-name-match))
	(if (or (sather-in-comment-p b-start)
		(sather-in-quoted-string-p b-start))
	    (setq b-start nil)))

      (if (not b-end)
	  nil
	(goto-char mb-is)
	(if (sather-routine-is-p)
	    (progn
	      (goto-char b-start)
	      (while (and (not found)
			  (re-search-forward sather-routine-regexp b-end t))
		(setq me-name (match-end sather-routine-regexp-name-match))
		(setq mb-is (match-beginning sather-routine-regexp-is-match))
		(setq found (and (> p0 me-name)
				 (< p0 mb-is)))
		)
	      found)

	  ;; Not a routine, so it's a class.  If `b-start' is `nil',
	  ;; there are no blocks before point.  If we can search
	  ;; backwards and find a class, we're in a design region.
	  (if (not b-start)
	      (if (and (re-search-backward sather-class-regexp nil t)
		       (> p0 (match-end sather-class-regexp-name-match))
		       (< p0 mb-is))
		  t
		nil)
	    ;; Go to the previous block, and search forward.  If point
	    ;; is inside the class match, we're in a design region.
	    (goto-char b-start)
	    (if (and (re-search-forward sather-class-regexp nil t)
		     (> p0 (match-end sather-class-regexp-name-match))
		     (< p0 mb-is))
		t
	      nil)
	    )
	  ))
      )
    ))

;; Moves point to previous line excluding blank lines.  Returns t if
;; successful, nil if not.
(defun sather-move-to-prev-non-blank ()
  (beginning-of-line)
  (re-search-backward "^\\s-*[^ \t\n]" nil t))

;; Returns current line indentation.
(defun sather-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-indentation)
    ))

(defun sather-which-class ()
  "Return the name of the class in which point resides."
  (interactive)
  (save-excursion
    (let ((current-point (point)))
      (if (re-search-backward sather-class-regexp nil t)
	  (let ((class-point (point))
		(mb1 (match-beginning sather-class-regexp-mod-match))
		(me1 (match-end sather-class-regexp-mod-match))
		(mb5 (match-beginning sather-class-regexp-name-match))
		(me5 (match-end sather-class-regexp-name-match)))
	    (if (and (re-search-forward "^end" nil t)
		     (<= current-point (point))
		     (>= current-point class-point))
		(message "Point currently within %s `%s'."
			 (buffer-substring mb1 me1)
			 (buffer-substring mb5 me5))
	      (message "Previous %s is `%s'."
		       (buffer-substring mb1 me1)
		       (buffer-substring mb5 me5))
	      ))
	(message "Point not within class boundries.")
	))
    ))

;;; Support for class/routine hiding (outlining with selective
;;; display):

;; Class hiding works on classes that begin and end in the first
;; column.  Routine hiding should work for routines of `any' format.
;; I've tried it on the Sather library code, and it seems to work okay
;; with most of it, if somewhat slowly.

;; Hide all lines in the region.
(defun sather-hide-region (start end)
  (subst-char-in-region start end ?\n ?\r t))

;; Show (unhide) all lines in the region.
(defun sather-show-region (start end)
  (subst-char-in-region start end ?\r ?\n t))

;; Search for the next class-terminating end.
(defun sather-next-class-end ()
  (re-search-forward sather-class-end-regexp nil t))

;; v2: This should now work (all the time?).  It should also be a bit
;; faster.

;; Find the "end" statement that matches the current level open
;; expression.  The `end' parameter gives the buffer position ending
;; the open expression.
(defun sather-find-matching-end (end)
  (save-excursion
    (goto-char end)
    (let ((depth 1))
      (while (> depth 0)
        (let ((mb) (me))
          (if (re-search-forward sather-block-regexp nil t)
              (progn
		;; sather-in-comment-p kills these, so save them.
                (setq mb (match-beginning sather-block-regexp-name-match))
                (setq me (match-end 2))
                (if (and (not (sather-in-comment-p mb))
			 (not (sather-in-quoted-string-p mb)))
                    (progn
                      (goto-char mb)
                      (if (looking-at "end")
                          (setq depth (1- depth))
			(setq depth (1+ depth)))
                      ))
                ))
          (goto-char me)
          ))
      )
    (point)
    ))

(defun sather-hide-classes ()
  "Hide all the class definitions in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward sather-class-regexp nil t)
      (message "Hiding class... `%s'"
	       (buffer-substring
		(match-beginning sather-class-regexp-name-match)
		(match-end sather-class-regexp-name-match)))
      (sather-hide-region (match-beginning 0)
			  (progn (sather-next-class-end)
				 (match-end 0)))
      (end-of-line)
      (if (not (eobp))
	  (forward-char 1))
      )
    (message "Hiding class... done")
    ))

(defun sather-hide-current-class ()
  "Hide the current class definition."
  (interactive)
  (save-excursion
    (if (not (looking-at sather-class-regexp))
	(re-search-backward sather-class-regexp nil t))
    (sather-hide-region (point) (progn (sather-next-class-end) (point)))
    )
  (beginning-of-line))

(defun sather-show-classes ()
  "Show (unhide) all the class definitions in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
		(re-search-forward sather-class-regexp nil t))
      (message "Showing class... `%s'"
	       (buffer-substring
		(match-beginning sather-class-regexp-name-match)
		(match-end sather-class-regexp-name-match)))
      (sather-show-region (point) (progn (end-of-line) (point)))
      (if (not (eobp))
	  (forward-line 1))
      )
    (message "Showing class... done")
    ))

(defun sather-show-current-class ()
  "Show (unhide) the current class definition."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (sather-show-region (point) (progn (end-of-line) (point)))
    ))

(defun sather-hide-routines ()
  "Hide the routine definitions in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
		(re-search-forward sather-routine-regexp nil t))
      (message "Hiding routine... `%s'"
	       (buffer-substring
		(match-beginning sather-routine-regexp-name-match)
		(match-end sather-routine-regexp-name-match)))
      (goto-char (match-beginning sather-routine-regexp-is-match))
      (sather-hide-region (point)
			  (sather-find-matching-end
			   (match-end sather-routine-regexp-is-match)))
      (end-of-line)
      (if (not (eobp))
	  (forward-char 1)))
    (message "Hiding routine... done"))
  (beginning-of-line))

(defun sather-hide-current-routine ()
  "Hide the current routine definition."
  (interactive)
  (let ((b-o-l))
    (save-excursion
      (let ((start-point (point)))
	(beginning-of-line)
	(if (looking-at sather-routine-regexp)
	    (progn
	      (sather-hide-region
	       (match-beginning sather-routine-regexp-is-match)
	       (sather-find-matching-end
		(match-end sather-routine-regexp-is-match)))
	      (setq b-o-l t))
	  (if (re-search-backward sather-routine-regexp nil t)
	      (let* ((mb (match-beginning 0))
		     (me (match-end sather-routine-regexp-is-match))
		     (end (sather-find-matching-end me)))
		(if (and
		     (>= start-point mb)
		     (<= start-point end))
		    (progn
		      (sather-hide-region mb end)
		      (setq b-o-l t))
		  ))
	    ))
	))				; save + let
    (if b-o-l
	(beginning-of-line))
    ))

(defun sather-hide-current-class-routines ()
  "Hide the routines of the current class."
  (interactive)
  (let ((valid))
    (save-excursion
      (let* ((save-point (point))
	     (cs (re-search-backward sather-class-regexp nil t))
	     (ce (progn
		   (sather-next-class-end)
		   (end-of-line)
		   (if (not (eobp))
		       (forward-char 1))
		   (point))))
	(if (and (>= save-point cs) (< save-point ce))
	    (progn
	      (narrow-to-region cs ce)
	      (setq valid t))
	  (progn
	    (message "Not within class boundries.")
	    (setq valid nil)))
	))				; save- + let
    (if valid
	(progn
	  (sather-hide-routines)
	  (widen)))
    ))

(defun sather-show-routines ()
  "Show (unhide) all the routine definitions in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (re-search-forward sather-routine-regexp nil t)
      (message "Showing routine... `%s'"
	       (buffer-substring
		(match-beginning sather-routine-regexp-name-match)
		(match-end sather-routine-regexp-name-match)))
      (beginning-of-line)
      (sather-show-region (match-beginning sather-routine-regexp-is-match)
			  (progn (end-of-line) (point)))
      (if (not (eobp))
	  (forward-char 1)))
    (message "Showing routine... done")
    ))

(defun sather-show-current-routine ()
  "Show (unhide) the current routine definition."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at sather-routine-regexp)
	(sather-show-region (match-beginning sather-routine-regexp-is-match)
			    (progn
			      (goto-char
			       (match-end sather-routine-regexp-is-match))
			      (end-of-line)
			      (point)))
      )
    ))

(defun sather-show-current-class-routines ()
  "Show (unhide) the routines of the current class."
  (interactive)
  (save-excursion
    (re-search-backward sather-class-regexp nil t)
    (narrow-to-region (point) (progn (sather-next-class-end) (point)))
    (sather-show-routines)
    (widen)
    ))

(defun sather-show-all ()
  "Show (unhide) everything.
Use this in case some of the specialized functions don't work.  Also,
it's a lot faster than some of the specialized functions."
  (interactive)
  (save-excursion
    (widen)
    (sather-show-region (point-min) (point-max))
    ))

(defun sather-mouse-hide-something (event)
  "Look at the context of the mouse click, and hide whatever is \"there\"."
  (interactive "e")
  (save-excursion

    ;; Of course, they both do this in different ways.
    (if sather-xemacs-p
	(goto-char (event-point event))
      (goto-char (posn-point (event-start event))))
    (beginning-of-line)

    (cond ((looking-at ".*\r")
	   (sather-show-region (point) (progn (end-of-line) (point))))
	  ((looking-at sather-class-regexp)
	   (sather-hide-current-class))
	  (t
	   (sather-hide-current-routine)))
    ))

;;; Hiding/Showing comments:

(defun sather-hide-comments ()
  "Hide all the comments in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((p-start) (pv "/-\\|") (i 0))
      (while (re-search-forward "\\s-*--" nil t)
	(goto-char (match-beginning 0))
	(setq p-start (save-excursion (end-of-line 0) (point)))
	(while (and (looking-at "\\s-*\\(--\\|$\\)")
		    (= (forward-line) 0)))
	(sather-hide-region p-start (1- (point)))
	(end-of-line)
	(message "hiding comments... %c" (aref pv (% i 4)))
	(setq i (1+ i))
	))
    (message "hiding comments... done")
    ))

(defun sather-show-comments ()
  "Show all the comments in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((p-start) (pv "/-\\|") (i 0))
      (while (re-search-forward "\r\\s-*--" nil t)
	(sather-show-region (match-beginning 0) (progn (end-of-line) (point)))
	(message "showing comments... %c" (aref pv (% i 4)))
	(setq i (1+ i))
	))
    (message "showing comments... done")
    ))

;;; Movement:

(defun sather-forward-class ()
  "Go to the next class."
  (interactive)
  (if (not (re-search-forward sather-class-regexp nil t))
      (message "No next class."))
  )

(defun sather-backward-class ()
  "Go to the previous/containing class."
  (interactive)
  (if (not (re-search-backward sather-class-regexp nil t))
      (message "No previous class."))
  )

(defun sather-forward-routine ()
  "Go to the next routine."
  (interactive)
  (if (not (re-search-forward sather-routine-regexp nil t))
      (message "No next routine."))
  )

(defun sather-backward-routine ()
  "Go to the previous/containing routine."
  (interactive)
  (if (not (re-search-backward sather-routine-regexp nil t))
      (message "No previous routine."))
  )

;;; Sexps for Sather:

;; This is easy because we already have `sather-find-matching-end'.  It
;; would require more work to go the other way.
(defun sather-forward-block ()
  "Go to the matching end of the current block opener."
  (interactive)
  (cond ((looking-at sather-class-regexp)
	 (re-search-forward sather-class-end-regexp nil t))
	((looking-at sather-routine-regexp)
	 (goto-char (sather-find-matching-end
		     (match-end sather-routine-regexp-is-match))))
	((looking-at sather-block-regexp)
	 (if (looking-at "\\s-+end")
	     (message "No opening expression found.")
	   (goto-char (sather-find-matching-end
		       (match-end sather-block-regexp-name-match)))))
	(t
	 (message "No opening expression found."))
	)
  (point)
  )

(defun sather-indent-block ()
  "Indent the block that begins the line containing point."
  (interactive)
  (save-excursion
    (indent-region (progn (beginning-of-line) (point))
		   (sather-forward-block)
		   nil)
    ))

;;; Processes:

(defun sather-run-program (command)
  "Run the working Sather program."
  (interactive "sRun program: ")
  (shell-command (concat command "&")))

(defun sather-run-program-now ()
  "Run the working Sather program."
  (interactive)
  (shell-command (concat sather-program-name "&")))

(defun sather-browse (command)
  "Run the browser with the current sather-browse-command."
  (interactive "sBrowse command: ")
  (shell-command (concat command "&")))

(defun sather-browse-now ()
  "Run the browser with the current sather-browse-command."
  (interactive)
  (shell-command (concat sather-browse-command "&")))

;;; Interacting with the Sather Manual.

(defun sather-topic-lookup ()
  "Search for a topic using the Sather Specification info manual.

This will take over any info session you have going.  You may want to
rename any current info buffers using \\[rename-buffer]."
  (interactive)
  (save-excursion
    (let ((topic))
      (if (not (looking-at "\\<\\sw+"))
	  (backward-sexp))
      (if (looking-at "\\<\\sw+")
	  (setq topic (buffer-substring
		       (point)
		       (progn (forward-sexp) (point)))))
      (setq topic (read-string "Topic: " topic))
      (info sather-manual-name)
      (Info-index topic))
    ))

(defun sather-browse-manual ()
  "Browse the Sather Manual."
  (interactive)
  (info sather-manual-name))

(defun sather-browse-tutorial ()
  "Browse the Sather Eclectic Tutorial."
  (interactive)
  (info sather-tutorial-name))

(defun sather-describe-mode ()
  "Describe sather-mode by running \\[describe-function] 'sather-mode."
  (interactive)
  (describe-function 'sather-mode))

;;; Submitting bug reports (mostly stolen from cc-mode (GPL)):

(defconst sather-version "1.71"
  "The sather-mode version number.")

(defconst sather-compiler-help-address "bug-sather@gnu.org"
  "Address accepting submission of Sather Compiler bug reports.")

(defconst sather-mode-help-address "lewikk@grasshopper.aud.alcatel.com"
  "Address accepting submission of sather-mode bug reports.")

(defun sather-version ()
  "Echo the current version of sather-mode in the minibuffer."
  (interactive)
  (message "Using sather-mode v%s" sather-version))

(defun sather-compiler-submit-bug-report ()
  "Submit via mail a bug report on the Sather compiler."
  (interactive)
  ;; Load in reporter.
  (let ((reporter-prompt-for-summary-p t))
    (and
     (y-or-n-p "Do you want to submit a report on the Sather compiler? ")
     (require 'reporter)
     (reporter-submit-bug-report
      sather-compiler-help-address
      "Compiler Report"
      nil
      nil
      nil
      "Dear Boris:\n\n\
This report is for Sather Compiler version: \n\n\
This is a description of the problem:\n\n\
The following code segment demonstrates the problem:\n\n"
      )
     )					; and
    ))

(defun sather-mode-submit-bug-report ()
  "Submit via mail a bug report on sather-mode."
  (interactive)
  ;; Load in reporter.
  (let ((reporter-prompt-for-summary-p t))
    (and
     (y-or-n-p "Do you want to submit a report on sather-mode? ")
     (require 'reporter)
     (reporter-submit-bug-report
      sather-mode-help-address
      (concat "Report on sather-mode v" sather-version)
      ;; Variables that affect indentation (mostly).
      (list 'sather-indent
	    'sather-continued-line-indent
	    'sather-keep-left-comments
	    'sather-indent-comments-with-region
	    'sather-indent-design-comments-correctly
	    'sather-progress-interval
	    'sather-auto-indent
	    'sather-auto-newline
	    'sather-hungry-delete-key
	    'sather-delete-function
	    'sather-newline-prefix-indent
	    'sather-comment-col
	    'sather-compile-command
	    'sather-browse-command
	    'sather-program-name
	    'sather-manual-name
	    'sather-tutorial-name
	    'tab-width
	    'sather-highlight-type)
      nil
      nil
      "Dear Kevin, nice try, but: "
      )
     )					; and
    ))

;;; Give it up:
(provide 'sather)
