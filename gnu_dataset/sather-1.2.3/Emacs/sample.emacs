;;; -*-emacs-lisp-*-
;;; ---------------------->  Emacs Lisp - sourcefile  <----------------------
;;; Copyright (C) 199x by International Computer Science Institute
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

;;; Version:         1.7
;;; Last Modified:   1996/05/28 11:25:00

;;; This file contains some convenient forms you may wish to put in
;;; one of your Emacs startup files.  On the other hand, you can just
;;; uncomment the things you want and load the whole file from your
;;; `.emacs' file, like this (do _not_ uncomment these lines!):
;;;
;;; (load "sample.emacs")
;;;
;;; Or you may prefer to:
;;;
;;; (load (expand-file-name
;;;        (concat (getenv "SATHER_HOME") "/Emacs/sample.emacs")))

;;; Complain if SATHER_HOME is not defined.  This must be set if you
;;; want Emacs to use files from the Sather distribution/installation
;;; home directory.
(if (getenv "SATHER_HOME")
    ()
  (error "Environment variable SATHER_HOME is not defined!"))

;;; Look for the ELisp files included in the Sather distribution.
(setq load-path
      (append
       (list (expand-file-name (concat (getenv "SATHER_HOME") "/Emacs")))
       load-path))

;;; Look forthe Info files included in the Sather distribution.
(setq Info-directory-list
      (append
       (list (expand-file-name (concat (getenv "SATHER_HOME") "/Emacs")))
       Info-default-directory-list))

;;; This is how Emacs knows how to load Sather source files.
(autoload 'sather-mode "sather" "Sather Mode" t)
(setq auto-mode-alist
      (append '(("\\.sa$" . sather-mode)
                ("\\.psa$" . sather-mode)
                ) auto-mode-alist))

(autoload 'sather-module-mode "sather-module" "Sather Module Mode" t)
(setq auto-mode-alist
      (append '(("\\.module$" . sather-module-mode)
                ("\\.pmodule$" . sather-module-mode)
                ) auto-mode-alist))

;;; The next three code segements set up different packages that
;;; support syntax highlighting for program source code.  sather-mode
;;; supports all three.  Which one to choose is up to you; however, I
;;; would strongly suggest that you use hl319 if you are using GNU
;;; Emacs 19, and font-lock if you are using XEmacs 19.

;; Set the following to setup highlighting support using that package
;; and background intensity value.
(defvar sather-use-highlight-package 'hl319
  "The package to use for highlighting.  One of: hl319, hilit19, font-lock.
Checked in `sample.emacs' for easy configuration.")

(defvar sather-highlight-background-mode 'light
  "Passed to the highlight package defined in `sather-use-highlight-package'.
`light' means you use a light colored background in Emacs (eg, white,
light grey, etc).  `dark' means you use a dark background (eg, black,
dark blue, etc..")

(cond
 ;; This sets up the hl319 package for syntax highlighting.
 ((eq sather-use-highlight-package 'hl319)
  (cond (window-system
         (add-hook 'find-file-hooks    'hilit-install-line-hooks)
         (setq hilit-mode-enable-list  '(not text-mode)
               hilit-background-mode   sather-highlight-background-mode)
         (setq hilit-user-face-table
               ;; face      light                    dark             mono
               '((comment   firebrick4-italic        pink-italic      nil)
                 (defun     blue-bold                cyan-bold        italic)
                 (constant  blue                     cyan             nil)
                 (private   olive-italic             bold-underline   nil)
                 (attr      orangered-bold-underline bold-italic      nil)
                 (shared    tomatored-bold-underline italic-underline nil)
                 (include   deeppink2-bold-underline palegreen-italic nil)
                 (string    purple                   sandybrown       nil)
                 (sugar     bold                     white            nil)
                 (iterator  darkorchid-bold          cyan             bold)
                 (keyword   navy                     white            bold)
                 (builtin   navy                     wheat-bold       bold)
                 (type      navy                     wheat-bold       bold)
                 (exception purple                   nil              nil)
                 ))
         (require 'hl319)
         ))
  )

 ;; This sets up the hilit19 package for syntax highlighting.
 ((eq sather-use-highlight-package 'hilit19)
  (cond (window-system
         (setq hilit-mode-enable-list  '(not text-mode)
               hilit-background-mode   sather-highlight-background-mode
               hilit-inhibit-hooks     nil
               hilit-inhibit-rebinding nil)
         (require 'hilit19)
         (hilit-translate sugar    'Plum1
                          builtin  'white-bold
                          iterator 'green-bold)
         ))
  )

  ;; This sets up the font-lock package for syntax highlighting.
 ((eq sather-use-highlight-package 'font-lock)
  (cond (window-system
         (setq-default buffers-menu-max-size nil) ; Show all buffers.

         (if (> emacs-minor-version 28)
             (add-hook 'find-file-hooks 'turn-on-font-lock))

         (setq font-lock-maximum-decoration t)
         (setq font-lock-display-type 'color)
         (setq font-lock-background-mode sather-highlight-background-mode)

         ;; Faces for a light background.
         (cond
          ((eq font-lock-background-mode 'light)
           (setq font-lock-face-attributes
                 '((font-lock-comment-face "firebrick4" nil nil t nil)
                   (font-lock-string-face "purple" nil nil nil nil)
                   (font-lock-keyword-face "navy" nil nil nil nil)
                   (font-lock-type-face "navy" nil nil nil nil)
                   (font-lock-function-name-face "blue" nil t nil nil)
                   (font-lock-reference-face "blue2"  nil nil nil nil)

                   ;; Extras for Sather.
                   (font-lock-constant-face "black" nil nil nil nil)
                   (font-lock-exception-face "purple" nil nil nil nil)
                   (font-lock-iterator-face "darkorchid" nil t nil nil)
                   (font-lock-private-face "olive" nil nil nil nil)
                   (font-lock-attr-face "orangered" nil t nil nil)
                   (font-lock-shared-face "brown" nil t nil nil)
                   (font-lock-include-face "deeppink2" nil nil nil t)
                   (font-lock-sugar-face nil nil t nil nil)
                   (font-lock-builtin-face "navy" nil nil nil nil)
                   (font-lock-class-face "red" nil t nil t)
                   ))
           )

          ;; Faces for a dark background.
          ((eq font-lock-background-mode 'dark)
           (setq font-lock-face-attributes
                 '((font-lock-comment-face "White" nil nil t nil)
                   (font-lock-string-face "Orange" nil nil nil nil)
                   (font-lock-keyword-face "White" nil nil nil nil)
                   (font-lock-type-face nil nil t t nil)
                   (font-lock-function-name-face "Cyan" nil t nil nil)
                   (font-lock-reference-face "Plum1" nil nil nil nil)

                   ;; Extras for Sather.
                   (font-lock-constant-face "Cyan" nil nil nil nil)
                   (font-lock-exception-face "Orange" nil nil nil nil)
                   (font-lock-iterator-face "Green" nil t nil nil)
                   (font-lock-private-face nil nil t nil t)
                   (font-lock-attr-face nil nil t t nil)
                   (font-lock-shared-face nil nil nil t t)
                   (font-lock-include-face "PaleGreen" nil nil t nil)
                   (font-lock-sugar-face "Plum1" nil nil nil nil)
                   (font-lock-builtin-face nil nil t t nil)
                   (font-lock-class-face "White" nil t nil nil)
                   ))
           )
          )
         ;; This turns off highlighting for that face.
         (setq font-lock-variable-name-face nil)
         (require 'font-lock)
         ))
  )
 )

;;; This is what Emacs does when it loads a Sather file.
(setq sather-mode-hook
      '(lambda ()
         ;; Set the standard indentation width to be three spaces (default).
         ;(setq sather-indent 3)

         ;; Tell sather-mode to indent design comments correctly.
         ;(setq sather-indent-design-comments-correctly t)

         ;; Turn on auto-indentation.
         (sather-toggle-indent-state 1)

         ;; Turn off auto-newlines (defualt).
         ;(sather-toggle-newline-state -1)

         ;; Turn on the ``hungry'' delete key.
         ;(sather-toggle-hungry-state 1)

         ;; Turn on the indent-prefix state (default).
         ;(sather-toggle-prefix-state 1)

	 ;; Don't fill comments that start with more than one
	 ;; whitespace character.
	 (setq sather-comment-fill-break-regexp "^\\s-*--|?\\s-\\s-")

         ;; Use font-lock for Sather buffers.
         ;(font-lock-mode 1)

         ;; Use the Sather Emacs Library functions.
         ;; Use the `backspace' key for deleting left.
         ;(local-set-key [backspace] 'sather-electric-delete)

         ;; Move forward a Sather block.
         (local-set-key [M-S-right] 'sather-forward-sexp)

         ;; Use a different function for deleting.
         ;(setq sather-delete-function 'backward-delete-char)

         ;; Use a good, standard tab width of 8.
         (setq tab-width 8)

         ;; Make Sather words include `_', `!', `#', and `$'.
         (modify-syntax-entry ?_ "w")
         (modify-syntax-entry ?! "w")
         (modify-syntax-entry ?# "w")
         (modify-syntax-entry ?$ "w")
         ))

(setq server-switch-hook
      '(lambda ()
         (if (string-match "\\.sa$" (buffer-file-name))
             (sather-server-buffer-undone (current-buffer)))
         ))
