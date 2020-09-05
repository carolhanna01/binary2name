;;; -*-emacs-lisp-*-

(setq load-path 
      (cons 
       (concat "/usr/share/" (symbol-name debian-emacs-flavor)
	       "/site-lisp/sather-elisp")
       load-path))

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
