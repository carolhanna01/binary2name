;;; test-hbuts.el --- Test all Hyperbole buttons following point for any errors
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     15-Feb-20 at 11:20:10
;;
;; Copyright (C) 2020  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  Use this to automate testing of all of the Hyperbole explicit and
;;  implicit buttons in the current buffer.
;;
;;  {M-x test-hbuts-and-display-from-point RET} will test all buttons
;;  from the current position until the end of the buffer, pausing
;;  briefly for `test-hbuts-delay-time' to show the button activated
;;  and its activation display result.
;;
;;  {M-x test-hbuts-from-point RET} does the same thing but doesn't
;;  pause at each button, so it is much faster.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hbut)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar test-hbuts-delay-time 0.10
  "Delay time to perform redisplay at the start and end of a button test activation.")

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun test-hbuts-and-display-from-point ()
  "For each Hyperbole button found after point, pause with point on it, activate it, pause again and show result.
Run until end of buffer or an error.  If no error, print a success message with a count of the number of
buttons tested.  Delay time is the value of the  `test-hbuts-delay-time' setting."
  (interactive)
  (test-hbuts-from-point test-hbuts-delay-time))

(defun test-hbuts-from-point (display-delay-time)
  "For each Hyperbole button found after point, activate it.  Run until end of buffer or an error.
If no error, print a success message with a count of the number of buttons tested."
  (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0.0)))
  (let* ((obuf (current-buffer))
		 (ofile buffer-file-name)
		 (count 0)
		 (start (point))
		 (read-only buffer-read-only)
		 (timer (run-with-idle-timer 0.1 nil #'test-hbuts-activate display-delay-time obuf ofile))
		end)
	(unwind-protect
		(progn
		  ;; Force read-only mode so buttons tested don't accidentally
		  ;; modify the buffer being tested.
		  (read-only-mode 1)
		  (condition-case ()
			  (unwind-protect
		          (while (< (point) (point-max))
					(setq count (+ count (test-hbuts-activate display-delay-time obuf ofile))))
	            (message "Success: All %d Hyperbole buttons following position %d tested without error." count start))
			;; Allow for C-g aborts
			((debug error) (keyboard-quit))))
	  (when (timerp timer) (cancel-timer timer)))
	  (read-only-mode (if read-only 1 0))))

(defun test-hbuts-activate (display-delay-time obuf ofile)
  "Aggregate user inputs until a RET or no longer waiting for user input and send to Emacs' `unread-command-events' for processing.
Return count of Hyperbole buttons that were activated. "
  (let ((opoint (point))
        (count 0)
        end)
    (while (and (< opoint (point-max)) (waiting-for-user-input-p))
      (if (hbut:at-p)
	      (progn
		    (setq count (1+ count))
		    (hbut:act)
            ;; (kbd-key:key-series-to-events (kbd-key:normalize "test RET"))
	        (setq end (hattr:get 'hbut:current 'lbl-end))
	        (sit-for display-delay-time)
	        (if (buffer-live-p obuf)
		        (unless (eq (window-buffer (selected-window)) obuf)
		          (pop-to-buffer obuf))
	          (if ofile
		          (find-file-other-window ofile)
		        (error "(test-hbuts-activate): Buffer to test was deleted: %s" obuf)))
	        (goto-char (1+ (or end opoint))))
        (goto-char (1+ opoint)))
      (setq opoint (point)))
    count))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'test-hbuts)

;;; test-hbuts.el ends here
