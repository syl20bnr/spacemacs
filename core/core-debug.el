;;; core-debug.el --- Spacemacs Core File  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Interface to time execution of intialization, and collecting system information
;; for reporting bugs.

;;; Code:

(eval-when-compile
  ;; defined in init.el
  (defvar emacs-start-time)
  (defvar spacemacs-version)
  (defvar dotspacemacs-distribution)
  (defvar dotspacemacs-editing-style)
  (defvar dotspacemacs--configuration-layers-saved)
  (defvar evil-ex-commands)
  (defvar configuration-layer-template-directory))

(declare-function profiler-report "profiler" ())
(declare-function profiler-stop "profiler" ())
(declare-function spacemacs//git-get-current-branch "core-release-management" ())
(declare-function spacemacs/git-get-current-branch-rev "core-release-management" ())
(declare-function configuration-layer/layer-used-p "core-configuration-layer" (layer-name))
(declare-function spacemacs/set-leader-keys-for-major-mode "core-keybindings" (mode key def &rest bindings))


;;;; Debug Facility

(defvar spacemacs-debug-timer-threshold 0.15
  "Generate message if file takes longer than this number of seconds to load.")

(defvar spacemacs-debugp nil)
(defvar spacemacs-debug-with-profile nil)
(defvar spacemacs-debug-with-timed-requires nil)
(defvar spacemacs-debug-with-adv-timers nil)


;;;;; Timers

(defun spacemacs//timed-initialize (orig-func &rest args)
  "Time the invocation of `package-initialize' and return the time in seconds.
ORIG-FUNC must be `package-initialize', ARGS are arguments passed to it."
  (let ((start (current-time))
        delta)
    (prog1
        (apply orig-func args)
      (setq delta (float-time (time-since start)))
      (when (> delta spacemacs-debug-timer-threshold)
        (with-current-buffer "*load-times*"
          (goto-char (point-max))
          (insert (format "package-initialize took %.3f sec\n" delta)))))))

(defun spacemacs//timed-require(orig-func &rest args)
  "Time the execution of `require' and return the time in seconds.
ORIG-FUNC must be `require', ARGS are arguments passed to it."
  (let ((start (current-time))
        delta)
    (prog1
        (apply orig-func args)
      (setq delta (float-time (time-since start)))
      (when (> delta spacemacs-debug-timer-threshold)
        (with-current-buffer "*load-times*"
          (goto-char (point-max))
          (insert (format "File %s: Required %s: %.3f sec\n"
                          load-file-name (car args) delta)))))))

(defun spacemacs//timed-load(orig-func &rest args)
  "Time the execution of `load' and return the time in seconds.
ORIG-FUNC must be `load', ARGS are arguments passed to it."
  (let ((start (current-time))
        delta)
    (prog1
        (apply orig-func args)
      (setq delta (float-time (time-since start)))
      (when (> delta spacemacs-debug-timer-threshold)
        (with-current-buffer "*load-times*"
          (goto-char (point-max))
          (insert (format "File %s: Loaded %s: %.3f sec\n"
                          load-file-name (car args) delta)))))))


;;;;; Accumulation Timers

(defun spacemacs//load-timer (orig-func &rest args)
  "Time execution of `load' or `require' since start of Emacs.

When the time taken for such invocation exceeds
`spacemacs-debug-timer-threshold', it's recorded in \"*load-times*\" buffer.

ORIG-FUNC is the function to be timed, this should be `load' or `require'.
ARGS are the arguments passed to ORIG-FUNC.

This function should not be used directly. Use `spacemacs||add-function-timer'
instead."
  (let ((start (current-time))
        (required (car args))
        delta)
    (prog1
        (apply orig-func args)
      (setq delta (float-time (time-since start)))
      (when (> delta spacemacs-debug-timer-threshold)
        (with-current-buffer "*load-times*"
          (goto-char (point-max))
          (insert (format "[%.3f] (%.3f) Load or require\n    Feature: %s\n    In file: %s\n\n"
                          (float-time (time-since emacs-start-time))
                          delta required load-file-name)))))))

(defmacro spacemacs||add-function-timer (func)
  "Time the execution of FUNC since start of Emacs.

A timer function is created and added as the `:around' advice to FUNC.

When the time taken for loading a package exceeds
`spacemacs-debug-timer-threshold', it's recorded in \"*load-times\" buffer."
  (if (memq func '(load require))
      `(advice-add ',func :around #'spacemacs//load-timer)
    (let ((timer
           (lambda (orig-func &rest args)
             (let ((start (current-time))
                   delta)
               (prog1
                   (apply orig-func args)
                 (setq delta (float-time (time-since start)))
                 (when (> delta spacemacs-debug-timer-threshold)
                   (with-current-buffer "*load-times*"
                     (goto-char (point-max))
                     (insert (format "[%.3f] (%.3f) Function call\n    Function: %s\n    Args: %s\n\n"
                                     (float-time (time-since emacs-start-time))
                                     delta func args)))))))))
      `(advice-add ',func :around #',timer))))


(defun spacemacs/init-debug ()
  "Set up debug hooks."

  ;; CPU + memory profiler
  (when spacemacs-debug-with-profile
    (require 'profiler)
    (profiler-start 'cpu+mem)
    (add-hook 'after-init-hook
              (lambda ()
                (run-with-idle-timer 2 nil (lambda ()
                                             (profiler-report)
                                             (profiler-stop))))))

  ;; Timing the duration of each loading
  (when spacemacs-debug-with-timed-requires
    (require 'time-date)
    (with-current-buffer (get-buffer-create "*load-times*")
        (insert (format "Threshold set at %.3f seconds\n\n"
                        spacemacs-debug-timer-threshold))

      (advice-add 'package-initialize :around #'spacemacs//timed-initialize)
      (advice-add 'require :around #'spacemacs//timed-require)
      (advice-add 'load :around #'spacemacs//timed-load)))

  ;; Timing the accumulative duration of each loading
  (when spacemacs-debug-with-adv-timers
    (require 'time-date)
    (with-current-buffer (get-buffer-create "*load-times*")
      (insert (format "Measured times greater than %.3f sec:\n\n"
                      spacemacs-debug-timer-threshold)))

    (add-hook 'after-init-hook
              (lambda ()
                (with-current-buffer "*load-times*"
                  (goto-char (point-max))
                  (insert (format "[%.3f] Spacemacs finished initializing\n\n"
                                  (float-time (time-since emacs-start-time)))))))

    (spacemacs||add-function-timer load)
    (spacemacs||add-function-timer require)
    (spacemacs||add-function-timer package-initialize)
    (spacemacs||add-function-timer configuration-layer/load)
    (spacemacs||add-function-timer configuration-layer//configure-package))

  ;; Keep debug-on-error on for stuff that is lazily loaded
  (add-hook 'after-init-hook (lambda () (setq debug-on-error t))))


;; Report issue

(defun spacemacs//describe-system-info-string ()
  "Gather info about your Spacemacs setup and return it as a string."
  (format
   (concat "#### System Info :computer:\n"
           "- OS: %s\n"
           "- Emacs: %s\n"
           "- Spacemacs: %s\n"
           "- Spacemacs branch: %s (rev. %s)\n"
           "- Graphic display: %s\n"
           "- Running in daemon: %s\n"
           "- Distribution: %s\n"
           "- Editing style: %s\n"
           "- Completion: %s\n"
           "- Layers:\n```elisp\n%s```\n"
           "- System configuration features: %s\n")
   system-type
   emacs-version
   spacemacs-version
   (spacemacs//git-get-current-branch)
   (spacemacs/git-get-current-branch-rev)
   (display-graphic-p)
   (daemonp)
   dotspacemacs-distribution
   dotspacemacs-editing-style
   (cond ((configuration-layer/layer-used-p 'helm)
          'helm)
         ((configuration-layer/layer-used-p 'ivy)
          'ivy)
         ((configuration-layer/layer-used-p 'compleseus)
          'compleseus)
         (t 'helm))
   (pp-to-string dotspacemacs--configuration-layers-saved)
   (bound-and-true-p system-configuration-features)))

(defun spacemacs/describe-system-info ()
  "Gather info about your Spacemacs setup and copy it to clipboard.
System information is copied to clipboard.
In case it's killed by other programs, it's also send to \"*Messages*\" buffer."
  (interactive)
  (let ((sysinfo (spacemacs//describe-system-info-string)))
    (kill-new sysinfo)
    (message sysinfo)
    (message (concat "Information has been copied to clipboard.\n"
                     "You can paste it in the gitter chat.\n"
                     "Check the \"*Messages*\" buffer if you need to review it"))))

(defun spacemacs//describe-last-keys-string ()
  "Gather info about last few key inputs and return it as a string."
  (let ((keys (key-description (recent-keys))))
    (with-temp-buffer
      (set-fill-column 60)
      (insert keys)
      (fill-region (point-min) (point-max))
      (format "#### Emacs last keys :musical_keyboard: \n```text\n%s\n```\n" (buffer-string)))))

(defun spacemacs/describe-last-keys ()
  "Gather info about last few key inputs and copy it to clipboard."
  (interactive)
  (let ((keys (spacemacs//describe-last-keys-string)))
    (kill-new keys)
    (message keys)
    (message (concat "Information has been copied to clipboard.\n"
                     (propertize
                      "PLEASE REVIEW THE DATA BEFORE GOING FURTHER AS IT CAN CONTAIN SENSITIVE DATA (PASSWORD, ...)\n"
                      'face 'font-lock-warning-face)
                     "You can paste it in the gitter chat.\n"
                     "Check the *Messages* buffer if you need to review it"))))

(defun spacemacs/describe-ex-command (ex-command)
  "Describe an `evil-ex-commands'.
EX-COMMAND must be a command in `evil-ex-commands'."
  (interactive (list (completing-read "Describe ex-command: " evil-ex-commands nil t)))
  (let* ((func (alist-get ex-command evil-ex-commands nil nil 'string=))
         (prompt (if (stringp func)
                     "The ex-command :%s is an alias for the ex-command :%s. Describe :%s?"
                   "The ex-command :%s calls %s. Describe %s?")))
    (when (y-or-n-p (format prompt
                            ex-command
                            func
                            func))
      (if (stringp func)
          (spacemacs/describe-ex-command func)
        (describe-function func)))))

(defun spacemacs/report-issue (arg)
  "Open a buffer with issue report template and system information.
When prefix ARG is non-nil, include the last keys pressed."
  (interactive "P")
  (let ((buf (generate-new-buffer "REPORT_SPACEMACS_ISSUE"))
        (system-info (spacemacs//describe-system-info-string))
        (backtrace (if (get-buffer "*Backtrace*")
                       (with-current-buffer "*Backtrace*"
                         (buffer-substring-no-properties
                          (point-min)
                          (min (point-max) 1000)))
                     "<<BACKTRACE IF RELEVANT>>"))
        (last-keys
         (if (and arg (y-or-n-p "Do you really want to include your last pressed keys, which may include some sensitive data? "))
             (concat (spacemacs//describe-last-keys-string) "\n")
           "")))
    (switch-to-buffer buf)
    (let ((ov (make-overlay (point-min) (point-min)))
          (prop-val
           (concat (propertize "REPLACE ALL UPPERCASE EXPRESSIONS\nPRESS `C-c C-c` TO SUBMIT, OR PRESS `C-c C-k` TO DISCARD"
                               'display
                               '(raise -1)
                               'face
                               'font-lock-warning-face)
                   "\n\n")))
      (overlay-put ov 'after-string prop-val))
    (insert-file-contents (concat configuration-layer-template-directory "REPORTING.template"))
    (cl-loop for (placeholder replacement) in `(("%SYSTEM_INFO%" ,system-info)
                                                ("%BACKTRACE%" ,backtrace)
                                                ("(%LAST_KEYS%)\n" ,last-keys))
             do (save-excursion
                  (goto-char (point-min))
                  (search-forward placeholder)
                  (replace-match replacement 'keep-case 'literal)))
    (set-buffer-modified-p nil)
    (spacemacs/report-issue-mode)))

(defun spacemacs//report-issue-kill-buffer-query ()
  "Check if issue has been edited when buffer is about to be killed.
This is intended to be used with `kill-buffer-query-functions'."
  (if (buffer-modified-p)
      (y-or-n-p "Issue has unsaved changes, kill buffer anyways? ")
    t))

(define-derived-mode spacemacs/report-issue-mode text-mode "Report-Issue"
  "Major mode for reporting issues with Spacemacs.

When done editing, you can type \\<spacemacs/report-issue-mode-map>\\[spacemacs//report-issue-done] to create the issue on GitHub.
You must be logged in already for this to work.

After you see that the issue has been created successfully, you can close this buffer.

At any time, you can type \\[kill-buffer] to close this buffer.

\\{spacemacs/report-issue-mode-map}
"
  (font-lock-add-keywords 'spacemacs/report-issue-mode
                          '(("\\(<<.*?>>\\)" . 'font-lock-comment-face)))
  (add-hook 'kill-buffer-query-functions
            'spacemacs//report-issue-kill-buffer-query
            nil t))

(define-key spacemacs/report-issue-mode-map
  (kbd "C-c C-c") 'spacemacs//report-issue-done)
(define-key spacemacs/report-issue-mode-map
  (kbd "C-c C-k") 'kill-buffer)

(with-eval-after-load 'bind-map
  (spacemacs/set-leader-keys-for-major-mode 'spacemacs/report-issue-mode
    "," 'spacemacs//report-issue-done
    "c" 'spacemacs//report-issue-done
    "a" 'kill-buffer
    "k" 'kill-buffer))

(defun spacemacs//report-issue-done ()
  "Try to create an GitHub issue with system info.
If the resulting URL is too long (> 8192 characters), it fallbacks to copying
the buffer content to clipboard and opens an empty GitHub issue page."
  (interactive)
  (let* ((url-prefix "http://github.com/syl20bnr/spacemacs/issues/new?body=")
         (body (url-hexify-string (buffer-string)))
         (url (url-encode-url (concat url-prefix body))))
    (if (< (length url) 8192)
        (browse-url url)
      (copy-region-as-kill (point-min) (point-max))
      (browse-url url-prefix)
      (message (concat "Information has been copied to clipboard.\n"
                       "Please paste it as the body of the GitHub issue.\n"
                       (propertize
                        "PLEASE REVIEW THE DATA BEFORE GOING FURTHER AS IT CAN CONTAIN SENSITIVE DATA (PASSWORD, ...)\n"
                        'face 'font-lock-warning-face))))))


;; misc
(defun spacemacs/display-and-copy-version ()
  "Echo the current spacemacs version and copy it."
  (interactive)
  (let ((msg (format "Spacemacs v.%s" spacemacs-version)))
    (message msg) (kill-new msg)))

(provide 'core-debug)
;;; core-debug.el ends here
