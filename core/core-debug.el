;;; core-debug.el --- Spacemacs Core File  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/display-and-copy-version ()
  "Echo the current spacemacs version and copy it."
  (interactive)
  (let ((msg (format "Spacemacs v.%s" spacemacs-version)))
    (message msg) (kill-new msg)))


;; startup debug

(require 'profiler)

(defvar spacemacs-debug-timer-threshold 0.15
  "Generate message if file takes longer than this number of
seconds to load")

(defvar spacemacs-debugp nil)
(defvar spacemacs-debug-with-profile nil)
(defvar spacemacs-debug-with-timed-requires nil)
(defvar spacemacs-debug-with-adv-timers nil)

(defun spacemacs//load-timer (origfunc &rest args)
  "Used to time invocation of `require' or `load'."
  (let ((start (current-time))
        (required (car args))
        delta)
    (prog1
        (apply origfunc args)
      (setq delta (float-time (time-since start)))
      (when (> delta spacemacs-debug-timer-threshold)
        (with-current-buffer "*load-times*"
          (goto-char (point-max))
          (insert (format "[%.3f] (%.3f) Load or require\n    Feature: %s\n    In file: %s\n\n"
                          (float-time (time-since emacs-start-time))
                          delta required load-file-name)))))))

(defmacro spacemacs||make-function-timer (func)
  "Used to time call to FUNC."
  `(lambda (origfunc &rest args)
     (let ((start (current-time))
           delta)
       (prog1
           (apply origfunc args)
         (setq delta (float-time (time-since start)))
         (when (> delta spacemacs-debug-timer-threshold)
           (with-current-buffer "*load-times*"
             (goto-char (point-max))
             (insert (format "[%.3f] (%.3f) Function call\n    Function: %s\n    Args: %s\n\n"
                             (float-time (time-since emacs-start-time))
                             delta ',func args))))))))

(defmacro spacemacs||make-function-profiler (func)
  `(lambda (origfunc &rest args)
     (if (profiler-running-p)
         (profiler-report)
       (profiler-start 'cpu))
     (prog1
         (apply origfunc args)
       (with-current-buffer "*load-times*"
         (goto-char (point-max))
         (insert (format "[%.3f] Done profiling function: %s\n\n"
                         (float-time (time-since emacs-start-time)) ',func)))
       (profiler-report))))

(defun spacemacs/init-debug ()
  "Set the debug hooks."
  (when spacemacs-debug-with-profile
    (profiler-start 'cpu+mem)
    (add-hook 'after-init-hook
              (lambda ()
                (run-with-idle-timer 2 nil (lambda ()
                                             (profiler-report)
                                             (profiler-stop))))))

  (when spacemacs-debug-with-timed-requires
    (with-current-buffer (get-buffer-create "*load-times*")
      (insert (format "Threshold set at %.3f seconds\n\n"
                      spacemacs-debug-timer-threshold)))

    (defadvice package-initialize (around spacemacs//timed-initialize activate)
      (let ((start (current-time)) res delta)
        (setq res ad-do-it
              delta (float-time (time-since start)))
        (when (> delta spacemacs-debug-timer-threshold)
          (with-current-buffer "*load-times*"
            (goto-char (point-max))
            (insert (format "package-initialize took %.3f sec\n" delta))))
        res))

    (defadvice require (around spacemacs//timed-require activate)
      (let ((start (current-time)) res delta)
        (setq res ad-do-it
              delta (float-time (time-since start)))
        (when (> delta spacemacs-debug-timer-threshold)
          (with-current-buffer "*load-times*"
            (goto-char (point-max))
            (insert (format "File %s: Required %s: %.3f sec\n"
                            load-file-name (ad-get-arg 0) delta))))
        res))

    (defadvice load (around spacemacs//timed-load activate)
      (let ((start (current-time)) res delta)
        (setq res ad-do-it
              delta (float-time (time-since start)))
        (when (> delta spacemacs-debug-timer-threshold)
          (with-current-buffer "*load-times*"
            (goto-char (point-max))
            (insert (format "File %s: Loaded %s: %.3f sec\n"
                            load-file-name (ad-get-arg 0) delta))))
        res)))

  (when spacemacs-debug-with-adv-timers
    (with-current-buffer (get-buffer-create "*load-times*")
      (insert (format "Measured times greater than %.3f sec:\n\n"
                      spacemacs-debug-timer-threshold)))

    (add-hook 'after-init-hook
              (lambda ()
                (with-current-buffer "*load-times*"
                  (goto-char (point-max))
                  (insert (format "[%.3f] Spacemacs finished initializing\n\n"
                                  (float-time (time-since emacs-start-time)) )))))

    (advice-add 'load :around #'spacemacs//load-timer)
    (advice-add 'require :around #'spacemacs//load-timer)
    (advice-add 'package-initialize
                :around
                (spacemacs||make-function-timer package-intialize))
    (advice-add 'configuration-layer/sync
                :around
                (spacemacs||make-function-timer configuration-layer/sync))
    ;; (advice-add 'configuration-layer/sync
    ;;             :around
    ;;             (spacemacs||make-function-profiler configuration-layer/sync))
    (advice-add 'configuration-layer//configure-package
                :around
                (spacemacs||make-function-timer configuration-layer//configure-package)))

  ;; Keep debug-on-error on for stuff that is lazily loaded
  (add-hook 'after-init-hook (lambda () (setq debug-on-error t))))


;; Report issue

(defun spacemacs//describe-system-info-string ()
  "Gathers info about your Spacemacs setup and returns it as a string."
  (format
   (concat "#### System Info :computer:\n"
           "- OS: %s\n"
           "- Emacs: %s\n"
           "- Spacemacs: %s\n"
           "- Spacemacs branch: %s (rev. %s)\n"
           "- Graphic display: %s\n"
           "- Distribution: %s\n"
           "- Editing style: %s\n"
           "- Completion: %s\n"
           "- Layers:\n```elisp\n%s```\n"
           (when (version<= "25.1" emacs-version)
             "- System configuration features: %s\n"))
   system-type
   emacs-version
   spacemacs-version
   (spacemacs//git-get-current-branch)
   (spacemacs/git-get-current-branch-rev)
   (display-graphic-p)
   dotspacemacs-distribution
   dotspacemacs-editing-style
   (cond ((configuration-layer/layer-usedp 'helm)
          'helm)
         ((configuration-layer/layer-usedp 'ivy)
          'ivy)
         (t 'helm))
   (pp-to-string dotspacemacs--configuration-layers-saved)
   (bound-and-true-p system-configuration-features)))

(defun spacemacs/describe-system-info ()
  "Gathers info about your Spacemacs setup and copies to clipboard."
  (interactive)
  (let ((sysinfo (spacemacs//describe-system-info-string)))
    (kill-new sysinfo)
    (message sysinfo)
    (message (concat "Information has been copied to clipboard.\n"
                     "You can paste it in the gitter chat.\n"
                     "Check the *Messages* buffer if you need to review it"))))

(defun spacemacs//describe-last-keys-string ()
  "Gathers info about your Emacs last keys and returns it as a string."
  (loop
   for key
   across (recent-keys)
   collect (if (or (integerp key) (symbolp key) (listp key))
               (single-key-description key)
             (prin1-to-string key))
   into keys
   finally (return
            (with-temp-buffer
              (set-fill-column 60)
              (insert (mapconcat 'identity keys " "))
              (fill-region (point-min) (point-max))
              (format "#### Emacs last keys :musical_keyboard: \n```text\n%s\n```\n" (buffer-string))))))

(defun spacemacs/describe-last-keys ()
  "Gathers info about your Emacs last keys and copies to clipboard."
  (interactive)
  (let ((lossage (spacemacs//describe-last-keys-string)))
    (kill-new lossage)
    (message lossage)
    (message (concat "Information has been copied to clipboard.\n"
                     (propertize
                      "PLEASE REVIEW THE DATA BEFORE GOING FURTHER AS IT CAN CONTAIN SENSITIVE DATA (PASSWORD, ...)\n"
                      'face 'font-lock-warning-face)
                     "You can paste it in the gitter chat.\n"
                     "Check the *Messages* buffer if you need to review it"))))

(defun spacemacs/report-issue (arg)
  "Open a spacemacs/report-issue-mode buffer prepopulated with
   issue report template and system information.

   With prefix arg,include the last keys pressed."
  (interactive "P")
  (let ((buf
         (generate-new-buffer "REPORT_SPACEMACS_ISSUE"))
        (system-info
         (spacemacs//describe-system-info-string))
        (backtrace
         (if (get-buffer "*Backtrace*")
             (with-current-buffer "*Backtrace*"
               (buffer-substring-no-properties
                (point-min)
                (min (point-max) 1000)))
           "<<BACKTRACE IF RELEVANT>>"))
        (last-keys
         (if (and arg (y-or-n-p (concat "Do you really want to "
                                        "include your last pressed keys? It "
                                        "may include some sensitive data.")))
             (concat (spacemacs//describe-last-keys-string) "\n")
           "")))
    (switch-to-buffer buf)
    (insert-file-contents-literally
     (concat configuration-layer-template-directory "REPORTING.template"))
    (loop
     for (placeholder replacement)
     in `(("%SYSTEM_INFO%" ,system-info)
          ("%BACKTRACE%" ,backtrace)
          ("(%LAST_KEYS%)\n" ,last-keys))
     do (save-excursion
          (goto-char (point-min))
          (search-forward placeholder)
          (replace-match replacement [keep-case] [literal])))
    (spacemacs/report-issue-mode)))

(define-derived-mode spacemacs/report-issue-mode markdown-mode "Report-Issue"
  "Major mode for reporting issues with Spacemacs.

When done editing, you can type \\[spacemacs//report-issue-done] to create the
issue on Github. You must be logged in already for this to work. After you see
that the issue has been created successfully, you can close this buffer.

Markdown syntax is supported in this buffer.

\\{spacemacs/report-issue-mode-map}
"
  (font-lock-add-keywords 'spacemacs/report-issue-mode
                          '(("\\(<<.*?>>\\)" . 'font-lock-comment-face))))

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
  (interactive)
  (let ((url "http://github.com/syl20bnr/spacemacs/issues/new?body="))
    (setq url (url-encode-url (concat url (buffer-string))))
    ;; HACK: encode some characters according to HTML URL Encoding Reference
    ;; http://www.w3schools.com/tags/ref_urlencode.asp
    (setq url (replace-regexp-in-string "#" "%23" url))
    (setq url (replace-regexp-in-string ";" "%3B" url))
    (browse-url url)))

(provide 'core-debug)
