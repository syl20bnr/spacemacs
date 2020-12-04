;;; core-debug.el --- Space-macs Core File  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/display-and-copy-version ()
  "Echo the current space-macs version and copy it."
  (interactive)
  (let ((msg (format "Space-macs v.%s" space-macs-version)))
    (message msg) (kill-new msg)))


;; startup debug

(require 'profiler)

(defvar space-macs-debug-timer-threshold 0.15
  "Generate message if file takes longer than this number of
seconds to load")

(defvar space-macs-debugp nil)
(defvar space-macs-debug-with-profile nil)
(defvar space-macs-debug-with-timed-requires nil)
(defvar space-macs-debug-with-adv-timers nil)

(defun space-macs//load-timer (origfunc &rest args)
  "Used to time invocation of `require' or `load'."
  (let ((start (current-time))
        (required (car args))
        delta)
    (prog1
        (apply origfunc args)
      (setq delta (float-time (time-since start)))
      (when (> delta space-macs-debug-timer-threshold)
        (with-current-buffer "*load-times*"
          (goto-char (point-max))
          (insert (format "[%.3f] (%.3f) Load or require\n    Feature: %s\n    In file: %s\n\n"
                          (float-time (time-since e-macs-start-time))
                          delta required load-file-name)))))))

(defmacro space-macs||make-function-timer (func)
  "Used to time call to FUNC."
  `(lambda (origfunc &rest args)
     (let ((start (current-time))
           delta)
       (prog1
           (apply origfunc args)
         (setq delta (float-time (time-since start)))
         (when (> delta space-macs-debug-timer-threshold)
           (with-current-buffer "*load-times*"
             (goto-char (point-max))
             (insert (format "[%.3f] (%.3f) Function call\n    Function: %s\n    Args: %s\n\n"
                             (float-time (time-since e-macs-start-time))
                             delta ',func args))))))))

(defmacro space-macs||make-function-profiler (func)
  `(lambda (origfunc &rest args)
     (if (profiler-running-p)
         (profiler-report)
       (profiler-start 'cpu))
     (prog1
         (apply origfunc args)
       (with-current-buffer "*load-times*"
         (goto-char (point-max))
         (insert (format "[%.3f] Done profiling function: %s\n\n"
                         (float-time (time-since e-macs-start-time)) ',func)))
       (profiler-report))))

(defun space-macs/init-debug ()
  "Set the debug hooks."
  (when space-macs-debug-with-profile
    (profiler-start 'cpu+mem)
    (add-hook 'after-init-hook
              (lambda ()
                (run-with-idle-timer 2 nil (lambda ()
                                             (profiler-report)
                                             (profiler-stop))))))

  (when (version<= "27.0" e-macs-version)
    (require 'time-date))
  (when space-macs-debug-with-timed-requires
    (with-current-buffer (get-buffer-create "*load-times*")
      (insert (format "Threshold set at %.3f seconds\n\n"
                      space-macs-debug-timer-threshold)))

    (defadvice package-initialize (around space-macs//timed-initialize activate)
      (let ((start (current-time)) res delta)
        (setq res ad-do-it
              delta (float-time (time-since start)))
        (when (> delta space-macs-debug-timer-threshold)
          (with-current-buffer "*load-times*"
            (goto-char (point-max))
            (insert (format "package-initialize took %.3f sec\n" delta))))
        res))

    (defadvice require (around space-macs//timed-require activate)
      (let ((start (current-time)) res delta)
        (setq res ad-do-it
              delta (float-time (time-since start)))
        (when (> delta space-macs-debug-timer-threshold)
          (with-current-buffer "*load-times*"
            (goto-char (point-max))
            (insert (format "File %s: Required %s: %.3f sec\n"
                            load-file-name (ad-get-arg 0) delta))))
        res))

    (defadvice load (around space-macs//timed-load activate)
      (let ((start (current-time)) res delta)
        (setq res ad-do-it
              delta (float-time (time-since start)))
        (when (> delta space-macs-debug-timer-threshold)
          (with-current-buffer "*load-times*"
            (goto-char (point-max))
            (insert (format "File %s: Loaded %s: %.3f sec\n"
                            load-file-name (ad-get-arg 0) delta))))
        res)))

  (when space-macs-debug-with-adv-timers
    (with-current-buffer (get-buffer-create "*load-times*")
      (insert (format "Measured times greater than %.3f sec:\n\n"
                      space-macs-debug-timer-threshold)))

    (add-hook 'after-init-hook
              (lambda ()
                (with-current-buffer "*load-times*"
                  (goto-char (point-max))
                  (insert (format "[%.3f] Space-macs finished initializing\n\n"
                                  (float-time (time-since e-macs-start-time)) )))))

    (advice-add 'load :around #'space-macs//load-timer)
    (advice-add 'require :around #'space-macs//load-timer)
    (advice-add 'package-initialize
                :around
                (space-macs||make-function-timer package-initialize))
    (advice-add 'configuration-layer/load
                :around
                (space-macs||make-function-timer configuration-layer/load))
    ;; (advice-add 'configuration-layer/load
    ;;             :around
    ;;             (space-macs||make-function-profiler configuration-layer/load))
    (advice-add 'configuration-layer//configure-package
                :around
                (space-macs||make-function-timer configuration-layer//configure-package)))

  ;; Keep debug-on-error on for stuff that is lazily loaded
  (add-hook 'after-init-hook (lambda () (setq debug-on-error t))))


;; Report issue

(defun space-macs//describe-system-info-string ()
  "Gathers info about your Space-macs setup and returns it as a string."
  (format
   (concat "#### System Info :computer:\n"
           "- OS: %s\n"
           "- e-macs: %s\n"
           "- Space-macs: %s\n"
           "- Space-macs branch: %s (rev. %s)\n"
           "- Graphic display: %s\n"
           "- Distribution: %s\n"
           "- Editing style: %s\n"
           "- Completion: %s\n"
           "- Layers:\n```elisp\n%s```\n"
           (when (version<= "25.1" e-macs-version)
             "- System configuration features: %s\n"))
   system-type
   e-macs-version
   space-macs-version
   (space-macs//git-get-current-branch)
   (space-macs/git-get-current-branch-rev)
   (display-graphic-p)
   dotspace-macs-distribution
   dotspace-macs-editing-style
   (cond ((configuration-layer/layer-used-p 'helm)
          'helm)
         ((configuration-layer/layer-used-p 'ivy)
          'ivy)
         (t 'helm))
   (pp-to-string dotspace-macs--configuration-layers-saved)
   (bound-and-true-p system-configuration-features)))

(defun space-macs/describe-system-info ()
  "Gathers info about your Space-macs setup and copies to clipboard."
  (interactive)
  (let ((sysinfo (space-macs//describe-system-info-string)))
    (kill-new sysinfo)
    (message sysinfo)
    (message (concat "Information has been copied to clipboard.\n"
                     "You can paste it in the gitter chat.\n"
                     "Check the *Messages* buffer if you need to review it"))))

(defun space-macs//describe-last-keys-string ()
  "Gathers info about your e-macs last keys and returns it as a string."
  (cl-loop
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
              (format "#### e-macs last keys :musical_keyboard: \n```text\n%s\n```\n" (buffer-string))))))

(defun space-macs/describe-last-keys ()
  "Gathers info about your e-macs last keys and copies to clipboard."
  (interactive)
  (let ((lossage (space-macs//describe-last-keys-string)))
    (kill-new lossage)
    (message lossage)
    (message (concat "Information has been copied to clipboard.\n"
                     (propertize
                      "PLEASE REVIEW THE DATA BEFORE GOING FURTHER AS IT CAN CONTAIN SENSITIVE DATA (PASSWORD, ...)\n"
                      'face 'font-lock-warning-face)
                     "You can paste it in the gitter chat.\n"
                     "Check the *Messages* buffer if you need to review it"))))

(defun space-macs/report-issue (arg)
  "Open a space-macs/report-issue-mode buffer prepopulated with
   issue report template and system information.

   With prefix arg,include the last keys pressed."
  (interactive "P")
  (let ((buf
         (generate-new-buffer "REPORT_SPACe-macs_ISSUE"))
        (system-info
         (space-macs//describe-system-info-string))
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
             (concat (space-macs//describe-last-keys-string) "\n")
           "")))
    (switch-to-buffer buf)
    (let ((ov (make-overlay (point-min) (point-min)))
          (prop-val
           (concat (propertize (concat "REPLACE ALL UPPERCASE EXPRESSIONS"
                                       " AND PRESS `C-c C-c` TO SUBMIT")
                               'display
                               '(raise -1)
                               'face
                               'font-lock-warning-face)
                   "\n\n")))
      (overlay-put ov 'after-string prop-val))
    (insert-file-contents
     (concat configuration-layer-template-directory "REPORTING.template"))
    (cl-loop
     for (placeholder replacement)
     in `(("%SYSTEM_INFO%" ,system-info)
          ("%BACKTRACE%" ,backtrace)
          ("(%LAST_KEYS%)\n" ,last-keys))
     do (save-excursion
          (goto-char (point-min))
          (search-forward placeholder)
          (replace-match replacement [keep-case] [literal])))
    (set-buffer-modified-p nil)
    (space-macs/report-issue-mode)))

(defun space-macs//report-issue-kill-buffer-query ()
  "Check if issue has been edited when buffer is about to be
  killed. Intended to be used with
  `kill-buffer-query-functions'"
  (if (buffer-modified-p)
      (y-or-n-p "Issue has unsaved changes, kill buffer anyways? ")
    t))

(define-derived-mode space-macs/report-issue-mode text-mode "Report-Issue"
  "Major mode for reporting issues with Space-macs.

When done editing, you can type \\[space-macs//report-issue-done] to create the
issue on GitHub. You must be logged in already for this to work. After you see
that the issue has been created successfully, you can close this buffer.

\\{space-macs/report-issue-mode-map}
"
  (font-lock-add-keywords 'space-macs/report-issue-mode
                          '(("\\(<<.*?>>\\)" . 'font-lock-comment-face)))
  (add-hook 'kill-buffer-query-functions
            'space-macs//report-issue-kill-buffer-query
            nil t))

(define-key space-macs/report-issue-mode-map
  (kbd "C-c C-c") 'space-macs//report-issue-done)
(define-key space-macs/report-issue-mode-map
  (kbd "C-c C-k") 'kill-buffer)

(with-eval-after-load 'bind-map
  (space-macs/set-leader-keys-for-major-mode 'space-macs/report-issue-mode
    "," 'space-macs//report-issue-done
    "c" 'space-macs//report-issue-done
    "a" 'kill-buffer
    "k" 'kill-buffer))

(defun space-macs//report-issue-done ()
  (interactive)
  (let ((url "http://github.com/syl20bnr/space-macs/issues/new?body=")
        (body (url-hexify-string (buffer-string))))
    (browse-url (url-encode-url (concat url body)))))

(provide 'core-debug)


