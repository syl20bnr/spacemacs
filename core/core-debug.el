;;; core-debug.el --- Spacemacs Core File  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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

(provide 'core-debug)
