;;; core-debug.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Keep debug-on-error on for stuff that is lazily loaded
(add-hook 'after-init-hook (lambda () (setq debug-on-error t)))

(when (member "--profile" command-line-args)
  (setq command-line-args (delete "--profile" command-line-args))
  (profiler-start 'cpu+mem)
  (add-hook 'after-init-hook
            (lambda ()
              (run-with-idle-timer 2 nil (lambda ()
                                           (profiler-report)
                                           (profiler-stop))))))

(when (member "--timed-requires" command-line-args)
  (setq command-line-args (delete "--timed-requires" command-line-args))

  (defvar spacemacs-load-time-threshold 0.15
    "Generate message if file takes longer than this number of
seconds to load")

  (with-current-buffer (get-buffer-create "*load-times*")
    (insert (format "All files that took longer than %.3f seconds to load\n\n"
                    spacemacs-load-time-threshold)))

  (defadvice require (around spacemacs//timed-require activate)
    (let ((start (current-time)) res delta)
      (setq res ad-do-it
            delta (float-time (time-since start)))
      (when (> delta spacemacs-load-time-threshold)
        (with-current-buffer "*load-times*"
          (goto-char (point-max))
          (insert (format "File %s: Required %s: %.3f sec\n"
                          load-file-name (ad-get-arg 0) delta))))
      res))

  (defadvice load (around spacemacs//timed-load activate)
    (let ((start (current-time)) res delta)
      (setq res ad-do-it
            delta (float-time (time-since start)))
      (when (> delta spacemacs-load-time-threshold)
        (with-current-buffer "*load-times*"
          (goto-char (point-max))
          (insert (format "File %s: Loaded %s: %.3f sec\n"
                          load-file-name (ad-get-arg 0) delta))))
      res)))

(provide 'core-debug)
