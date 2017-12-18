;;; packages.el --- Java functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; Gradle

(when (configuration-layer/package-used-p 'gradle-mode)
  (defun spacemacs/gradle-clean-build ()
    "Execute 'gradle clean build' command."
    (interactive)
    (gradle-execute "clean build"))

  (defun spacemacs/gradle-test-buffer ()
    "Execute 'gradle test' command against current buffer tests."
    (interactive)
    (gradle-single-test (file-name-base (buffer-file-name)))))

;; REPL

(defun spacemacs/groovy-send-region-switch (start end)
  "Send region content to REPL and switch to it in insert mode."
  (interactive "r")
  (groovy-send-region-and-go start end)
  (evil-insert-state))

(defun spacemacs/groovy-send-definition-switch ()
  "Send function content to REPL and switch to it in insert mode."
  (interactive)
  (groovy-send-definition-and-go)
  (evil-insert-state))

(defun spacemacs/groovy-load-file ()
  "Save buffer, load it to REPL."
  (interactive)
  (save-buffer)
  (groovy-load-file (buffer-file-name)))

(defun spacemacs/groovy-load-file-switch ()
  "Save buffer, load buffer to REPL and switch to it in insert mode."
  (interactive)
  (spacemacs/groovy-load-file)
  (switch-to-groovy nil)
  (evil-insert-state))
