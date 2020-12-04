;;; packages.el --- Space-macs Language Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-language-packages
      '((define-word :toggle (not (bound-and-true-p osx-use-dictionary-app)))
        google-translate))

(defun space-macs-language/init-define-word ()
  (use-package define-word
    :defer t
    :init
    (space-macs/set-leader-keys
      "xwd" 'define-word-at-point)))

(defun space-macs-language/init-google-translate ()
  (use-package google-translate
    :commands (space-macs/set-google-translate-languages)
    :init
    (progn
      (defun space-macs/set-google-translate-languages (&optional override-p)
        "Set source language for google translate.
For instance pass En as source for English."
        (interactive "P")
        (autoload 'google-translate-read-args "google-translate-default-ui")
        (let* ((langs (google-translate-read-args override-p nil))
               (source-language (car langs))
               (target-language (cadr langs)))
          (setq google-translate-default-source-language source-language)
          (setq google-translate-default-target-language target-language)
          (message
           (format "Set google translate source language to %s and target to %s"
                   source-language target-language))))

      (defun space-macs/set-google-translate-target-language ()
        "Set the target language for google translate."
        (interactive)
        (space-macs/set-google-translate-languages nil))

      (space-macs/set-leader-keys
        "xgL" 'space-macs/set-google-translate-languages
        "xgl" 'space-macs/set-google-translate-target-language
        "xgQ" 'google-translate-query-translate-reverse
        "xgq" 'google-translate-query-translate
        "xgT" 'google-translate-at-point-reverse
        "xgt" 'google-translate-at-point)
      (setq google-translate-enable-ido-completion t)
      (setq google-translate-show-phonetic t)
      (setq google-translate-default-source-language "auto"))))


