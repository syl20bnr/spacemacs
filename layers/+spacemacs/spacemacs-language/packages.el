;;; packages.el --- Spacemacs Language Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-language-packages
      '((define-word :toggle (not (bound-and-true-p osx-use-dictionary-app)))
        google-translate))

(defun spacemacs-language/init-define-word ()
  (use-package define-word
    :defer t
    :init
    (spacemacs/set-leader-keys
      "xwd" 'define-word-at-point)))

(defun spacemacs-language/init-google-translate ()
  (use-package google-translate
    :commands (spacemacs/set-google-translate-languages)
    :init
    (progn
      (defun spacemacs/set-google-translate-languages (source target)
        "Set source language for google translate.
For instance pass En as source for English."
        (interactive
         "sEnter source language (ie. en): \nsEnter target language (ie. en): "
         source target)
        (message
         (format "Set google translate source language to %s and target to %s"
                 source target))
        (setq google-translate-default-source-language (downcase source))
        (setq google-translate-default-target-language (downcase target)))
      (spacemacs/set-leader-keys
        "xgl" 'spacemacs/set-google-translate-languages
        "xgQ" 'google-translate-query-translate-reverse
        "xgq" 'google-translate-query-translate
        "xgT" 'google-translate-at-point-reverse
        "xgt" 'google-translate-at-point)
      (setq google-translate-enable-ido-completion t)
      (setq google-translate-show-phonetic t)
      (setq google-translate-default-source-language "en")
      (setq google-translate-default-target-language "fr"))))
