;;; packages.el --- languagetool layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Robbert van der Helm <mail@robbertvanderhelm.nl>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst languagetool-packages
  '((langtool :toggle (space-macs//languagetool-detect))))

(defun languagetool/init-langtool ()
    (use-package langtool
      :defer t
      :init
      (progn
        ;; The whitespace rules give a lot of false positives when linting rich
        ;; text.
        (setq-default langtool-disabled-rules '("WHITESPACE_RULE"))
        (space-macs/set-leader-keys
          "Sl" 'space-macs/languagetool-toggle
          "SL" 'langtool-correct-buffer)
        (define-key evil-normal-state-map (kbd "[ a")
          'space-macs/languagetool-previous-error)
        (define-key evil-normal-state-map (kbd "] a")
          'space-macs/languagetool-next-error))))


