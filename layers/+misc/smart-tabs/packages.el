;;; packages.el --- smart-tabs layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Curtis Mackie <curtis@mackie.ninja>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; This layer integrates smart-tabs-mode with Spacemacs, allowing users to indent
;; with tabs and align with spaces. It ensures that it works as expected with
;; evil's = operator, as well as streamlining the configuration process for new
;; language support.

;;; Code:

(defconst smart-tabs-packages
  '(smart-tabs-mode))

(defun smart-tabs/init-smart-tabs-mode ()
  (use-package smart-tabs-mode
    :diminish smart-tabs-mode
    :config
    (progn
      (add-hook 'smart-tabs-mode-hook
                (lambda ()
                  ;; Set up evil-mode to play nice with smart-tabs-mode
                  (unless smart-tabs-respect-evil-customizations
                    (setq-local evil-shift-width tab-width)
                    (setq-local evil-indent-convert-tabs nil))))
      ;; Run default insinuations
      (dolist (lang smart-tabs-default-insinuations)
        (smart-tabs-insinuate lang)
        (add-hook (car (smart-tabs-get-standard-language lang))
                  'smart-tabs/enable-tabs-mode)))))

;;; packages.el ends here
