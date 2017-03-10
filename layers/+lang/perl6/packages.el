;;; packages.el --- perl6 layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  Bahtiar `kalkin-`''Gadimov <bahtiar@gadimov.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar perl6-packages
  '(company
    flycheck
    flycheck-perl6
    perl6-mode
    json-mode
    )
)

(defun perl6/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes perl6-mode))

(defun perl6/init-json-mode () (use-package json-mode :defer t))

(defun perl6/init-perl6-mode()
  (use-package perl6-mode
    :defer t
    :init
    (progn
      (add-to-list 'spacemacs-jump-handlers-perl6-mode 'evil-jump-to-tag)
      (add-to-list 'auto-mode-alist '("/perl6/site/sources/" . perl6-mode))
      (when (configuration-layer/package-usedp 'company)
        (push 'company-capf company-backends-perl6-mode)
        )
      (spacemacs|define-text-object "«" "double-angle-bracket" "«" "»")
      (spacemacs|define-text-object "｢" "corner-bracket" "｢" "｣")
      (spacemacs|define-text-object "‘" "single-quotation-mark" "‘" "’")
      (spacemacs|define-text-object "“" "double-quotation-mark" "“" "”")
      )
    )
  )

(defun perl6/init-flycheck-perl6 () (use-package flycheck-perl6 :ensure t :if
                                      (configuration-layer/package-usedp 'flycheck)) )

(defun perl6/post-init-flycheck () (spacemacs/add-flycheck-hook 'perl6-mode))

;;; packages.el ends here
