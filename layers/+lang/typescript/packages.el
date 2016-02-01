;;; packages.el --- typescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Bowdon <c.bowdon@bath.edu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq typescript-packages '(tide
                            flycheck-typescript-tslint
                            web-mode))

(defun typescript/init-tide ()
  (use-package tide
    :defer t
    :commands (typescript/jump-to-type-def)
    :init (progn
            (evilified-state-evilify tide-references-mode tide-references-mode-map
              (kbd "C-j") 'tide-find-previous-reference
              (kbd "C-k") 'tide-find-next-reference
              (kbd "C-l") 'tide-goto-reference)

            (add-hook 'typescript-mode-hook
                      (lambda ()
                      (tide-setup)
                      (flycheck-mode t)
                      (setq flycheck-check-syntax-automatically '(save mode-enabled))
                      (eldoc-mode t)
                      (when (configuration-layer/package-usedp 'company)
                            (company-mode-on)))))
    :config (progn

              (when typescript-use-tslint
                    (use-package flycheck-typescript-tslint)
                    (flycheck-add-next-checker 'typescript-tide
                                               'typescript-tslint 'append))

              (spacemacs/declare-prefix-for-mode 'typescript-mode "mg" "goto")
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mh" "help")
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mn" "name")

              (defun typescript/jump-to-type-def()
                (interactive)
                (tide-jump-to-definition t))

              (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
                "gb" 'tide-jump-back
                "gg" 'tide-jump-to-definition
                "gt" 'typescript/jump-to-type-def
                "gu"  'tide-references
                "hh" 'tide-documentation-at-point
                "rr" 'tide-rename-symbol
                "sr"  'tide-restart-server))))


(when (configuration-layer/package-usedp 'web-mode)
  (defun typescript/init-web-mode ()
   (use-package web-mode
   :defer t
   :mode ("\\.tsx\\'" . web-mode)
   :config (add-hook 'web-mode-hook
                     (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (tide-setup)
                         (flycheck-mode +1)
                         (setq flycheck-check-syntax-automatically '(save mode-enabled))
                         (eldoc-mode +1)
                         (when (configuration-layer/package-usedp 'company)
                               (company-mode-on))))))))

(when typescript-use-tslint
  (defun typescript/init-flycheck-typescript-tslint ()
    (use-package flycheck-typescript-tslint
    :defer t)))
