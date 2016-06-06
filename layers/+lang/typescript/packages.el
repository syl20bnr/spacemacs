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

(setq typescript-packages
      '(
        tide
        typescript-mode
        web-mode
        ))

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
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mg" "goto")
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mh" "help")
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mn" "name")
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mr" "rename")
              (spacemacs/declare-prefix-for-mode 'typescript-mode "mS" "server")
              (spacemacs/declare-prefix-for-mode 'typescript-mode "ms" "send")

              (defun typescript/jump-to-type-def()
                (interactive)
                (tide-jump-to-definition t))

              (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
                "gb" 'tide-jump-back
                "gg" 'tide-jump-to-definition
                "gt" 'typescript/jump-to-type-def
                "gu" 'tide-references
                "hh" 'tide-documentation-at-point
                "rr" 'tide-rename-symbol
                "Sr" 'tide-restart-server))))

(defun typescript/post-init-web-mode ()
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (tide-setup)
                (flycheck-mode +1)
                (setq flycheck-check-syntax-automatically '(save mode-enabled))
                (eldoc-mode +1)
                (when (configuration-layer/package-usedp 'company)
                  (company-mode-on))))))

(defun typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :config (progn
              (when typescript-fmt-on-save
                (add-hook 'typescript-mode-hook 'typescript/fmt-before-save-hook))
              (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
                "="  'typescript/format
                "sp" 'typescript/open-region-in-playground))))
