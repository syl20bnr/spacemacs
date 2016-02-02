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

(setq typescript-packages '(flycheck-typescript-tslint
                            tide
                            typescript-mode
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

(defun typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t
    :commands (typescript/format-buffer)
    :config (progn
              (defun typescript/format-buffer ()
                "Format buffer with tsfmt."
                (interactive)
                (if (executable-find "tsfmt")
                    (let*  ((tmpfile (make-temp-file "~fmt-tmp" nil ".ts"))
                            (coding-system-for-read 'utf-8)
                            (coding-system-for-write 'utf-8)
                            (outputbuf (get-buffer-create "*~fmt-tmp.ts*")))
                      (unwind-protect
                          (progn
                            (with-current-buffer outputbuf (erase-buffer))
                            (write-region nil nil tmpfile)
                            (if (zerop (apply 'call-process "tsfmt" nil outputbuf nil (list tmpfile)))
                                (let ((p (point)))
                                  (save-excursion
                                    (with-current-buffer (current-buffer)
                                      (erase-buffer)
                                      (insert-buffer-substring outputbuf)))
                                  (goto-char p)
                                  (message "formatted.")
                                  (kill-buffer outputbuf))
                                (progn 
                                  (message "Formatting failed!")
                                  (display-buffer outputbuf)))
                            (progn
                              (delete-file tmpfile)))))
                  (message "tsfmt not found. Run \"npm install -g typescript-formatter\"")))

              (spacemacs/set-leader-keys-for-major-mode 'typescript-mode "=" 'typescript/format-buffer)

              (when typescript-fmt-on-save

                    (defun typescript/before-save-hook ()
                      (add-hook 'before-save-hook 'typescript/format-buffer t t))

                    (add-hook 'typescript-mode-hook 'typescript/before-save-hook)))))

(when typescript-use-tslint
  (defun typescript/init-flycheck-typescript-tslint ()
    (use-package flycheck-typescript-tslint
    :defer t)))
