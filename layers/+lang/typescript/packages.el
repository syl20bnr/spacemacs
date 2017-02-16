;;; packages.el --- typescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Chris Bowdon <c.bowdon@bath.edu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq typescript-packages
      '(
        add-node-modules-path
        company
        eldoc
        flycheck
        rainbow-identifiers
        tide
        typescript-mode
        web-mode
        ))

(defun typescript/post-init-add-node-modules-path ()
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook #'add-node-modules-path))

(defmacro typescript||iterate-modes (&rest body)
  `(progn
     ,@(loop for mode in (if typescript-use-web-mode-for-ts '(web-typescript-mode) '(typescript-mode web-typescript-mode))
             collect (let ((hook (intern (format "%S-hook" mode)))
                           (jump-handlers (intern (concat "spacemacs-jump-handlers-" (symbol-name mode)))))
                       `(progn
                          ,@(subst mode 'mode (subst hook 'hook (subst jump-handlers 'jump-handlers body))))))))

(defun typescript/post-init-company ()
  (when (configuration-layer/package-used-p 'tide)
    (typescript||iterate-modes
     (spacemacs|add-company-backends
       :backends company-tide
       :modes mode))))

(defun typescript/post-init-rainbow-identifiers ()
  (when (or (eq colors-colorize-identifiers 'all) (eq colors-colorize-identifiers 'variables))
    (add-to-list 'rainbow-identifiers-faces-to-override 'web-mode-variable-name-face)
    (typescript||iterate-modes
     (add-hook 'hook 'rainbow-identifiers-mode))))

(defun typescript/post-init-eldoc ()
  (typescript||iterate-modes
   (add-hook 'hook 'eldoc-mode)))

(defun typescript/post-init-flycheck ()
  (spacemacs|use-package-add-hook tide
    :post-config
    (typescript||iterate-modes
     (flycheck-add-mode 'typescript-tide 'mode)
     (flycheck-add-mode 'typescript-tslint 'mode)
     (spacemacs/enable-flycheck 'mode))))

(defun typescript/init-tide ()
  (use-package tide
    :defer t
    :commands (typescript/jump-to-type-def)
    :init
    (progn
      (evilified-state-evilify tide-references-mode tide-references-mode-map
        (kbd "C-k") 'tide-find-previous-reference
        (kbd "C-j") 'tide-find-next-reference
        (kbd "C-l") 'tide-goto-reference))
    :config
    (progn
      (defun typescript/jump-to-type-def()
        (interactive)
        (tide-jump-to-definition t))

      (typescript||iterate-modes
       (spacemacs/declare-prefix-for-mode 'mode "mg" "goto")
       (spacemacs/declare-prefix-for-mode 'mode "mh" "help")
       (spacemacs/declare-prefix-for-mode 'mode "mn" "name")
       (spacemacs/declare-prefix-for-mode 'mode "mr" "rename")
       (spacemacs/declare-prefix-for-mode 'mode "mS" "server")
       (spacemacs/declare-prefix-for-mode 'mode "ms" "send")

       (spacemacs/set-leader-keys-for-major-mode 'mode
         "gb" 'tide-jump-back
         "gt" 'typescript/jump-to-type-def
         "gu" 'tide-references
         "hh" 'tide-documentation-at-point
         "rr" 'tide-rename-symbol
         "Sr" 'tide-restart-server)

       (add-hook 'hook 'tide-setup)))))

(defun typescript/post-init-tide ()
  (setf (flycheck-checker-get 'typescript-tide 'modes) '(typescript-mode web-typescript-mode))
  (typescript||iterate-modes
   (spacemacs|define-jump-handlers mode)
   (add-to-list 'jump-handlers 'tide-jump-to-definition)))

(defun typescript--prettify-symbols ()
  (push '("function" . ?λ) prettify-symbols-alist)
  (push '("React.createElement" . ?Ʀ) prettify-symbols-alist)
  (push '("return" . ?⏎) prettify-symbols-alist)
  (push '("async." . ?Թ) prettify-symbols-alist)
  (push '("this.props.store." . ?Σ) prettify-symbols-alist)

  (prettify-symbols-mode))

;; we need to (re)use the init-web-mode here as our mode needs to be defined before the misc post-init functions
(defun typescript/init-web-mode ()
  (define-derived-mode web-typescript-mode web-mode "web-typescript")
  (typescript||iterate-modes
   (add-hook 'hook 'typescript--prettify-symbols)
   (add-hook 'hook (lambda () (yas-minor-mode -1)))
   (when typescript-fmt-on-save
     (add-hook 'hook 'typescript/fmt-before-save-hook))
   (spacemacs/set-leader-keys-for-major-mode 'mode
     "="  'typescript/format
     "sp" 'typescript/open-region-in-playground)))

(defun typescript/post-init-web-mode ()
  (when typescript-use-web-mode-for-ts
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-typescript-mode)))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-typescript-mode)))

(defun typescript/init-typescript-mode ()
  (use-package typescript-mode
    :defer t))
