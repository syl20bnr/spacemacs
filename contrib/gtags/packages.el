;;; packages.el --- gtags Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar gtags-packages
  '(
    ;; package gtagss go here
    helm-gtags
    ggtags
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar gtags-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function gtags/init-<package-gtags>
;;
(defun gtags/init-ggtags ()
  "Initialize my package"
  (use-package ggtags
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook
                (lambda ()
                  ;; these modes have better eldoc integration
                  (unless (derived-mode-p 'c-mode
                                          'c++-mode
                                          'lisp-mode
                                          'emacs-lisp-mode
                                          'python-mode
                                          'ruby-mode)
                    (ggtags-mode 1)
                    (eldoc-mode 1)
                    (setq-local eldoc-documentation-function #'ggtags-eldoc-function)))))))

(defun gtags/init-helm-gtags ()
  (use-package helm-gtags
    :defer t
    :init
    (progn
      ;; add to Dired so we can select tag in Dired buffer
      ;; with helm-gtags-select
      ;; so is eshell
      (add-hook 'prog-mode-hook 'helm-gtags-mode)
      (setq helm-gtags-ignore-case t
            helm-gtags-auto-update t
            helm-gtags-use-input-at-cursor t
            helm-gtags-pulse-at-cursor t))
    :config
    (progn
      ;; if anyone uses helm-gtags, they would want to use these key bindings
      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key helm-gtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key helm-gtags-mode-map (kbd "M-*") 'pop-tag-mark)

      (let (l (if (eq dotspacemacs-editing-style 'emacs)
                  dotspacemacs-major-mode-emacs-leader-key
                dotspacemacs-major-mode-leader-key))
        (define-key helm-gtags-mode-map (kbd (concat l " gs")) 'helm-gtags-select)
        (define-key helm-gtags-mode-map (kbd (concat l " gs")) 'helm-gtags-resume)
        (define-key helm-gtags-mode-map (kbd (concat l " gR")) 'helm-gtags-dwim)
        (define-key helm-gtags-mode-map (kbd (concat l " gg")) 'helm-gtags-find-tag)
        (define-key helm-gtags-mode-map (kbd (concat l " gd")) 'helm-gtags-find-rtag)
        (define-key helm-gtags-mode-map (kbd (concat l " gr")) 'helm-gtags-tags-in-this-function)
        (define-key helm-gtags-mode-map (kbd (concat l " gi")) 'helm-gtags-create-tags)
        (define-key helm-gtags-mode-map (kbd (concat l " gc")) 'helm-gtags-parse-file)
        (define-key helm-gtags-mode-map (kbd (concat l " gl")) 'helm-gtags-show-stack)
        (define-key helm-gtags-mode-map (kbd (concat l " gS")) 'helm-gtags-update-tags)
        (define-key helm-gtags-mode-map (kbd (concat l " gu")) 'helm-gtags-next-history)
        (define-key helm-gtags-mode-map (kbd (concat l " gn")) 'helm-gtags-previous-history)
        (define-key helm-gtags-mode-map (kbd (concat l " gp")) 'helm-gtags-select-path)))
    ;; Often the body of an initialize function uses `use-package'
    ;; For more info on `use-package', see readme:
    ;; https://github.com/jwiegley/use-package
    ))
