;;; packages.el --- helm-gtags Layer packages File for Spacemacs
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

(defvar helm-gtags-packages
  '(
    ;; package helm-gtagss go here
    helm-gtags
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar helm-gtags-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function helm-gtags/init-<package-helm-gtags>
;;
(defun helm-gtags/init-helm-gtags ()
  "Initialize my package"
  (use-package helm-gtags
    :defer t
    :init
    (progn
      ;; add to Dired so we can select tag in Dired buffer
      ;; with helm-gtags-select
      ;; so is eshell
      (add-hook 'dired-mode-hook 'helm-gtags-mode)
      (add-hook 'eshell-mode-hook 'helm-gtags-mode)
      (add-hook 'c-mode-hook 'helm-gtags-mode)
      (add-hook 'c++-mode-hook 'helm-gtags-mode)
      (add-hook 'java-mode-hook 'helm-gtags-mode)
      (add-hook 'asm-mode-hook 'helm-gtags-mode)
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

      (dolist (m '(c-mode c++-mode java-mode asm-mode dired-mode eshell-mode term-mode shell-mode))
        (evil-leader/set-key-for-mode 'c++-mode
          "mgc" 'helm-gtags-create-tags           ; create a tag databas
          "mgd" 'helm-gtags-find-tag              ; find definitions
          "mgf" 'helm-gtags-select-path           ; jump to a file in tag database
          "mgg" 'helm-gtags-dwim                  ; jump to a location based on context
          "mgi" 'helm-gtags-tags-in-this-function ; present tags in current function only
          "mgl" 'helm-gtags-parse-file            ; jump to definitions in file
          "mgn" 'helm-gtags-next-history          ; jump to next location in context stack
          "mgp" 'helm-gtags-previous-history      ; jump to previous location in context stack
          "mgr" 'helm-gtags-find-rtag             ; find references
          "mgR" 'helm-gtags-resume                ; resume previous helm-gtags sesssion
          "mgs" 'helm-gtags-select                ; select any tag in a project retreived by gtags
          "mgS" 'helm-gtags-show-stack            ; show stack of visited locations
          "mgu" 'helm-gtags-update-tags           ; manually update tag database
          )))))
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
