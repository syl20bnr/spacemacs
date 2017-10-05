;;; packages.el --- treemacs Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Alexander Miller <alexanderm@web.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst treemacs-packages
  '(treemacs
    treemacs-projectile
    (treemacs-evil :toggle (memq dotspacemacs-editing-style '(vim hybrid)))
    golden-ratio
    winum))

(defun treemacs/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-regexp (rx "*Treemacs" (0+ any)))))

(defun treemacs/post-init-winum ()
  (spacemacs|use-package-add-hook winum
    :post-config
    (setq winum-assign-func #'treemacs--window-number-ten)))

(defun treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :defer t
    :init
    (spacemacs/set-leader-keys
      "fp" #'treemacs-projectile-toggle
      "fP" #'treemacs-projectile)))

(defun treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :after treemacs
    :if (memq dotspacemacs-editing-style '(vim hybrid))))

(defun treemacs/init-treemacs ()
  (use-package treemacs
    :defer t
    :init
    (spacemacs/set-leader-keys
      "ft"    #'treemacs-toggle
      "fT"    #'treemacs
      "fB"    #'treemacs-bookmark
      "f C-t" #'treemacs-find-file)
    :config
    (progn
      (setq treemacs-follow-after-init          t
            treemacs-width                      35
            treemacs-position                   'left
            treemacs-is-never-other-window      nil
            treemacs-silent-refresh             nil
            treemacs-indentation                2
            treemacs-git-integration            t
            treemacs-change-root-without-asking nil
            treemacs-sorting                    'alphabetic-desc
            treemacs-show-hidden-files          t
            treemacs-never-persist              nil
            treemacs-goto-tag-strategy          'refetch-index
            treemacs-collapse-dirs              treemacs-use-collapsed-directories)

      (when treemacs-use-follow-mode
        (treemacs-follow-mode t))

      (when treemacs-use-filewatch-mode
        (treemacs-filewatch-mode t)))))
