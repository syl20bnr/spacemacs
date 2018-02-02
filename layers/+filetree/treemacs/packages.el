;;; packages.el --- treemacs Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Alexander Miller <alexanderm@web.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst treemacs-packages
  '(golden-ratio
    treemacs
    (treemacs-evil :toggle (memq dotspacemacs-editing-style '(vim hybrid)))
    treemacs-projectile
    winum))

(defun treemacs/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-regexp
                 (rx "*Treemacs" (0+ any)))))

(defun treemacs/init-treemacs ()
  (use-package treemacs
    :commands (treemacs-select-window treemacs--window-number-ten)
    :defer t
    :init
    (spacemacs/set-leader-keys
      "ft"    #'treemacs-toggle
      "fT"    #'treemacs
      "fB"    #'treemacs-bookmark
      "f C-t" #'treemacs-find-file)
    :config
    (progn
      (spacemacs/define-evil-state-face "treemacs" "MediumPurple1")
      (setq treemacs-follow-after-init t
            treemacs-width 35
            treemacs-position 'left
            treemacs-is-never-other-window nil
            treemacs-silent-refresh nil
            treemacs-indentation 2
            treemacs-change-root-without-asking nil
            treemacs-sorting 'alphabetic-desc
            treemacs-show-hidden-files t
            treemacs-never-persist nil
            treemacs-goto-tag-strategy 'refetch-index
            treemacs-collapse-dirs treemacs-use-collapsed-directories)

      (when treemacs-use-follow-mode
        (treemacs-follow-mode t))

      (when treemacs-use-filewatch-mode
        (treemacs-filewatch-mode t))

      ;; this boundp check guards against a new feature that not all treemacs installations will have
      ;; TODO remove this guard in a few weeks
      (when (boundp 'treemacs-git-mode)
        (when (memq treemacs-use-git-mode '(simple extended))
          (treemacs-git-mode treemacs-use-git-mode))))))

(defun treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :after treemacs
    :if (memq dotspacemacs-editing-style '(vim hybrid))))

(defun treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :defer t
    :init
    (spacemacs/set-leader-keys
      "fp" #'treemacs-projectile-toggle
      "fP" #'treemacs-projectile)))

(defun treemacs/pre-init-winum ()
  (spacemacs|use-package-add-hook winum
    :post-config
    (progn
      ;; window 0 is reserved for file trees
      (spacemacs/set-leader-keys "0" #'treemacs-select-window)
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))))
