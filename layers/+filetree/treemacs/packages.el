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
    :commands (treemacs-select-window treemacs--window-number-ten
               treemacs-current-visibility)
    :defer t
    :init
    (progn
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
      (spacemacs/set-leader-keys
        "ft"    'treemacs
        "fB"    'treemacs-bookmark
        "fT"    'treemacs-find-file
        "f M-t" 'treemacs-find-tag
        "pt"    'spacemacs/treemacs-project-toggle)
      (which-key-add-major-mode-key-based-replacements 'treemacs-mode
        "c"     "treemacs-create"
        "o"     "treemacs-visit-node"
        "oa"    "treemacs-visit-node-ace"
        "t"     "treemacs-toggles"
        "y"     "treemacs-copy"
        "C-p"   "treemacs-projects"
        "C-p c" "treemacs-projects-collapse"))
    :config
    (progn
      (spacemacs/define-evil-state-face "treemacs" "MediumPurple1")
      (when treemacs-use-follow-mode
        (treemacs-follow-mode t))
      (when treemacs-use-filewatch-mode
        (treemacs-filewatch-mode t))
      (when (memq treemacs-use-git-mode '(simple extended))
        (treemacs-git-mode treemacs-use-git-mode))
      (add-to-list 'spacemacs-window-split-ignore-prefixes treemacs--buffer-name-prefix))))

(defun treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :after treemacs
    :if (memq dotspacemacs-editing-style '(vim hybrid))))

(defun treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :after treemacs
    :defer t))

(defun treemacs/pre-init-winum ()
  (spacemacs|use-package-add-hook winum
    :post-config
    (progn
      ;; window 0 is reserved for file trees
      (spacemacs/set-leader-keys "0" 'treemacs-select-window)
      (define-key winum-keymap (kbd "M-0") 'treemacs-select-window))))
