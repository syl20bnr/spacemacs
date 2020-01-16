;;; packages.el --- treemacs Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Alexander Miller <alexanderm@web.de>
;;         Hong Xu <hong@topbug.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst treemacs-packages
  '(
    golden-ratio
    treemacs
    (treemacs-evil :toggle (memq dotspacemacs-editing-style '(vim hybrid)))
    (treemacs-magit :requires magit)
    (treemacs-persp :requires persp-mode)
    treemacs-projectile
    winum
    ))

(defun treemacs/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-regexp
                 (rx "*Treemacs" (0+ any)))))

(defun treemacs/init-treemacs ()
  (use-package treemacs
    :commands (treemacs-select-window
               treemacs-select-scope-type
               treemacs--window-number-ten
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
            treemacs-goto-tag-strategy 'refetch-index)
      (add-hook 'treemacs-mode-hook
                #'spacemacs/treemacs-setup-width-lock)
      (spacemacs/set-leader-keys
        "ft"    'treemacs
        "fB"    'treemacs-bookmark
        "fT"    'treemacs-find-file
        "f M-t" 'treemacs-find-tag
        "pt"    'spacemacs/treemacs-project-toggle)
      (which-key-add-major-mode-key-based-replacements 'treemacs-mode
        "c"         "treemacs-create"
        "o"         "treemacs-visit-node"
        "oa"        "treemacs-visit-node-ace"
        "t"         "treemacs-toggles"
        "y"         "treemacs-copy"
        "C-c C-p"   "treemacs-projects"
        "C-c C-p c" "treemacs-projects-collapse"))
    :config
    (progn
      (spacemacs/define-evil-state-face "treemacs" "MediumPurple1")
      ;; minor modes are enabled by default, so they must be explicitly
      ;; turned off
      (if (eq treemacs-use-follow-mode t)
          (treemacs-follow-mode t)
        (treemacs-follow-mode -1))
      (if (eq treemacs-use-follow-mode 'tag)
          (treemacs-tag-follow-mode t)
        (treemacs-tag-follow-mode -1))
      (if treemacs-use-filewatch-mode
          (treemacs-filewatch-mode t)
        (treemacs-filewatch-mode -1))
      (if (memq treemacs-use-git-mode '(simple extended deferred))
          (treemacs-git-mode treemacs-use-git-mode)
        (treemacs-git-mode -1))
      (add-to-list 'spacemacs-window-split-ignore-prefixes
                   treemacs--buffer-name-prefix))))

(defun treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :after treemacs
    :if (memq dotspacemacs-editing-style '(vim hybrid))))

(defun treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :after treemacs
    :defer t
    :init (require 'treemacs-projectile)))

(defun treemacs/init-treemacs-persp ()
  (use-package treemacs-persp :after treemacs))

(defun treemacs/pre-init-winum ()
  (spacemacs|use-package-add-hook winum
    :post-config
    (progn
      ;; `0', `M-0' and `C-x w 0' are bound to `winum-select-window-0-or-10'
      (define-key winum-keymap
        [remap winum-select-window-0-or-10] #'treemacs-select-window)
      ;; replace the which-key name
      (push '((nil . "winum-select-window-0-or-10") .
              (nil . "treemacs-select-window"))
            which-key-replacement-alist)
      (with-eval-after-load 'treemacs
        (dolist (n (number-sequence 1 5))
          (add-to-list 'winum-ignored-buffers
                       (format "%sFramebuffer-%s*"
                               treemacs--buffer-name-prefix n)))))))

(defun treemacs/init-treemacs-magit ()
  (use-package treemacs-magit
    :after treemacs magit
    :defer t))
