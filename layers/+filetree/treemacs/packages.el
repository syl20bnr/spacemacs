;;; packages.el --- Tree-macs Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Alexander Miller <alexanderm@web.de>
;;         Hong Xu <hong@topbug.net>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst tree-macs-packages
  '(
    golden-ratio
    tree-macs
    (tree-macs-evil :toggle (memq dotspace-macs-editing-style '(vim hybrid)))
    (tree-macs-icons-dired :toggle tree-macs-use-icons-dired)
    (tree-macs-all-the-icons :toggle tree-macs-use-all-the-icons-theme)
    (tree-macs-magit :requires magit)
    (tree-macs-persp :requires persp-mode)
    tree-macs-projectile
    winum
    ))

(defun tree-macs/pre-init-golden-ratio ()
  (space-macs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-regexp
                 (rx "*Tree-macs" (0+ any)))))

(defun tree-macs/init-tree-macs ()
  (use-package tree-macs
    :commands (tree-macs-select-window
               tree-macs-select-scope-type
               tree-macs--window-number-ten
               tree-macs-current-visibility)
    :defer t
    :init
    (progn
      (setq tree-macs-follow-after-init t)
      (add-hook 'tree-macs-mode-hook
                #'space-macs/tree-macs-setup-width-lock)
      (space-macs|spacebind
       "Files manipulation."
       :global
       (("f" "Files"
         ("t" tree-macs "File tree")
         ("B" tree-macs-bookmark "Find bookmark in file tree")
         ("T" tree-macs-find-file "Focus current file in file tree")
         ("M-t" tree-macs-find-tag "Focus tag in file tree" ))
        ("p" "Project"
         ("t" space-macs/tree-macs-project-toggle "Open project in file tree"))))
      (which-key-add-major-mode-key-based-replacements 'tree-macs-mode
        "c"         "tree-macs-create"
        "o"         "tree-macs-visit-node"
        "oa"        "tree-macs-visit-node-ace"
        "t"         "tree-macs-toggles"
        "y"         "tree-macs-copy"
        "C-c C-p"   "tree-macs-projects"
        "C-c C-p c" "tree-macs-projects-collapse"))
    :config
    (progn
      (space-macs/define-evil-state-face "tree-macs" "MediumPurple1")
      ;; minor modes are enabled by default, so they must be explicitly
      ;; turned off
      (if (eq tree-macs-use-follow-mode t)
          (tree-macs-follow-mode t)
        (tree-macs-follow-mode -1))
      (if (eq tree-macs-use-follow-mode 'tag)
          (tree-macs-tag-follow-mode t)
        (tree-macs-tag-follow-mode -1))
      (if tree-macs-use-filewatch-mode
          (tree-macs-filewatch-mode t)
        (tree-macs-filewatch-mode -1))
      (if (memq tree-macs-use-git-mode '(simple extended deferred))
          (tree-macs-git-mode tree-macs-use-git-mode)
        (tree-macs-git-mode -1))
      (add-to-list 'space-macs-window-split-ignore-prefixes
                   tree-macs--buffer-name-prefix))))

(defun tree-macs/init-tree-macs-evil ()
  (use-package tree-macs-evil
    :after tree-macs
    :if (memq dotspace-macs-editing-style '(vim hybrid))))

(defun tree-macs/init-tree-macs-projectile ()
  (use-package tree-macs-projectile
    :after tree-macs
    :defer t
    :init (require 'tree-macs-projectile)))

(defun tree-macs/init-tree-macs-persp ()
  (use-package tree-macs-persp
    :after tree-macs persp-mode
    :config (when (eq tree-macs-use-scope-type 'Perspectives)
              (tree-macs-set-scope-type 'Perspectives))))

(defun tree-macs/init-tree-macs-icons-dired ()
  (use-package tree-macs-icons-dired
    :hook (dired-mode . tree-macs-icons-dired-mode)))

(defun tree-macs/init-tree-macs-all-the-icons ()
  (use-package tree-macs-all-the-icons
    :if tree-macs-use-all-the-icons-theme
    :hook (tree-macs-mode . (lambda () (tree-macs-load-theme 'all-the-icons)))))

(defun tree-macs/pre-init-winum ()
  (space-macs|use-package-add-hook winum
    :post-config
    (progn
      ;; `0', `M-0' and `C-x w 0' are bound to `winum-select-window-0-or-10'
      (define-key winum-keymap
        [remap winum-select-window-0-or-10] #'tree-macs-select-window)
      ;; replace the which-key name
      (push '((nil . "winum-select-window-0-or-10") .
              (nil . "tree-macs-select-window"))
            which-key-replacement-alist)
      (with-eval-after-load 'tree-macs
        (dolist (n (number-sequence 1 5))
          (add-to-list 'winum-ignored-buffers
                       (format "%sFramebuffer-%s*"
                               tree-macs--buffer-name-prefix n)))))))

(defun tree-macs/init-tree-macs-magit ()
  (use-package tree-macs-magit
    :after tree-macs magit
    :defer t))


