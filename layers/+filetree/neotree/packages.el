;;; packages.el --- Neotree Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst neotree-packages
  '(
    neotree
    winum
    ))

(defun neotree/init-neotree ()
  (use-package neotree
    :defer t
    :commands neo-global--window-exists-p
    :init
    (progn
      (setq neo-window-width 32
            neo-create-file-auto-open t
            neo-banner-message "Press ? for neotree help"
            neo-show-updir-line nil
            neo-mode-line-type 'neotree
            neo-smart-open t
            neo-dont-be-alone t
            neo-persist-show nil
            neo-show-hidden-files t
            neo-auto-indent-point t
            neo-modern-sidebar t
            neo-vc-integration nil)

      (when (eq 'darwin system-type)
       (setq neo-default-system-application "open"))

      (spacemacs|define-transient-state neotree
        :title "NeoTree Key Hints"
        :doc "
Navigation^^^^             Actions^^         Visual actions/config^^^
───────^^^^─────────────── ───────^^──────── ───────^^^────────────────
[_L_]   next sibling^^     [_c_] create      [_TAB_] shrink/enlarge
[_H_]   previous sibling^^ [_C_] copy        [_|_]   vertical split
[_J_]   goto child^^       [_d_] delete      [_-_]   horizontal split
[_K_]   goto parent^^      [_r_] rename      [_gr_]  refresh^
[_l_]   open/expand^^      [_R_] change root [_s_]   hidden:^^^ %s(if neo-buffer--show-hidden-file-p \"on\" \"off\")
[_h_]   up/collapse^^      ^^                ^^^
[_j_]   line down^^        ^^                ^^^
[_k_]   line up^^          ^^                ^^
[_'_]   quick look         ^^                ^^
[_RET_] open               ^^^^              [_?_]   close hints
"
        :bindings
        ("RET" spacemacs/neotree-expand-or-open)
        ("TAB" neotree-stretch-toggle)
        ("|" neotree-enter-vertical-split)
        ("-" neotree-enter-horizontal-split)
        ("?" nil :exit t)
        ("'" neotree-quick-look)
        ("c" neotree-create-node)
        ("C" neotree-copy-node)
        ("d" neotree-delete-node)
        ("gr" neotree-refresh)
        ("h" spacemacs/neotree-collapse-or-up)
        ("H" neotree-select-previous-sibling-node)
        ("j" neotree-next-line)
        ("J" neotree-select-down-node)
        ("k" neotree-previous-line)
        ("K" neotree-select-up-node)
        ("l" spacemacs/neotree-expand-or-open)
        ("L" neotree-select-next-sibling-node)
        ("r" neotree-rename-node)
        ("R" neotree-change-root)
        ("s" neotree-hidden-file-toggle))

      (defun spacemacs//neotree-key-bindings ()
        "Set the key bindings for a neotree buffer."
        (evilified-state-evilify-map neotree-mode-map
          :mode neotree-mode
          :bindings
          (kbd "TAB")  'neotree-stretch-toggle
          (kbd "RET") 'spacemacs/neotree-expand-or-open
          (kbd "|") 'neotree-enter-vertical-split
          (kbd "-") 'neotree-enter-horizontal-split
          (kbd "'") 'neotree-quick-look
          (kbd "c") 'neotree-create-node
          (kbd "C") 'neotree-copy-node
          (kbd "d") 'neotree-delete-node
          (kbd "gr") 'neotree-refresh
          (kbd "h") 'spacemacs/neotree-collapse-or-up
          (kbd "H") 'neotree-select-previous-sibling-node
          (kbd "j") 'neotree-next-line
          (kbd "J") 'neotree-select-down-node
          (kbd "k") 'neotree-previous-line
          (kbd "K") 'neotree-select-up-node
          (kbd "l") 'spacemacs/neotree-expand-or-open
          (kbd "L") 'neotree-select-next-sibling-node
          (kbd "q") 'neotree-hide
          (kbd "r") 'neotree-rename-node
          (kbd "R") 'neotree-change-root
          (kbd "?") 'spacemacs/neotree-transient-state/body
          (kbd "s") 'neotree-hidden-file-toggle))

      (spacemacs/set-leader-keys
        "ft" 'neotree-toggle
        "fT" 'neotree-show
        "pt" 'neotree-find-project-root))
    :config
    (progn
      (spacemacs//neotree-key-bindings)
      (add-to-list 'spacemacs-window-split-ignore-prefixes neo-buffer-name))))

(defun neotree/pre-init-winum ()
  (spacemacs|use-package-add-hook winum
    :post-config
    ;; window 0 is reserved for file trees
    (spacemacs/set-leader-keys "0" #'neotree-show)
    (define-key winum-keymap (kbd "M-0") #'neotree-show)
    (add-to-list 'winum-assign-functions #'spacemacs//winum-neotree-assign-func)))
