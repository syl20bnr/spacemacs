;;; packages.el --- Spacemacs Layouts Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-layouts-packages
      '(eyebrowse
        helm
        ivy
        persp-mode
        spaceline
        (counsel-projectile :requires ivy)))



(defun spacemacs-layouts/init-eyebrowse ()
  (use-package eyebrowse
    :init
    (progn
      (setq eyebrowse-wrap-around t)
      (eyebrowse-mode)
      ;; transient state
      (spacemacs|transient-state-format-hint workspaces
        spacemacs--workspaces-ts-full-hint
        "\n
 Go to^^^^^^                         Actions^^^^
 ─────^^^^^^───────────────────────  ───────^^^^───────────────────────
 [_0_.._9_]^^     nth/new workspace  [_c_/_C_] clone workspace
 [_C-0_.._C-9_]^^ nth/new workspace  [_s_/_S_] single window workspace
 [_<tab>_]^^^^    last workspace     [_d_]^^   close current workspace
 [_n_/_C-l_]^^    next workspace     [_R_]^^   rename current workspace
 [_N_/_p_/_C-h_]  prev workspace     [_?_]^^   toggle help
 [_w_]^^^^        another workspace
 [_l_]^^^^        layouts TS")

      (spacemacs|define-transient-state workspaces
        :title "Workspaces Transient State"
        :hint-is-doc t
        :dynamic-hint (spacemacs//workspaces-ts-hint)
        :bindings
        ("?" spacemacs//workspaces-ts-toggle-hint)
        ("0" eyebrowse-switch-to-window-config-0 :exit t)
        ("1" eyebrowse-switch-to-window-config-1 :exit t)
        ("2" eyebrowse-switch-to-window-config-2 :exit t)
        ("3" eyebrowse-switch-to-window-config-3 :exit t)
        ("4" eyebrowse-switch-to-window-config-4 :exit t)
        ("5" eyebrowse-switch-to-window-config-5 :exit t)
        ("6" eyebrowse-switch-to-window-config-6 :exit t)
        ("7" eyebrowse-switch-to-window-config-7 :exit t)
        ("8" eyebrowse-switch-to-window-config-8 :exit t)
        ("9" eyebrowse-switch-to-window-config-9 :exit t)
        ("C-0" eyebrowse-switch-to-window-config-0)
        ("C-1" eyebrowse-switch-to-window-config-1)
        ("C-2" eyebrowse-switch-to-window-config-2)
        ("C-3" eyebrowse-switch-to-window-config-3)
        ("C-4" eyebrowse-switch-to-window-config-4)
        ("C-5" eyebrowse-switch-to-window-config-5)
        ("C-6" eyebrowse-switch-to-window-config-6)
        ("C-7" eyebrowse-switch-to-window-config-7)
        ("C-8" eyebrowse-switch-to-window-config-8)
        ("C-9" eyebrowse-switch-to-window-config-9)
        ("<tab>" eyebrowse-last-window-config)
        ("<return>" nil :exit t)
        ("TAB" eyebrowse-last-window-config)
        ("RET" nil :exit t)
        ("c" eyebrowse-create-window-config :exit t)
        ("C" eyebrowse-create-window-config)
        ("C-h" eyebrowse-prev-window-config)
        ("C-l" eyebrowse-next-window-config)
        ("d" eyebrowse-close-window-config)
        ("l" spacemacs/layouts-transient-state/body :exit t)
        ("n" eyebrowse-next-window-config)
        ("N" eyebrowse-prev-window-config)
        ("p" eyebrowse-prev-window-config)
        ("R" spacemacs/workspaces-ts-rename :exit t)
        ("s" spacemacs/single-win-workspace :exit t)
        ("S" spacemacs/single-win-workspace)
        ("w" eyebrowse-switch-to-window-config :exit t))
      ;; note: we don't need to declare the `SPC l w' binding, it is
      ;; declare in the layout transient state
      (spacemacs/set-leader-keys "bW" 'spacemacs/goto-buffer-workspace)
      ;; hooks
      (when (configuration-layer/package-used-p 'persp-mode)
        (add-hook 'persp-before-switch-functions
                  #'spacemacs/update-eyebrowse-for-perspective)
        (add-hook 'eyebrowse-post-window-switch-hook
                  #'spacemacs/save-eyebrowse-for-perspective)
        (add-hook 'persp-activated-functions
                  #'spacemacs/load-eyebrowse-for-perspective)
        (add-hook 'persp-before-save-state-to-file-functions
                  #'spacemacs/update-eyebrowse-for-perspective)
        (add-hook 'persp-after-load-state-functions
                  #'spacemacs/load-eyebrowse-after-loading-layout))
      ;; vim-style tab switching
      (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
      (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config))))



(defun spacemacs-layouts/post-init-helm ()
  (with-eval-after-load 'helm (spacemacs//persp-helm-setup))
  (spacemacs/set-leader-keys
    "bB" 'spacemacs-layouts/non-restricted-buffer-list-helm
    "pl" 'spacemacs/helm-persp-switch-project))



(defun spacemacs-layouts/post-init-ivy ()
  (spacemacs/set-leader-keys
    "bB" 'spacemacs-layouts/non-restricted-buffer-list-ivy))



(defun spacemacs-layouts/init-persp-mode ()
  (use-package persp-mode
    :init
    (progn
      (setq persp-add-buffer-on-after-change-major-mode 'free
            persp-auto-resume-time (if (or dotspacemacs-auto-resume-layouts
                                           spacemacs-force-resume-layouts)
                                       1 -1)
            persp-is-ibc-as-f-supported nil
            persp-nil-name dotspacemacs-default-layout-name
            persp-reset-windows-on-nil-window-conf nil
            persp-set-last-persp-for-new-frames nil
            persp-save-dir spacemacs-layouts-directory
            persp-set-ido-hooks t)

      (spacemacs/defer-until-after-user-config #'spacemacs//activate-persp-mode)

      ;; layouts transient state
      (spacemacs|transient-state-format-hint layouts
        spacemacs--layouts-ts-full-hint
        "\n
 Go to^^^^^^                        Actions^^^^
 ─────^^^^^^──────────────────────  ───────^^^^───────────────────────────────
 [_0_.._9_]^^     nth/new layout    [_a_]^^   add buffer
 [_C-0_.._C-9_]^^ nth/new layout    [_A_]^^   add all buffers from layout
 [_<tab>_]^^^^    last layout       [_d_]^^   close current layout
 [_n_/_C-l_]^^    next layout       [_D_]^^   close other layout
 [_N_/_p_/_C-h_]  prev layout       [_L_]^^   load layouts from file
 [_b_]^^^^        buffer in layout  [_r_]^^   remove current buffer
 [_h_]^^^^        default layout    [_R_]^^   rename current layout
 [_l_]^^^^        another layout    [_s_/_S_] save all layouts/save by names
 [_o_]^^^^        custom layout     [_t_]^^   show buffer w/o adding to layout
 [_w_]^^^^        workspaces TS     [_x_]^^   kill current w/buffers
 [_e_]^^^^        select layout     [_X_]^^   kill other w/buffers
 ^^^^^^                             [_<_/_>_] move layout left/right
 ^^^^^^                             [_?_]^^   toggle help")

      (spacemacs|define-transient-state layouts
        :title "Layouts Transient State"
        :hint-is-doc t
        :dynamic-hint (spacemacs//layouts-ts-hint)
        :bindings
        ;; need to exit in case number doesn't exist
        ("?" spacemacs//layouts-ts-toggle-hint)
        ("1" spacemacs/persp-switch-to-1 :exit t)
        ("2" spacemacs/persp-switch-to-2 :exit t)
        ("3" spacemacs/persp-switch-to-3 :exit t)
        ("4" spacemacs/persp-switch-to-4 :exit t)
        ("5" spacemacs/persp-switch-to-5 :exit t)
        ("6" spacemacs/persp-switch-to-6 :exit t)
        ("7" spacemacs/persp-switch-to-7 :exit t)
        ("8" spacemacs/persp-switch-to-8 :exit t)
        ("9" spacemacs/persp-switch-to-9 :exit t)
        ("0" spacemacs/persp-switch-to-0 :exit t)
        ("e" spacemacs/layout-switch-to :exit t)
        ("C-1" spacemacs/persp-switch-to-1)
        ("C-2" spacemacs/persp-switch-to-2)
        ("C-3" spacemacs/persp-switch-to-3)
        ("C-4" spacemacs/persp-switch-to-4)
        ("C-5" spacemacs/persp-switch-to-5)
        ("C-6" spacemacs/persp-switch-to-6)
        ("C-7" spacemacs/persp-switch-to-7)
        ("C-8" spacemacs/persp-switch-to-8)
        ("C-9" spacemacs/persp-switch-to-9)
        ("C-0" spacemacs/persp-switch-to-0)
        ("<tab>" spacemacs/jump-to-last-layout :exit t)
        ("<return>" nil :exit t)
        ("TAB" spacemacs/jump-to-last-layout)
        ("RET" nil :exit t)
        ("C-h" persp-prev)
        ("C-l" persp-next)
        ("<" spacemacs/move-current-persp-left)
        (">" spacemacs/move-current-persp-right)
        ("a" persp-add-buffer :exit t)
        ("A" persp-import-buffers :exit t)
        ("b" spacemacs/persp-buffers :exit t)
        ("d" spacemacs/layouts-ts-close)
        ("D" spacemacs/layouts-ts-close-other :exit t)
        ("h" spacemacs/layout-goto-default :exit t)
        ("L" persp-load-state-from-file :exit t)
        ("l" spacemacs/persp-perspectives :exit t)
        ("n" persp-next)
        ("N" persp-prev)
        ("o" spacemacs/select-custom-layout :exit t)
        ("p" persp-prev)
        ("r" persp-remove-buffer :exit t)
        ("R" spacemacs/layouts-ts-rename :exit t)
        ("s" persp-save-state-to-file :exit t)
        ("S" persp-save-to-file-by-names :exit t)
        ("t" persp-temporarily-display-buffer :exit t)
        ("w" spacemacs/workspaces-transient-state/body :exit t)
        ("x" spacemacs/layouts-ts-kill)
        ("X" spacemacs/layouts-ts-kill-other :exit t))
      (spacemacs/set-leader-keys "l" 'spacemacs/layouts-transient-state/body)
      ;; custom layouts
      (spacemacs|define-custom-layout "@Spacemacs"
        :binding "e"
        :body
        (spacemacs/find-dotfile)))
    :config
    (progn
      (spacemacs|hide-lighter persp-mode)
      (defadvice persp-activate (before spacemacs//save-toggle-layout activate)
        (setq spacemacs--last-selected-layout persp-last-persp-name))
      (add-hook 'persp-mode-hook 'spacemacs//layout-autosave)
      (advice-add 'persp-load-state-from-file
                  :before 'spacemacs//layout-wait-for-modeline)
      (when layouts-enable-local-variables
        (advice-add 'persp-switch :before #'spacemacs//load-layout-local-vars))
      (dolist (fn spacemacs-layouts-restricted-functions)
        (advice-add fn
                    :around 'spacemacs-layouts//advice-with-persp-buffer-list))
      (spacemacs/declare-prefix "b" "persp-buffers")
      (spacemacs/set-leader-keys
        "ba"   'persp-add-buffer
        "br"   'persp-remove-buffer))))



(defun spacemacs-layouts/post-init-spaceline ()
  (setq spaceline-display-default-perspective
        dotspacemacs-display-default-layout))



(defun spacemacs-layouts/init-counsel-projectile ()
  (use-package counsel-projectile
    :defer t
    :init (spacemacs/set-leader-keys "pl" 'spacemacs/ivy-persp-switch-project)
    :config (ivy-set-actions
             'spacemacs/ivy-persp-switch-project
             '(("d" spacemacs/ivy-switch-project-open-dired "dired")))))
