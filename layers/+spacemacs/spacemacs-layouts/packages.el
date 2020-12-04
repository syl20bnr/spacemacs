;;; packages.el --- Space-macs Layouts Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-layouts-packages
      '(eyebrowse
        helm
        ivy
        persp-mode
        spaceline
        (counsel-projectile :requires ivy)))



(defun space-macs-layouts/init-eyebrowse ()
  (use-package eyebrowse
    :init
    (progn
      (setq eyebrowse-wrap-around t)
      (eyebrowse-mode)
      ;; transient state
      (space-macs|transient-state-format-hint workspaces
        space-macs--workspaces-ts-full-hint
        "\n
 Go to^^^^^^                         Actions^^^^
 â”€â”€â”€â”€â”€^^^^^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€â”€â”€^^^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
 [_0_.._9_]^^     nth/new workspace  [_c_/_C_] clone workspace
 [_C-0_.._C-9_]^^ nth/new workspace  [_s_/_S_] single window workspace
 [_<tab>_]^^^^    last workspace     [_d_]^^   close current workspace
 [_n_/_C-l_]^^    next workspace     [_R_]^^   rename current workspace
 [_N_/_p_/_C-h_]  prev workspace     [_?_]^^   toggle help
 [_w_]^^^^        another workspace
 [_l_]^^^^        layouts TS")

      (space-macs|define-transient-state workspaces
        :title "Workspaces Transient State"
        :hint-is-doc t
        :dynamic-hint (space-macs//workspaces-ts-hint)
        :bindings
        ("?" space-macs//workspaces-ts-toggle-hint)
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
        ("l" space-macs/layouts-transient-state/body :exit t)
        ("n" eyebrowse-next-window-config)
        ("N" eyebrowse-prev-window-config)
        ("p" eyebrowse-prev-window-config)
        ("R" space-macs/workspaces-ts-rename :exit t)
        ("s" space-macs/single-win-workspace :exit t)
        ("S" space-macs/single-win-workspace)
        ("w" eyebrowse-switch-to-window-config :exit t))
      ;; note: we don't need to declare the `SPC l w' binding, it is
      ;; declare in the layout transient state
      (space-macs/set-leader-keys "bW" 'space-macs/goto-buffer-workspace)
      ;; hooks
      (when (configuration-layer/package-used-p 'persp-mode)
        (add-hook 'persp-before-switch-functions
                  #'space-macs/update-eyebrowse-for-perspective)
        (add-hook 'eyebrowse-post-window-switch-hook
                  #'space-macs/save-eyebrowse-for-perspective)
        (add-hook 'persp-activated-functions
                  #'space-macs/load-eyebrowse-for-perspective)
        (add-hook 'persp-before-save-state-to-file-functions
                  #'space-macs/update-eyebrowse-for-perspective)
        (add-hook 'persp-after-load-state-functions
                  #'space-macs/load-eyebrowse-after-loading-layout))
      ;; vim-style tab switching
      (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
      (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config))))



(defun space-macs-layouts/post-init-helm ()
  (with-eval-after-load 'helm (space-macs//persp-helm-setup))
  (space-macs/set-leader-keys
    "bB" 'space-macs-layouts/non-restricted-buffer-list-helm
    "pl" 'space-macs/helm-persp-switch-project))



(defun space-macs-layouts/post-init-ivy ()
  (space-macs/set-leader-keys
    "bB" 'space-macs-layouts/non-restricted-buffer-list-ivy))



(defun space-macs-layouts/init-persp-mode ()
  (use-package persp-mode
    :init
    (progn
      (setq persp-add-buffer-on-after-change-major-mode 'free
            persp-auto-resume-time (if (or dotspace-macs-auto-resume-layouts
                                           space-macs-force-resume-layouts)
                                       1 -1)
            persp-is-ibc-as-f-supported nil
            persp-nil-name dotspace-macs-default-layout-name
            persp-reset-windows-on-nil-window-conf nil
            persp-set-last-persp-for-new-frames nil
            persp-save-dir space-macs-layouts-directory
            persp-set-ido-hooks t)

      (space-macs/defer-until-after-user-config #'space-macs//activate-persp-mode)

      ;; layouts transient state
      (space-macs|transient-state-format-hint layouts
        space-macs--layouts-ts-full-hint
        "\n
 Go to^^^^^^                        Actions^^^^
 â”€â”€â”€â”€â”€^^^^^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€â”€â”€^^^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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

      (space-macs|define-transient-state layouts
        :title "Layouts Transient State"
        :hint-is-doc t
        :dynamic-hint (space-macs//layouts-ts-hint)
        :bindings
        ;; need to exit in case number doesn't exist
        ("?" space-macs//layouts-ts-toggle-hint)
        ("1" space-macs/persp-switch-to-1 :exit t)
        ("2" space-macs/persp-switch-to-2 :exit t)
        ("3" space-macs/persp-switch-to-3 :exit t)
        ("4" space-macs/persp-switch-to-4 :exit t)
        ("5" space-macs/persp-switch-to-5 :exit t)
        ("6" space-macs/persp-switch-to-6 :exit t)
        ("7" space-macs/persp-switch-to-7 :exit t)
        ("8" space-macs/persp-switch-to-8 :exit t)
        ("9" space-macs/persp-switch-to-9 :exit t)
        ("0" space-macs/persp-switch-to-0 :exit t)
        ("e" space-macs/layout-switch-to :exit t)
        ("C-1" space-macs/persp-switch-to-1)
        ("C-2" space-macs/persp-switch-to-2)
        ("C-3" space-macs/persp-switch-to-3)
        ("C-4" space-macs/persp-switch-to-4)
        ("C-5" space-macs/persp-switch-to-5)
        ("C-6" space-macs/persp-switch-to-6)
        ("C-7" space-macs/persp-switch-to-7)
        ("C-8" space-macs/persp-switch-to-8)
        ("C-9" space-macs/persp-switch-to-9)
        ("C-0" space-macs/persp-switch-to-0)
        ("<tab>" space-macs/jump-to-last-layout :exit t)
        ("<return>" nil :exit t)
        ("TAB" space-macs/jump-to-last-layout)
        ("RET" nil :exit t)
        ("C-h" persp-prev)
        ("C-l" persp-next)
        ("<" space-macs/move-current-persp-left)
        (">" space-macs/move-current-persp-right)
        ("a" persp-add-buffer :exit t)
        ("A" persp-import-buffers :exit t)
        ("b" space-macs/persp-buffers :exit t)
        ("d" space-macs/layouts-ts-close)
        ("D" space-macs/layouts-ts-close-other :exit t)
        ("h" space-macs/layout-goto-default :exit t)
        ("L" persp-load-state-from-file :exit t)
        ("l" space-macs/persp-perspectives :exit t)
        ("n" persp-next)
        ("N" persp-prev)
        ("o" space-macs/select-custom-layout :exit t)
        ("p" persp-prev)
        ("r" persp-remove-buffer :exit t)
        ("R" space-macs/layouts-ts-rename :exit t)
        ("s" persp-save-state-to-file :exit t)
        ("S" persp-save-to-file-by-names :exit t)
        ("t" persp-temporarily-display-buffer :exit t)
        ("w" space-macs/workspaces-transient-state/body :exit t)
        ("x" space-macs/layouts-ts-kill)
        ("X" space-macs/layouts-ts-kill-other :exit t))
      (space-macs/set-leader-keys "l" 'space-macs/layouts-transient-state/body)
      ;; custom layouts
      (space-macs|define-custom-layout "@Space-macs"
        :binding "e"
        :body
        (space-macs/find-dotfile)))
    :config
    (progn
      (space-macs|hide-lighter persp-mode)
      (defadvice persp-activate (before space-macs//save-toggle-layout activate)
        (setq space-macs--last-selected-layout persp-last-persp-name))
      (add-hook 'persp-mode-hook 'space-macs//layout-autosave)
      (advice-add 'persp-load-state-from-file
                  :before 'space-macs//layout-wait-for-modeline)
      (when layouts-enable-local-variables
        (advice-add 'persp-switch :before #'space-macs//load-layout-local-vars))
      (dolist (fn space-macs-layouts-restricted-functions)
        (advice-add fn
                    :around 'space-macs-layouts//advice-with-persp-buffer-list))
      (space-macs/declare-prefix "b" "persp-buffers")
      (space-macs/set-leader-keys
        "ba"   'persp-add-buffer
        "br"   'persp-remove-buffer))))



(defun space-macs-layouts/post-init-spaceline ()
  (setq spaceline-display-default-perspective
        dotspace-macs-display-default-layout))



(defun space-macs-layouts/init-counsel-projectile ()
  (use-package counsel-projectile
    :defer t
    :init (space-macs/set-leader-keys "pl" 'space-macs/ivy-persp-switch-project)
    :config (ivy-set-actions
             'space-macs/ivy-persp-switch-project
             '(("d" space-macs/ivy-switch-project-open-dired "dired")))))


