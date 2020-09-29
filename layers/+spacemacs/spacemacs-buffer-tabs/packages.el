;;; packages.el --- tabs layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018, 2020 Sylvain Benner & Contributors
;;
;; Author: Deepu Puthrote <git@deepumohan.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst spacemacs-buffer-tabs-packages
  '(centaur-tabs))

(defun spacemacs-buffer-tabs/init-centaur-tabs ()
  (use-package centaur-tabs
    :defer t
    :init
    (progn
      ;; configuration
      (setq centaur-tabs-cycle-scope 'tabs
            centaur-tabs-buffer-groups-function 'centaur-tabs-projectile-buffer-groups
            centaur-tabs-set-icons t
            centaur-tabs-gray-out-icons 'buffer
            centaur-tabs-show-navigation-buttons nil
            centaur-tabs-set-modified-marker t
            centaur-tabs-modified-marker "⚠"
            centaur-tabs-set-bar 'left
            centaur-tabs-style 'bar)
      ;; key bindings
      (spacemacs/declare-prefix "[" "buffer tabs")
      (spacemacs|spacebind
       "Buffer tabs displayed at the top of the buffer."
       :global
       (("b" "Buffers"
         ("t" spacemacs/toggle-buffer-tabs "buffer tabs"))
        ("[" "Buffer Tabs"
         ("<" spacemacs/buffer-tabs-transient-state/centaur-tabs-move-current-tab-to-left "Move tab to the left")
         (">" spacemacs/buffer-tabs-transient-state/centaur-tabs-move-current-tab-to-right "Move tab to the right")
         ("C-h" spacemacs/buffer-tabs-transient-state/centaur-tabs-backward "Select previous tab")
         ("C-l" spacemacs/buffer-tabs-transient-state/centaur-tabs-forward "Select next tab")
         ("N" spacemacs/buffer-tabs-transient-state/centaur-tabs-backward "Select previous tab")
         ("n" spacemacs/buffer-tabs-transient-state/centaur-tabs-forward "Select next tab")
         ("p" spacemacs/buffer-tabs-transient-state/centaur-tabs-backward "Select previous tab")
         ("t" "toggles"
          ("p" centaur-tabs-group-by-projectile-project "Group by project")
          ("g" centaur-tabs-group-buffer-groups "Group by group rules")))))
      ;; evil integration
      (define-key evil-normal-state-map (kbd "g t") 'spacemacs/buffer-tabs-transient-state/centaur-tabs-forward)
      (define-key evil-normal-state-map (kbd "g T") 'spacemacs/buffer-tabs-transient-state/centaur-tabs-backward)
      (define-key evil-motion-state-map (kbd "g t") 'spacemacs/buffer-tabs-transient-state/centaur-tabs-forward)
      (define-key evil-motion-state-map (kbd "g T") 'spacemacs/buffer-tabs-transient-state/centaur-tabs-backward)
      ;; global toggle
      (spacemacs|add-toggle buffer-tabs
        :mode centaur-tabs-mode
        :documentation "Enable buffer tabs."
        :evil-leader "T[")
      (when dotspacemacs-buffer-tabs
        (add-hook 'emacs-startup-hook
                  (lambda ()
                    (spacemacs|add-transient-hook window-configuration-change-hook
                      (lambda () (centaur-tabs-mode t))
                      lazy-load-centaur-tabs))))
      ;; transient state
      (spacemacs|transient-state-format-hint buffer-tabs
        spacemacs--buffer-tabs-ts-full-hint
        "\n
 Go to^^^^^^^^                         Actions^^^^
 ─────^^^^^^^^───────────────────────  ───────^^^^───────────────────────
 [_n_/_C-l_/_t_]^^    next tab         [_<_/_>_] move tab left/right
 [_N_/_p_/_C-h_/_T_]  prev tab         [_?_]^^   toggle help")
      (spacemacs|define-transient-state buffer-tabs
        :title "Buffer Tabs Transient State"
        :hint-is-doc t
        :dynamic-hint (spacemacs//buffer-tabs-ts-hint)
        :bindings
        ("?" spacemacs//buffer-tabs-ts-toggle-hint)
        ("<" centaur-tabs-move-current-tab-to-left)
        (">" centaur-tabs-move-current-tab-to-right)
        ("C-h" centaur-tabs-backward)
        ("C-l" centaur-tabs-forward)
        ("N" centaur-tabs-backward)
        ("n" centaur-tabs-forward)
        ("p" centaur-tabs-backward)
        ("T" centaur-tabs-backward)
        ("t" centaur-tabs-forward)
        ;; other
        ("q" nil :exit t)))
    :config
    (progn
      (centaur-tabs-headline-match)
      (defadvice spacemacs/post-theme-init (after spacemacs-buffer-tabs/post-theme-init activate)
        "Makes sure the headline color always matches the theme."
        (centaur-tabs-headline-match)))))
