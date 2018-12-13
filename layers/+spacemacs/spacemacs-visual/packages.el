;;; packages.el --- Spacemacs UI Visual Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-visual-packages
      '(
        (ansi-colors :location built-in)
        desktop
        fill-column-indicator
        hl-todo
        popup
        popwin
        (zoom-frm :location local)
        ))

(defun spacemacs-visual/init-ansi-colors ()
  (add-hook 'compilation-filter-hook
            'spacemacs-visual//compilation-buffer-apply-ansi-colors))

(defun spacemacs-visual/init-desktop ()
  (use-package desktop
    :defer t
    :init
    (setq desktop-dirname spacemacs-cache-directory)
    :config
    (add-to-list 'desktop-path spacemacs-cache-directory)))

(defun spacemacs-visual/init-fill-column-indicator ()
  (use-package fill-column-indicator
    :defer t
    :init
    (progn
      (setq fci-rule-width 1)
      ;; manually register the minor mode since it does not define any
      ;; lighter
      (add-to-list 'minor-mode-alist '(fci-mode ""))
      (spacemacs|add-toggle fill-column-indicator
        :status fci-mode
        :on (turn-on-fci-mode)
        :off (turn-off-fci-mode)
        :documentation "Display the fill column indicator."
        :evil-leader "tf"))
    :config
    (spacemacs|diminish fci-mode " ⓕ" " f")))

(defun spacemacs-visual/init-hl-todo ()
  (use-package hl-todo
    :defer t
    :init (spacemacs/add-to-hooks 'hl-todo-mode '(text-mode-hook
                                                  prog-mode-hook))))

(defun spacemacs-visual/init-popup ())

(defun spacemacs-visual/init-popwin ()
  (use-package popwin
    :config
    (progn
      (popwin-mode 1)
      (spacemacs/set-leader-keys "wpm" 'popwin:messages)
      (spacemacs/set-leader-keys "wpp" 'popwin:close-popup-window)

      ;; don't use default value but manage it ourselves
      (setq popwin:special-display-config nil)

      ;; buffers that we manage
      (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Process List*"         :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
      (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '(" *undo-tree*"           :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
      (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
      (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)
      (push '("*Google Translate*"     :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config))))

(defun spacemacs-visual/init-zoom-frm ()
  (use-package zoom-frm
    :commands (zoom-frm-unzoom
               zoom-frm-out
               zoom-frm-in)
    :init
    (progn
      (spacemacs|define-transient-state zoom-frm
        :title "Zoom Frame Transient State"
        :doc "
[_+_/_=_] zoom frame in [_-_] zoom frame out [_0_] reset zoom [_q_] quit"
        :bindings
        ("+" spacemacs/zoom-frm-in)
        ("=" spacemacs/zoom-frm-in)
        ("-" spacemacs/zoom-frm-out)
        ("0" spacemacs/zoom-frm-unzoom)
        ("q" nil :exit t))
      (spacemacs/set-leader-keys "zf" 'spacemacs/zoom-frm-transient-state/body)

      ;; Font size, either with ctrl + mouse wheel
      (global-set-key (kbd "<C-wheel-up>") 'spacemacs/zoom-frm-in)
      (global-set-key (kbd "<C-wheel-down>") 'spacemacs/zoom-frm-out))))
