;;; packages.el --- Space-macs UI Visual Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-visual-packages
      '(
        (ansi-colors :location built-in)
        desktop
        ;; `display-fill-column-indicator' is available in e-macs 27+
        (display-fill-column-indicator :location built-in
                                       :toggle (boundp 'display-fill-column-indicator))
        (fill-column-indicator :toggle (not (boundp 'display-fill-column-indicator)))
        hl-todo
        popup
        popwin
        (zoom-frm :location local)
        ))

(defun space-macs-visual/init-ansi-colors ()
  (add-hook 'compilation-filter-hook
            'space-macs-visual//compilation-buffer-apply-ansi-colors))

(defun space-macs-visual/init-desktop ()
  (use-package desktop
    :defer t
    :init
    (setq desktop-dirname space-macs-cache-directory)
    :config
    (add-to-list 'desktop-path space-macs-cache-directory)))

(defun space-macs-visual/init-display-fill-column-indicator ()
  (space-macs|add-toggle fill-column-indicator
    :mode display-fill-column-indicator-mode
    :documentation "Display the fill column indicator."
    :evil-leader "tf")
  (space-macs|add-toggle fill-column-indicator-globally
    :mode global-display-fill-column-indicator-mode
    :documentation "Display the fill column indicator globally."
    :evil-leader "t C-f")
  (with-eval-after-load 'display-fill-column-indicator
    ;; manually register the minor mode since it does not define any
    ;; lighter
    (add-to-list 'minor-mode-alist '(display-fill-column-indicator-mode ""))
    (space-macs|diminish display-fill-column-indicator-mode " â“•" " f")))

(defun space-macs-visual/init-fill-column-indicator ()
  (use-package fill-column-indicator
    :defer t
    :init
    (progn
      (setq fci-rule-width 1)
      ;; manually register the minor mode since it does not define any
      ;; lighter
      (add-to-list 'minor-mode-alist '(fci-mode ""))
      (space-macs|add-toggle fill-column-indicator
        :status fci-mode
        :on (turn-on-fci-mode)
        :off (turn-off-fci-mode)
        :documentation "Display the fill column indicator."
        :evil-leader "tf"))
    :config
    (space-macs|diminish fci-mode " â“•" " f")))

(defun space-macs-visual/init-hl-todo ()
  (use-package hl-todo
    :defer t
    :init
    ;; global hook activates hl-todo-mode for prog-mode, text-mode
    ;; mode can be explicitly defined using hl-todo-activate-in-modes variable
    (global-hl-todo-mode 1)))

(defun space-macs-visual/init-popup ())

(defun space-macs-visual/init-popwin ()
  (use-package popwin
    :config
    (progn
      (popwin-mode 1)
      (space-macs/set-leader-keys "wpm" 'popwin:messages)
      (space-macs/set-leader-keys "wpp" 'popwin:close-popup-window)

      ;; don't use default value but manage it ourselves
      (setq popwin:special-display-config nil)

      ;; buffers that we manage
      (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Process List*"         :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
      (push '(compilation-mode         :dedicated nil :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '(dap-server-log-mode      :dedicated nil :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*undo-tree*"            :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
      (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
      (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)
      (push '("*Google Translate*"     :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config))))

(defun space-macs-visual/init-zoom-frm ()
  (use-package zoom-frm
    :commands (zoom-frm-unzoom
               zoom-frm-out
               zoom-frm-in)
    :init
    (progn
      (space-macs|define-transient-state zoom-frm
        :title "Zoom Frame Transient State"
        :doc "
[_+_/_=_/_k_] zoom frame in   [_m_] max frame
[_-_/___/_j_] zoom frame out  [_f_] fullscreen
[_0_]^^^^     reset zoom      [_q_] quit"
        :bindings
        ("+" space-macs/zoom-frm-in)
        ("=" space-macs/zoom-frm-in)
        ("k" space-macs/zoom-frm-in)
        ("-" space-macs/zoom-frm-out)
        ("_" space-macs/zoom-frm-out)
        ("j" space-macs/zoom-frm-out)
        ("0" space-macs/zoom-frm-unzoom)
        ("f" space-macs/toggle-frame-fullscreen-non-native)
        ("m" space-macs/toggle-maximize-frame)
        ("q" nil :exit t))
      (space-macs/set-leader-keys "zf" 'space-macs/zoom-frm-transient-state/body)

      ;; Font size, either with ctrl + mouse wheel
      (global-set-key (kbd "<C-wheel-up>") 'space-macs/zoom-frm-in)
      (global-set-key (kbd "<C-wheel-down>") 'space-macs/zoom-frm-out))))


