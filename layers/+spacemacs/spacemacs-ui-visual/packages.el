;;; packages.el --- Spacemacs UI Visual Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-ui-visual-packages
      '(
        (ansi-colors :location built-in)
        fancy-battery
        fill-column-indicator
        golden-ratio
        hl-todo
        neotree
        popup
        popwin
        (smooth-scrolling :location built-in)
        spaceline
        (zoom-frm :location local)))

(defun spacemacs-ui-visual/init-ansi-colors ()
  (add-hook 'compilation-filter-hook
            'spacemacs-ui-visual//compilation-buffer-apply-ansi-colors))

(defun spacemacs-ui-visual/init-fancy-battery ()
  (use-package fancy-battery
    :defer t
    :init
    (progn
      (spacemacs|add-toggle mode-line-battery
        :mode fancy-battery-mode
        :documentation "Display battery info in mode-line."
        :evil-leader "tmb")
      (setq-default fancy-battery-show-percentage t))))

(defun spacemacs-ui-visual/init-fill-column-indicator ()
  (use-package fill-column-indicator
    :defer t
    :init
    (progn
      (setq fci-rule-width 1)
      (setq fci-rule-color "#D0BF8F")
      ;; manually register the minor mode since it does not define any
      ;; lighter
      (push '(fci-mode "") minor-mode-alist)
      (spacemacs|add-toggle fill-column-indicator
        :status fci-mode
        :on (turn-on-fci-mode)
        :off (turn-off-fci-mode)
        :documentation "Display the fill column indicator."
        :evil-leader "tf"))
    :config
    (spacemacs|diminish fci-mode " ⓕ" " f")))

(defun spacemacs-ui-visual/init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :init
    (progn
      (setq spacemacs-window-manipulation-transient-state-add-bindings
            '(("g" spacemacs/toggle-golden-ratio)))
      (spacemacs|add-toggle golden-ratio
        :status golden-ratio-mode
        :on (golden-ratio-mode) (golden-ratio)
        :off (golden-ratio-mode -1) (balance-windows)
        :documentation "Resize the focused window using the golden ratio."
        :evil-leader "tg"))
    :config
    (progn
      ;; golden-ratio-exclude-modes
      (dolist (m '("bs-mode"
                   "calc-mode"
                   "ediff-mode"
                   "dired-mode"
                   "gud-mode"
                   "gdb-locals-mode"
                   "gdb-registers-mode"
                   "gdb-breakpoints-mode"
                   "gdb-threads-mode"
                   "gdb-frames-mode"
                   "gdb-inferior-io-mode"
                   "gdb-disassembly-mode"
                   "gdb-memory-mode"
                   "speedbar-mode"
                   ))
        (add-to-list 'golden-ratio-exclude-modes m))

      (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

      ;; golden-ratio-extra-commands
      (dolist (f '(ace-window
                   ace-delete-window
                   ace-select-window
                   ace-swap-window
                   ace-maximize-window
                   avy-pop-mark
                   buf-move-left
                   buf-move-right
                   buf-move-up
                   buf-move-down
                   evil-avy-goto-word-or-subword-1
                   evil-avy-goto-line
                   evil-window-delete
                   evil-window-split
                   evil-window-vsplit
                   evil-window-left
                   evil-window-right
                   evil-window-up
                   evil-window-down
                   evil-window-bottom-right
                   evil-window-top-left
                   evil-window-mru
                   evil-window-next
                   evil-window-prev
                   evil-window-new
                   evil-window-vnew
                   evil-window-rotate-upwards
                   evil-window-rotate-downwards
                   evil-window-move-very-top
                   evil-window-move-far-left
                   evil-window-move-far-right
                   evil-window-move-very-bottom
                   quit-window
                   select-window-0
                   select-window-1
                   select-window-2
                   select-window-3
                   select-window-4
                   select-window-5
                   select-window-6
                   select-window-7
                   select-window-8
                   select-window-9
                   windmove-left
                   windmove-right
                   windmove-up
                   windmove-down))
        (add-to-list 'golden-ratio-extra-commands f))

      ;; golden-ratio-exclude-buffer-names
      (dolist (n '(" *NeoTree*"
                   "*LV*"
                   " *which-key*"))
        (add-to-list 'golden-ratio-exclude-buffer-names n))

      (add-to-list 'golden-ratio-inhibit-functions
                   'spacemacs/no-golden-ratio-guide-key)

      (spacemacs|diminish golden-ratio-mode " ⓖ" " g"))))

(defun spacemacs-ui-visual/init-hl-todo ()
  (use-package hl-todo
    :defer t
    :init (spacemacs/add-to-hooks 'hl-todo-mode '(text-mode-hook
                                                  prog-mode-hook))))

(defun spacemacs-ui-visual/init-neotree ()
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

      (spacemacs|define-transient-state neotree
        :title "NeoTree Key Hints"
        :doc "
Navigation^^^^             Actions^^         Visual actions/config^^^
───────^^^^─────────────── ───────^^──────── ───────^^^────────────────
[_L_]   next sibling^^     [_c_] create      [_TAB_] shrink/enlarge
[_H_]   previous sibling^^ [_d_] delete      [_|_]   vertical split
[_J_]   goto child^^       [_r_] rename      [_-_]   horizonatal split
[_K_]   goto parent^^      [_R_] change root [_gr_]  refresh^
[_l_]   open/expand^^      ^^                [_s_]   hidden:^^^ %s(if neo-buffer--show-hidden-file-p \"on\" \"off\")
[_h_]   up/collapse^^      ^^                ^^^
[_j_]   line down^^        ^^                ^^^
[_k_]   line up^^          ^^                ^^
[_RET_] open               ^^^^              [_?_]   close hints
"
        :bindings
        ("RET" neotree-enter)
        ("TAB" neotree-stretch-toggle)
        ("|" neotree-enter-vertical-split)
        ("-" neotree-enter-horizontal-split)
        ("?" nil :exit t)
        ("c" neotree-create-node)
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
          (kbd "RET") 'neotree-enter
          (kbd "|") 'neotree-enter-vertical-split
          (kbd "-") 'neotree-enter-horizontal-split
          (kbd "c") 'neotree-create-node
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
        "pt" 'neotree-find-project-root))
    :config
    (spacemacs//neotree-key-bindings)))

(defun spacemacs-ui-visual/init-popup ())

(defun spacemacs-ui-visual/init-popwin ()
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
      (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '(" *undo-tree*"           :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
      (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config))))

(defun spacemacs-ui-visual/init-smooth-scrolling ()
  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively (if dotspacemacs-smooth-scrolling 101 0))
  (spacemacs|add-toggle smooth-scrolling
    :status (= 101 scroll-conservatively)
    :on (spacemacs/enable-smooth-scrolling)
    :off (spacemacs/disable-smooth-scrolling)
    :documentation "Smooth scrolling."
    :evil-leader "tv"))

(defun spacemacs-ui-visual/init-spaceline ()
  (use-package spaceline-config
    :init
    (progn
      (add-hook 'spacemacs-post-user-config-hook 'spaceline-compile)
      (add-hook 'spacemacs-post-theme-change-hook
                'spacemacs/customize-powerline-faces)
      (add-hook 'spacemacs-post-theme-change-hook 'powerline-reset)
      (setq-default powerline-default-separator 'utf-8)
      (spacemacs|do-after-display-system-init
       (when (and (eq 'utf-8 powerline-default-separator))
         (setq-default powerline-default-separator 'wave))
       ;; seems to be needed to avoid weird graphical artefacts with the
       ;; first graphical client
       (require 'spaceline)
       (spaceline-compile)))
    :config
    (progn
      (spacemacs/customize-powerline-faces)
      (setq spaceline-org-clock-p nil
            spaceline-highlight-face-func 'spacemacs//evil-state-face)
      ;; Segment toggles
      (dolist (spec '((minor-modes "tmm")
                      (major-mode "tmM")
                      (version-control "tmv")
                      (new-version "tmV")
                      (point-position "tmp")
                      (org-clock "tmc")))
        (let* ((segment (car spec))
               (status-var (intern (format "spaceline-%S-p" segment))))
          (eval `(spacemacs|add-toggle ,(intern (format "mode-line-%S" segment))
                   :status ,status-var
                   :on (setq ,status-var t)
                   :off (setq ,status-var nil)
                   :documentation ,(format "Show %s in the mode-line."
                                           (replace-regexp-in-string
                                            "-" " " (format "%S" segment)))
                   :evil-leader ,(cadr spec)))))
      ;; unicode
      (let ((unicodep (dotspacemacs|symbol-value
                       dotspacemacs-mode-line-unicode-symbols)))
        (setq spaceline-window-numbers-unicode unicodep
              spaceline-workspace-numbers-unicode unicodep))
      (add-hook 'spaceline-pre-hook 'spacemacs//prepare-diminish)
      ;; New spacemacs version segment
      (defpowerline spacemacs-powerline-new-version
        (propertize
         spacemacs-version-check-lighter
         'mouse-face 'mode-line-highlight
         'help-echo (format "New version %s | Click with mouse-1 to update"
                            spacemacs-new-version)
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map
                        [mode-line down-mouse-1]
                        (lambda (event)
                          (interactive "@e")
                          (if (yes-or-no-p
                               (format
                                (concat "Do you want to update to the newest "
                                        "version %s ?") spacemacs-new-version))
                              (progn
                                (spacemacs/switch-to-version
                                 spacemacs-new-version))
                            (message "Update aborted."))))
                      map)))
      (spaceline-define-segment new-version
        (when spacemacs-new-version
          (spacemacs-powerline-new-version
           (spacemacs/get-new-version-lighter-face
            spacemacs-version spacemacs-new-version))))
      (apply #'spaceline-spacemacs-theme
             spacemacs-spaceline-additional-segments)
      ;; Additional spacelines
      (when (package-installed-p 'helm)
        (spaceline-helm-mode t))
      (when (configuration-layer/package-usedp 'info+)
        (spaceline-info-mode t))
      ;; Enable spaceline for buffers created before the configuration of
      ;; spaceline
      (spacemacs//set-powerline-for-startup-buffers))))

(defun spacemacs-ui-visual/init-zoom-frm ()
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
