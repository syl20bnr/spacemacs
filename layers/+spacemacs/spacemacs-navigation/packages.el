;;; packages.el --- Spacemacs Navigation Layer packages File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-navigation-packages
      '(ace-link
        auto-highlight-symbol
        (centered-cursor :location local)
        (doc-view :location built-in)
        flx-ido
        golden-ratio
        info+
        neotree
        open-junk-file
        paradox
        restart-emacs
        (smooth-scrolling :location built-in)
        winum))


(defun spacemacs-navigation/init-ace-link ()
  (use-package ace-link
    :commands spacemacs/ace-buffer-links
    :init
    (progn
      (define-key spacemacs-buffer-mode-map "o" 'spacemacs/ace-buffer-links)
      (with-eval-after-load 'info
        (define-key Info-mode-map "o" 'ace-link-info))
      (with-eval-after-load 'help-mode
        (define-key help-mode-map "o" 'ace-link-help))
      (with-eval-after-load 'eww
        (define-key eww-link-keymap "o" 'ace-link-eww)
        (define-key eww-mode-map "o" 'ace-link-eww)))))

(defun spacemacs-navigation/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :init
    (progn
      (setq ahs-case-fold-search nil
            ahs-default-range 'ahs-range-whole-buffer
            ;; by default disable auto-highlight of symbol
            ;; current symbol can always be highlighted with `SPC s h'
            ahs-idle-timer 0
            ahs-idle-interval 0.25
            ahs-inhibit-face-list nil
            spacemacs--symbol-highlight-transient-state-doc
            "
 %s  [_n_] next   [_N_/_p_] previous   [_r_] change range   [_R_] reset   [_e_] iedit
 %s  [_d_/_D_] next/previous definition")

      ;; since we are creating our own maps,
      ;; prevent the default keymap from getting created
      (setq auto-highlight-symbol-mode-map (make-sparse-keymap))

      (spacemacs|add-toggle automatic-symbol-highlight
        :status (timerp ahs-idle-timer)
        :on (progn
              (auto-highlight-symbol-mode)
              (setq ahs-idle-timer
                    (run-with-idle-timer ahs-idle-interval t
                                         'ahs-idle-function)))
        :off (when (timerp ahs-idle-timer)
               (auto-highlight-symbol-mode)
               (cancel-timer ahs-idle-timer)
               (setq ahs-idle-timer 0))
        :documentation "Automatic highlight of current symbol."
        :evil-leader "tha")
      (spacemacs/add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                                            markdown-mode-hook)))
    :config
    (progn
      (spacemacs|hide-lighter auto-highlight-symbol-mode)
      (defvar-local spacemacs-last-ahs-highlight-p nil
        "Info on the last searched highlighted symbol.")
      (defvar-local spacemacs--ahs-searching-forward t)

      (with-eval-after-load 'evil
        (define-key evil-motion-state-map (kbd "*")
          'spacemacs/enter-ahs-forward)
        (define-key evil-motion-state-map (kbd "#")
          'spacemacs/enter-ahs-backward))

      (spacemacs/set-leader-keys
        "sh" 'spacemacs/symbol-highlight
        "sH" 'spacemacs/goto-last-searched-ahs-symbol)

      ;; micro-state to easily jump from a highlighted symbol to the others
      (dolist (sym '(ahs-forward
                     ahs-forward-definition
                     ahs-backward
                     ahs-backward-definition
                     ahs-back-to-start
                     ahs-change-range))
        (let* ((advice (intern (format "spacemacs/%s" (symbol-name sym)))))
          (eval `(defadvice ,sym (around ,advice activate)
                   (spacemacs/ahs-highlight-now-wrapper)
                   ad-do-it
                   (spacemacs/ahs-highlight-now-wrapper)
                   (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))))))

      ;; transient state
      (spacemacs|define-transient-state symbol-highlight
        :title "Symbol Highlight Transient State"
        :dynamic-hint (spacemacs//symbol-highlight-ts-doc)
        :before-exit (spacemacs//ahs-ms-on-exit)
        :bindings
        ("d" ahs-forward-definition)
        ("D" ahs-backward-definition)
        ("e" spacemacs/ahs-to-iedit :exit t)
        ("n" spacemacs/quick-ahs-forward)
        ("N" spacemacs/quick-ahs-backward)
        ("p" spacemacs/quick-ahs-backward)
        ("R" ahs-back-to-start)
        ("r" ahs-change-range)
        ("q" nil :exit t)))))

(defun spacemacs-navigation/init-centered-cursor ()
  (use-package centered-cursor-mode
    :commands (centered-cursor-mode
               global-centered-cursor-mode)
    :init
    (progn
      (spacemacs|add-toggle centered-point
        :mode centered-cursor-mode
        :documentation
        "Keep point at the center of the window."
        :evil-leader "t-")
      (spacemacs|add-toggle centered-point-globally
        :mode global-centered-cursor-mode
        :documentation
        "Keep point at the center of the window globally."
        :evil-leader "t C--"))
    :config
    (progn
      (setq ccm-recenter-at-end-of-file t
            ccm-ignored-commands '(mouse-drag-region
                                   mouse-set-point
                                   widget-button-click
                                   scroll-bar-toolkit-scroll
                                   evil-mouse-drag-region))
      (spacemacs|diminish centered-cursor-mode " ⊝" " -"))))

(defun spacemacs-navigation/init-doc-view ()
  (use-package doc-view
    :defer t
    :init
    (evilified-state-evilify doc-view-mode doc-view-mode-map
      "/"  'spacemacs/doc-view-search-new-query
      "?"  'spacemacs/doc-view-search-new-query-backward
      "gg" 'doc-view-first-page
      "G"  'spacemacs/doc-view-goto-page
      "gt" 'doc-view-goto-page
      "h"  'doc-view-previous-page
      "j"  'doc-view-next-line-or-next-page
      "k"  'doc-view-previous-line-or-previous-page
      "K"  'doc-view-kill-proc-and-buffer
      "l"  'doc-view-next-page
      "n"  'doc-view-search
      "N"  'doc-view-search-backward
      (kbd "C-d") 'doc-view-scroll-up-or-next-page
      (kbd "C-k") 'doc-view-kill-proc
      (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
    :config
    (progn
      ;; fixed a weird issue where toggling display does not
      ;; swtich to text mode
      (defadvice doc-view-toggle-display
          (around spacemacs/doc-view-toggle-display activate)
        (if (eq major-mode 'doc-view-mode)
            (progn
              ad-do-it
              (text-mode)
              (doc-view-minor-mode))
          ad-do-it)))))

(defun spacemacs-navigation/init-flx-ido ()
  (use-package flx-ido
    :init (flx-ido-mode 1)))

(defun spacemacs-navigation/init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :init
    (progn
      (spacemacs/transient-state-register-add-bindings 'window-manipulation
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
                   "speedbar-mode"))

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
                   winum-select-window-0-or-10
                   winum-select-window-1
                   winum-select-window-2
                   winum-select-window-3
                   winum-select-window-4
                   winum-select-window-5
                   winum-select-window-6
                   winum-select-window-7
                   winum-select-window-8
                   winum-select-window-9
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

(defun spacemacs-navigation/init-info+ ()
  (use-package info+
    :defer t
    :init
    (progn
      (with-eval-after-load 'info
        (require 'info+))
      (setq Info-fontify-angle-bracketed-flag nil))))

(defun spacemacs-navigation/init-neotree ()
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
        ("RET" neotree-enter)
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
          (kbd "RET") 'neotree-enter
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
    (spacemacs//neotree-key-bindings)))

(defun spacemacs-navigation/init-open-junk-file ()
  (use-package open-junk-file
    :defer t
    :commands (open-junk-file)
    :init
    (progn
      (setq open-junk-file-format (concat spacemacs-cache-directory "junk/%Y/%m/%d-%H%M%S."))
      (spacemacs/set-leader-keys "fJ" 'spacemacs/open-junk-file))))

(defun spacemacs-navigation/init-paradox ()
  (use-package paradox
    :commands paradox-list-packages
    :init
    (progn
      (setq paradox-execute-asynchronously nil)
      (evilified-state-evilify paradox-menu-mode paradox-menu-mode-map
        "H" 'paradox-menu-quick-help
        "J" 'paradox-next-describe
        "K" 'paradox-previous-describe
        "L" 'paradox-menu-view-commit-list
        "o" 'paradox-menu-visit-homepage)
      (spacemacs/set-leader-keys
        "ak" 'spacemacs/paradox-list-packages))))

(defun spacemacs-navigation/init-restart-emacs ()
  (use-package restart-emacs
    :defer t
    :init
    (spacemacs/set-leader-keys
      "qd" 'spacemacs/restart-emacs-debug-init
      "qD" 'spacemacs/restart-stock-emacs-with-packages
      "qr" 'spacemacs/restart-emacs-resume-layouts
      "qR" 'spacemacs/restart-emacs
      "qt" 'spacemacs/restart-emacs-timed-requires
      "qt" 'spacemacs/restart-emacs-adv-timers)))

(defun spacemacs-navigation/init-smooth-scrolling ()
  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively (if dotspacemacs-smooth-scrolling 101 0))
  (spacemacs|add-toggle smooth-scrolling
    :status (= 101 scroll-conservatively)
    :on (spacemacs/enable-smooth-scrolling)
    :off (spacemacs/disable-smooth-scrolling)
    :documentation "Smooth scrolling."
    :evil-leader "tv"))

(defun spacemacs-navigation/init-winum ()
  (use-package winum
    :config
    (progn
      (setq winum-auto-assign-0-to-minibuffer nil
            winum-assign-func 'spacemacs//winum-assign-func
            winum-auto-setup-mode-line nil
            winum-ignored-buffers '(" *which-key*"))
      (spacemacs/set-leader-keys
        "`" 'winum-select-window-by-number
        "²" 'winum-select-window-by-number
        "0" 'winum-select-window-0-or-10
        "1" 'winum-select-window-1
        "2" 'winum-select-window-2
        "3" 'winum-select-window-3
        "4" 'winum-select-window-4
        "5" 'winum-select-window-5
        "6" 'winum-select-window-6
        "7" 'winum-select-window-7
        "8" 'winum-select-window-8
        "9" 'winum-select-window-9)
      (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
      (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
      (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
      (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
      (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
      (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
      (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
      (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
      (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
      (winum-mode))))
