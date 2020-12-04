;;; packages.el --- Space-macs Navigation Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-navigation-packages
      '(ace-link
        ace-window
        auto-highlight-symbol
        centered-cursor-mode
        (compile :location built-in)
        (doc-view :location built-in)
        (view :location built-in)
        golden-ratio
        (grep :location built-in)
        (info+ :location local)
        open-junk-file
        paradox
        restart-e-macs
        (smooth-scrolling :location built-in)
        symbol-overlay
        winum))

(defun space-macs-navigation/init-ace-link ()
  (use-package ace-link
    :commands space-macs/ace-buffer-links
    :init
    (progn
      (define-key space-macs-buffer-mode-map "o" 'space-macs/ace-buffer-links)
      (with-eval-after-load 'info
        (define-key Info-mode-map "o" 'ace-link-info))
      (with-eval-after-load 'help-mode
        (define-key help-mode-map "o" 'ace-link-help))
      (with-eval-after-load 'eww
        (define-key eww-link-keymap "o" 'ace-link-eww)
        (define-key eww-mode-map "o" 'ace-link-eww)))))

(defun space-macs-navigation/init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (progn
      (space-macs/set-leader-keys
        "bD" 'space-macs/ace-kill-this-buffer
        ;; FIXME: Needs new binding.
        ;; "wC" 'space-macs/ace-center-window
        "wD" 'space-macs/ace-delete-window
        "wM" 'ace-swap-window
        "wW" 'ace-window)
      ;; set ace-window keys to home-row
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))))

(defun space-macs-navigation/init-auto-highlight-symbol ()
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
            space-macs--symbol-highlight-transient-state-doc "
 %s
 [_n_] next   [_N_/_p_] prev  [_d_/_D_] next/prev def  [_r_] range  [_R_] reset  [_z_] recenter
 [_e_] iedit")

      ;; since we are creating our own maps,
      ;; prevent the default keymap from getting created
      (setq auto-highlight-symbol-mode-map (make-sparse-keymap))

      (space-macs|add-toggle automatic-symbol-highlight
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
      (space-macs/add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                                            markdown-mode-hook)))
    :config
    (progn
      (space-macs|hide-lighter auto-highlight-symbol-mode)
      (defvar-local space-macs-last-ahs-highlight-p nil
        "Info on the last searched highlighted symbol.")
      (defvar-local space-macs--ahs-searching-forward t)

      (with-eval-after-load 'evil
        (define-key evil-motion-state-map (kbd "*")
          'space-macs/enter-ahs-forward)
        (define-key evil-motion-state-map (kbd "#")
          'space-macs/enter-ahs-backward))

      (space-macs/set-leader-keys
        "sh" 'space-macs/symbol-highlight
        "sH" 'space-macs/goto-last-searched-ahs-symbol)

      ;; micro-state to easily jump from a highlighted symbol to the others
      (dolist (sym '(ahs-forward
                     ahs-forward-definition
                     ahs-backward
                     ahs-backward-definition
                     ahs-back-to-start
                     ahs-change-range))
        (let* ((advice (intern (format "space-macs/%s" (symbol-name sym)))))
          (eval `(defadvice ,sym (around ,advice activate)
                   (space-macs/ahs-highlight-now-wrapper)
                   ad-do-it
                   (space-macs/ahs-highlight-now-wrapper)
                   (setq space-macs-last-ahs-highlight-p (ahs-highlight-p))))))

      ;; transient state
      (space-macs|define-transient-state symbol-highlight
        :title "Symbol Highlight Transient State"
        :hint-is-doc t
        :dynamic-hint (space-macs//symbol-highlight-ts-doc)
        :before-exit (space-macs//ahs-ts-on-exit)
        :bindings
        ("d" ahs-forward-definition)
        ("D" ahs-backward-definition)
        ("e" space-macs/ahs-to-iedit :exit t)
        ("n" space-macs/quick-ahs-forward)
        ("N" space-macs/quick-ahs-backward)
        ("p" space-macs/quick-ahs-backward)
        ("R" ahs-back-to-start)
        ("r" ahs-change-range)
        ("z" (progn (recenter-top-bottom)
                    (space-macs/symbol-highlight)))
        ("q" nil :exit t)))))

(defun space-macs-navigation/init-centered-cursor-mode ()
  (use-package centered-cursor-mode
    :commands (centered-cursor-mode
               global-centered-cursor-mode)
    :init
    (progn
      (space-macs|add-toggle centered-point
        :mode centered-cursor-mode
        :documentation
        "Keep point at the center of the window."
        :evil-leader "t-")
      (space-macs|add-toggle centered-point-globally
        :mode global-centered-cursor-mode
        :documentation
        "Keep point at the center of the window globally."
        :evil-leader "t C--"))
    :config
    (progn
      (setq ccm-recenter-at-end-of-file t
            ccm-ignored-commands '(mouse-drag-region
                                   mouse-set-point
                                   mouse-set-region
                                   widget-button-click
                                   scroll-bar-toolkit-scroll
                                   evil-mouse-drag-region))
      (space-macs|diminish centered-cursor-mode " âŠ" " -"))))

(defun space-macs-navigation/init-compile ()
  (use-package compile
    :defer t
    :config
    (define-key compilation-mode-map "h" nil)))

(defun space-macs-navigation/init-doc-view ()
  (use-package doc-view
    :defer t
    :init
    (evilified-state-evilify doc-view-mode doc-view-mode-map
      "/"  'space-macs/doc-view-search-new-query
      "?"  'space-macs/doc-view-search-new-query-backward
      "gg" 'doc-view-first-page
      "G"  'space-macs/doc-view-goto-page
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
          (around space-macs/doc-view-toggle-display activate)
        (if (eq major-mode 'doc-view-mode)
            (progn
              ad-do-it
              (text-mode)
              (doc-view-minor-mode))
          ad-do-it)))))

(defun space-macs-navigation/init-view ()
  (use-package view
    :defer t
    :init
    ;; Add binding via mode symbole to have a local binding set
    ;; after loading view mode. If not done this way the new bindings
    ;; will only be affective after the user pressing `q' once.
    (evil-define-key 'normal 'view-mode
      "q" #'View-quit)))

(defun space-macs-navigation/init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :init
    (progn
      (space-macs/transient-state-register-add-bindings 'window
        '(("g" space-macs/toggle-golden-ratio)))
      (space-macs|add-toggle golden-ratio
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
                   "ranger-mode"
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
                   next-multiframe-window
                   previous-multiframe-window
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
                   'space-macs/no-golden-ratio-guide-key)

      (space-macs|diminish golden-ratio-mode " â“–" " g"))))

(defun space-macs-navigation/init-grep ()
  (use-package grep
    :defer t
    :config
    (define-key grep-mode-map "h" nil)))

(defun space-macs-navigation/init-info+ ()
  (use-package info+
    :defer t
    :init
    (progn
      (setq Info-fontify-angle-bracketed-flag nil)
      (add-hook 'Info-mode-hook (lambda () (require 'info+))))))

(defun space-macs-navigation/init-open-junk-file ()
  (use-package open-junk-file
    :defer t
    :commands (open-junk-file)
    :init
    (progn
      (setq open-junk-file-format (concat space-macs-cache-directory "junk/%Y/%m/%d-%H%M%S."))
      (space-macs/set-leader-keys "fJ" 'space-macs/open-junk-file)
      ;; function to run open-junk-file hooks is buggy when opening a large file
      ;; and e-macs warns about it.
      ;; Since this is not really useful to add hooks to open-junk-files lets remove
      ;; it
      (remove-hook 'find-file-hook 'find-file-hook--open-junk-file))))

(defun space-macs-navigation/init-paradox ()
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
      (space-macs/set-leader-keys
        "ak" 'space-macs/paradox-list-packages))))

(defun space-macs-navigation/init-restart-e-macs ()
  (use-package restart-e-macs
    :defer t
    :init
    (space-macs/set-leader-keys
      "qd" 'space-macs/restart-e-macs-debug-init
      "qD" 'space-macs/restart-stock-e-macs-with-packages
      "qr" 'space-macs/restart-e-macs-resume-layouts
      "qR" 'space-macs/restart-e-macs
      "qt" 'space-macs/restart-e-macs-timed-requires
      "qT" 'space-macs/restart-e-macs-adv-timers)))

(defun space-macs-navigation/init-smooth-scrolling ()
  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively (if dotspace-macs-smooth-scrolling 101 0))
  (space-macs|add-toggle smooth-scrolling
    :status (= 101 scroll-conservatively)
    :on (space-macs/enable-smooth-scrolling)
    :off (space-macs/disable-smooth-scrolling)
    :documentation "Smooth scrolling."
    :evil-leader "tv"))

(defun space-macs-navigation/init-symbol-overlay ()
  (use-package symbol-overlay
    :init
    (progn
      (setq space-macs--symbol-overlay-transient-state-doc "
%s
 [_n_] next   [_N_/_p_] prev      [_d_] def           [_f_/_b_] switch [_t_] scope
 [_e_] echo   [_o_]^^   unoverlay [_O_] unoverlay all [_c_]^^   copy   [_z_] center
 [_s_] search [_r_]^^   replace   [_R_] rename        ^^^^             [_q_] quit")

      ;; since we are creating our own maps,
      ;; prevent the default keymap from getting created
      (setq symbol-overlay-map (make-sparse-keymap)))
    :config
    (progn
      (space-macs/set-leader-keys
        "so" 'space-macs/symbol-overlay
        "sO" 'symbol-overlay-remove-all)

      ;; transient state
      (space-macs|define-transient-state symbol-overlay
        :title "Symbol Overlay Transient State"
        :hint-is-doc t
        :dynamic-hint (space-macs//symbol-overlay-ts-doc)
        :bindings
        ("b" symbol-overlay-switch-backward)
        ("c" symbol-overlay-save-symbol)
        ("d" symbol-overlay-jump-to-definition)
        ("e" symbol-overlay-echo-mark)
        ("f" symbol-overlay-switch-forward)
        ("n" symbol-overlay-jump-next)
        ("N" symbol-overlay-jump-prev)
        ("o" symbol-overlay-put)
        ("O" symbol-overlay-remove-all)
        ("p" symbol-overlay-jump-prev)
        ("r" symbol-overlay-query-replace)
        ("R" symbol-overlay-rename)
        ("s" symbol-overlay-isearch-literally)
        ("t" symbol-overlay-toggle-in-scope)
        ("z" recenter-top-bottom)
        ("q" nil :exit t)))))

(defun space-macs-navigation/init-winum ()
  (use-package winum
    :config
    (progn
      (setq winum-auto-assign-0-to-minibuffer nil
            winum-auto-setup-mode-line nil
            winum-ignored-buffers '(" *LV*" " *which-key*"))
      (space-macs/set-leader-keys
        "`" 'winum-select-window-by-number
        "Â²" 'winum-select-window-by-number
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


