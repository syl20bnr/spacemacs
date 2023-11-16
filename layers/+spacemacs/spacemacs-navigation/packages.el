;;; packages.el --- Spacemacs Navigation Layer packages File
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(setq spacemacs-navigation-packages
      '(ace-link
        ace-window
        auto-highlight-symbol
        centered-cursor-mode
        (compile :location built-in)
        (doc-view :location built-in)
        (view :location built-in)
        golden-ratio
        (grep :location built-in)
        (info+ :location (recipe :fetcher github
                                 :repo "emacsmirror/info-plus"))
        open-junk-file
        paradox
        restart-emacs
        (smooth-scrolling :location built-in)
        symbol-overlay
        winum))

(defun spacemacs-navigation/init-ace-link ()
  (use-package ace-link
    :commands spacemacs/ace-buffer-links
    :init
    (define-key spacemacs-buffer-mode-map "o" 'spacemacs/ace-buffer-links)
    (with-eval-after-load 'info
      (define-key Info-mode-map "o" 'ace-link-info))
    (with-eval-after-load 'help-mode
      (define-key help-mode-map "o" 'ace-link-help))
    (with-eval-after-load 'woman
      (define-key woman-mode-map "o" 'link-hint-open-link))
    (with-eval-after-load 'eww
      (define-key eww-link-keymap "o" 'ace-link-eww)
      (define-key eww-mode-map "o" 'ace-link-eww))))

(defun spacemacs-navigation/init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (spacemacs/set-leader-keys
      "bD" 'spacemacs/ace-kill-this-buffer
      ;; FIXME: Needs new binding.
      ;; "wC" 'spacemacs/ace-center-window
      "wD" 'spacemacs/ace-delete-window
      "wM" 'ace-swap-window
      "wW" 'ace-window)
    ;; set ace-window keys to home-row
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(defun spacemacs-navigation/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :commands (ahs-highlight-p)
    :init
    (setq ahs-case-fold-search nil
          ahs-default-range 'ahs-range-whole-buffer
          ahs-idle-interval 0.25
          ahs-inhibit-face-list nil)

    ;; transient state
    (setq spacemacs--symbol-highlight-transient-state-doc "
 %s
 [_n_] next   [_N_/_p_] prev  [_d_/_D_] next/prev def  [_r_] range  [_R_] reset  [_z_] recenter
 [_e_] iedit")
    (spacemacs|define-transient-state symbol-highlight
      :title "Symbol Highlight Transient State"
      :hint-is-doc t
      :dynamic-hint (spacemacs//symbol-highlight-ts-doc)
      :on-exit (spacemacs//ahs-ts-on-exit)
      :bindings
      ("d" ahs-forward-definition)
      ("D" ahs-backward-definition)
      ("e" spacemacs/ahs-to-iedit :exit t)
      ("n" spacemacs/quick-ahs-forward)
      ("N" spacemacs/quick-ahs-backward)
      ("p" spacemacs/quick-ahs-backward)
      ("R" ahs-back-to-start)
      ("r" ahs-change-range)
      ("z" recenter-top-bottom)
      ("q" nil :exit t))

    ;; since we are creating our own maps,
    ;; prevent the default keymap from getting created
    (setq auto-highlight-symbol-mode-map (make-sparse-keymap))

    (spacemacs|add-toggle automatic-symbol-highlight
      :mode auto-highlight-symbol-mode
      :documentation "Automatic highlight of current symbol."
      :evil-leader "tha")

    (with-eval-after-load 'evil
      (define-key evil-motion-state-map (kbd "*")
        'spacemacs/enter-ahs-forward)
      (define-key evil-motion-state-map (kbd "#")
        'spacemacs/enter-ahs-backward))

    (spacemacs/set-leader-keys
      "sh" 'spacemacs/symbol-highlight
      "sH" 'spacemacs/goto-last-searched-ahs-symbol)

    ;; Advice ahs jump functions to remember the last highlighted symbol
    (dolist (sym '(ahs-forward
                   ahs-forward-definition
                   ahs-backward
                   ahs-backward-definition
                   ahs-back-to-start
                   ahs-change-range))
      (advice-add sym :after #'spacemacs//remember-last-ahs-highlight))
    :config
    (spacemacs|hide-lighter auto-highlight-symbol-mode)
    (defvar-local spacemacs-last-ahs-highlight-p nil
      "Info on the last searched highlighted symbol.")
    (defvar-local spacemacs--ahs-searching-forward t)))

(defun spacemacs-navigation/init-centered-cursor-mode ()
  (use-package centered-cursor-mode
    :commands (centered-cursor-mode
               global-centered-cursor-mode)
    :init
    (spacemacs|add-toggle centered-point
      :mode centered-cursor-mode
      :documentation
      "Keep point at the center of the window."
      :evil-leader "t-")
    (spacemacs|add-toggle centered-point-globally
      :mode global-centered-cursor-mode
      :documentation
      "Keep point at the center of the window globally."
      :evil-leader "t C--")
    :config
    (setq ccm-recenter-at-end-of-file t
          ccm-ignored-commands '(mouse-drag-region
                                 mouse-set-point
                                 mouse-set-region
                                 widget-button-click
                                 scroll-bar-toolkit-scroll
                                 evil-mouse-drag-region))
    (spacemacs|diminish centered-cursor-mode " ⊝" " -")))

(defun spacemacs-navigation/init-compile ()
  (use-package compile
    :defer t
    :config
    (define-key compilation-mode-map "h" nil)))

(defun spacemacs-navigation/init-doc-view ()
  (use-package doc-view
    :defer t
    :config
    (evilified-state-evilify-map doc-view-mode-map
      :mode doc-view-mode
      :bindings
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
    ;; fixed a weird issue where toggling display does not
    ;; swtich to text mode
    (defadvice doc-view-toggle-display
        (around spacemacs/doc-view-toggle-display activate)
      (if (eq major-mode 'doc-view-mode)
          (progn
            ad-do-it
            (text-mode)
            (doc-view-minor-mode))
        ad-do-it))))

(defun spacemacs-navigation/init-view ()
  (use-package view
    :defer t
    :init
    ;; Add binding via mode symbole to have a local binding set
    ;; after loading view mode. If not done this way the new bindings
    ;; will only be affective after the user pressing `q' once.
    (evil-define-key 'normal 'view-mode
      "q" #'View-quit)))

(defun spacemacs-navigation/init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :init
    (spacemacs/transient-state-register-add-bindings 'window
      '(("g" spacemacs/toggle-golden-ratio)))
    (spacemacs|add-toggle golden-ratio
      :status golden-ratio-mode
      :on (golden-ratio-mode) (golden-ratio)
      :off (golden-ratio-mode -1) (balance-windows)
      :documentation "Resize the focused window using the golden ratio."
      :evil-leader "tg")
    :config
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
                 windmove-down
                 spacemacs/alternate-window))
      (add-to-list 'golden-ratio-extra-commands f))

    ;; golden-ratio-exclude-buffer-names
    (dolist (n '(" *NeoTree*"
                 "*LV*"
                 " *which-key*"))
      (add-to-list 'golden-ratio-exclude-buffer-names n))

    ;; golden-ratio-inhibit-functions
    (dolist (f '(spacemacs/no-golden-ratio-guide-key
                 spacemacs//ediff-in-comparison-buffer-p))
      (add-to-list 'golden-ratio-inhibit-functions f))

    (add-hook 'ediff-startup-hook 'spacemacs/ediff-balance-windows)

    (spacemacs|diminish golden-ratio-mode " ⓖ" " g")))

(defun spacemacs-navigation/init-grep ()
  (use-package grep
    :defer t
    :config
    (define-key grep-mode-map "h" nil)))

(defun spacemacs-navigation/init-info+ ()
  (use-package info+
    :defer t
    :init
    (spacemacs/set-leader-keys "hj" 'info-display-manual)
    (setq Info-fontify-angle-bracketed-flag nil)
    (with-eval-after-load "info" (require 'info+))))

(defun spacemacs-navigation/init-open-junk-file ()
  (use-package open-junk-file
    :defer t
    :commands (open-junk-file)
    :init
    (setq open-junk-file-format (concat spacemacs-cache-directory "junk/%Y/%m/%d-%H%M%S."))
    (spacemacs/set-leader-keys "fJ" 'spacemacs/open-junk-file)
    ;; function to run open-junk-file hooks is buggy when opening a large file
    ;; and Emacs warns about it.
    ;; Since this is not really useful to add hooks to open-junk-files lets remove
    ;; it
    (remove-hook 'find-file-hook 'find-file-hook--open-junk-file)))

(defun spacemacs-navigation/init-paradox ()
  (use-package paradox
    :commands paradox-list-packages
    :init
    (setq paradox-execute-asynchronously nil)
    (spacemacs/set-leader-keys
      "ak" 'spacemacs/paradox-list-packages)
    :config
    (evilified-state-evilify-map paradox-menu-mode-map
      :mode paradox-menu-mode
      :bindings
      "H" 'paradox-menu-quick-help
      "J" 'paradox-next-describe
      "K" 'paradox-previous-describe
      "L" 'paradox-menu-view-commit-list
      "o" 'paradox-menu-visit-homepage)))

(defun spacemacs-navigation/init-restart-emacs ()
  (use-package restart-emacs
    :defer (spacemacs/defer)
    :init
    (with-eval-after-load 'files
      ;; unbind `restart-emacs' and declare it from package for ticket #15505
      (fmakunbound 'restart-emacs)
      (autoload 'restart-emacs "restart-emacs"))

    (spacemacs/set-leader-keys
      "qd" 'spacemacs/restart-emacs-debug-init
      "qD" 'spacemacs/restart-stock-emacs-with-packages
      "qr" 'spacemacs/restart-emacs-resume-layouts
      "qR" 'spacemacs/restart-emacs
      "qt" 'spacemacs/restart-emacs-timed-requires
      "qT" 'spacemacs/restart-emacs-adv-timers)))

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

(defun spacemacs-navigation/init-symbol-overlay ()
  (use-package symbol-overlay
    :init
    (setq spacemacs--symbol-overlay-transient-state-doc "
%s
 [_n_] next   [_N_/_p_] prev      [_d_] def           [_f_/_b_] switch [_t_] scope
 [_e_] echo   [_o_]^^   unoverlay [_O_] unoverlay all [_c_]^^   copy   [_z_] center
 [_s_] search [_r_]^^   replace   [_R_] rename        ^^^^             [_q_] quit")

    ;; since we are creating our own maps,
    ;; prevent the default keymap from getting created
    (setq symbol-overlay-map (make-sparse-keymap))
    :config
    (spacemacs/set-leader-keys
      "so" 'spacemacs/symbol-overlay
      "sO" 'symbol-overlay-remove-all)

    ;; transient state
    (spacemacs|define-transient-state symbol-overlay
      :title "Symbol Overlay Transient State"
      :hint-is-doc t
      :dynamic-hint (spacemacs//symbol-overlay-ts-doc)
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
      ("q" nil :exit t))))

(defun spacemacs-navigation/init-winum ()
  (use-package winum
    :config
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-auto-setup-mode-line (eq dotspacemacs-mode-line-theme 'vanilla)
          winum-ignored-buffers '(" *LV*" " *which-key*"))
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
    (winum-mode)))
