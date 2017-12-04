;;; packages.el --- C/C++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq c-c++-packages
  '(
    cc-mode
    disaster
    clang-format
    cmake-ide
    cmake-mode
    company
    (company-c-headers :requires company)
    company-ycmd
    flycheck
    gdb-mi
    ggtags
    counsel-gtags
    google-c-style
    helm-cscope
    helm-gtags
    realgud
    semantic
    srefactor
    stickyfunc-enhance
    ycmd
    xcscope
    rtags
    (company-rtags :requires company rtags)
    (flycheck-rtags :requires flycheck)
    (helm-rtags :requires helm)
    ))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist
                   `("\\.h\\'" . ,c-c++-default-mode-for-headers)))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (dolist (mode c-c++-modes)
        (spacemacs/declare-prefix-for-mode mode "mc" "compile")
        (spacemacs/declare-prefix-for-mode mode "mg" "goto")
        (spacemacs/declare-prefix-for-mode mode "mp" "project")
        (spacemacs/set-leader-keys-for-major-mode mode
          "ga" 'projectile-find-other-file
          "gA" 'projectile-find-other-file-other-window)))))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (dolist (mode c-c++-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "D" 'disaster)))))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :if c-c++-enable-clang-support
    :init
    (progn
      (when c-c++-enable-clang-format-on-save
        (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save c-c++-mode-hooks))
      (dolist (mode c-c++-modes)
        (spacemacs/declare-prefix-for-mode mode "m=" "format")
        (spacemacs/set-leader-keys-for-major-mode mode
          "==" 'spacemacs/clang-format-region-or-buffer
          "=f" 'spacemacs/clang-format-function)))))

(defun c-c++/init-cmake-ide ()
  (use-package cmake-ide
    :if c-c++-enable-cmake-ide-support
    :config
    (progn
      (cmake-ide-setup)
      (dolist (mode c-c++-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "cc" 'cmake-ide-compile
          "pc" 'cmake-ide-run-cmake
          "pC" 'cmake-ide-maybe-run-cmake
          "pd" 'cmake-ide-delete-file)))))

(defun c-c++/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))))

(defun c-c++/post-init-company ()
  (when (configuration-layer/package-used-p 'cmake-mode)
    (spacemacs|add-company-backends :backends company-cmake :modes cmake-mode))
  (when c-c++-enable-clang-support
    (spacemacs|add-company-backends :backends company-clang
      :modes c-mode-common)
    (when c-c++-enable-c++11
      (setq company-clang-arguments '("-std=c++11")))
    (setq company-clang-prefix-guesser 'spacemacs/company-more-than-prefix-guesser)
    (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args c-c++-mode-hooks)))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

(defun c-c++/post-init-flycheck ()
  (dolist (mode c-c++-modes)
    (spacemacs/enable-flycheck mode))
  (when c-c++-enable-clang-support
    (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args c-c++-mode-hooks)
    (when c-c++-enable-c++11
      (setq flycheck-clang-language-standard "c++11"))))

(defun c-c++/post-init-ggtags ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun c-c++/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun c-c++/post-init-counsel-gtags ()
  (dolist (mode c-c++-modes)
    (spacemacs/counsel-gtags-define-keys-for-mode mode)))

(defun c-c++/post-init-helm-gtags ()
  (dolist (mode c-c++-modes)
    (spacemacs/helm-gtags-define-keys-for-mode mode)))

(defun c-c++/init-realgud()
  (use-package realgud
    :defer t
    :commands (realgud:gdb)
    :init
    (progn
      (dolist (mode c-c++-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "dd" 'realgud:gdb
          "de" 'realgud:cmd-eval-dwim))
      (advice-add 'realgud-short-key-mode-setup
                  :before #'spacemacs//short-key-state)
      (evilified-state-evilify-map realgud:shortkey-mode-map
        :eval-after-load realgud
        :mode realgud-short-key-mode
        :bindings
        "s" 'realgud:cmd-next
        "i" 'realgud:cmd-step
        "b" 'realgud:cmd-break
        "B" 'realgud:cmd-clear
        "o" 'realgud:cmd-finish
        "c" 'realgud:cmd-continue
        "e" 'realgud:cmd-eval
        "r" 'realgud:cmd-restart
        "q" 'realgud:cmd-quit
        "S" 'realgud-window-cmd-undisturb-src))))

(defun c-c++/init-google-c-style ()
  (use-package google-c-style
    :if (or 'c-c++-enable-google-style 'c-c++-enable-google-newline)
    :config (progn
    (when 'c-c++-enable-google-style (add-hook 'c-mode-common-hook 'google-set-c-style))
    (when 'c-c++-enable-google-newline (add-hook 'c-mode-common-hook 'google-set-c-style)))))

(defun c-c++/post-init-semantic ()
  (spacemacs/add-to-hooks 'semantic-mode c-c++-mode-hooks))

(defun c-c++/post-init-srefactor ()
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode "r" 'srefactor-refactor-at-point))
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-srefactor c-c++-mode-hooks))

(defun c-c++/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-stickyfunc-enhance c-c++-mode-hooks))

(defun c-c++/post-init-ycmd ()
  (spacemacs/add-to-hooks 'ycmd-mode c-c++-mode-hooks)
  (add-to-list 'spacemacs-jump-handlers-c++-mode '(ycmd-goto :async t))
  (add-to-list 'spacemacs-jump-handlers-c-mode '(ycmd-goto :async t))
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode
      "gG" 'ycmd-goto-imprecise)))

(defun c-c++/post-init-company-ycmd ()
  (spacemacs|add-company-backends :backends company-ycmd :modes c-mode-common))

(defun c-c++/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode c-c++-modes)
      (spacemacs/set-leader-keys-for-major-mode mode "gi" 'cscope-index-files))))

(defun c-c++/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode c-c++-modes)
      (spacemacs/setup-helm-cscope mode))))
;;;
;;; Adapted from comment:
;;; https://github.com/syl20bnr/spacemacs/issues/2327#issuecomment-153283156
;;; by user
;;; https://github.com/autosquid
;;;
(defun rtags-major-mode-keybindings (mode)
  (spacemacs/declare-prefix-for-mode mode "me" "refactor")
  (spacemacs/set-leader-keys-for-major-mode mode
    "e." 'rtags-find-symbol-at-point
    "e," 'rtags-find-references-at-point
    "ev" 'rtags-find-virtuals-at-point
    "eV" 'rtags-print-enum-value-at-point
    "e/" 'rtags-find-all-references-at-point
    "eY" 'rtags-cycle-overlays-on-screen
    "e>" 'rtags-find-symbol
    "e<" 'rtags-find-references
    "e[" 'rtags-location-stack-back
    "e]" 'rtags-location-stack-forward
    "eD" 'rtags-diagnostics
    "eG" 'rtags-guess-function-at-point
    "ep" 'rtags-set-current-project
    "eP" 'rtags-print-dependencies
    "ee" 'rtags-reparse-file
    "eE" 'rtags-preprocess-file
    "eR" 'rtags-rename-symbol
    "eM" 'rtags-symbol-info
    "eS" 'rtags-display-summary
    "eO" 'rtags-goto-offset
    "e;" 'rtags-find-file
    "eF" 'rtags-fixit
    "eL" 'rtags-copy-and-print-current-location
    "eX" 'rtags-fix-fixit-at-point
    "eB" 'rtags-show-rtags-buffer
    "eI" 'rtags-imenu
    "eT" 'rtags-taglist
    "eh" 'rtags-print-class-hierarchy
    "ea" 'rtags-print-source-arguments
    ))

(defun c-c++/init-rtags ()
  (use-package rtags
    :if c-c++-enable-rtags-support
    :init
    (setq rtags-autostart-diagnostics t)
    (setq rtags-display-result-backend 'helm)
    (rtags-diagnostics)
    (define-key evil-normal-state-map (kbd "RET") 'rtags-select-other-window)
    (define-key evil-normal-state-map (kbd "M-RET") 'rtags-select)
    (define-key evil-normal-state-map (kbd "q") 'rtags-bury-or-delete)

    (rtags-major-mode-keybindings 'c-mode)
    (rtags-major-mode-keybindings 'c++-mode)
    ))

(defun c-c++/init-company-rtags ()
  (use-package company-rtags
    :if c-c++-enable-rtags-support
    :defer t
))

(defun c-c++/post-init-company-rtags ()
  (when  c-c++-enable-rtags-support
    (setq rtags-completions-enabled t)
    (spacemacs|add-company-backends
           :backends company-rtags
           :modes c-mode-common)))

(defun c-c++/init-flycheck-rtags ()
  (use-package flycheck-rtags
    :if c-c++-enable-rtags-support))

(defun c-c++/init-helm-rtags ()
  (use-package helm-rtags
    :if c-c++-enable-rtags-support))
