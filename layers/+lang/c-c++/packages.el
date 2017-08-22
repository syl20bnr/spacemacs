;;; packages.el --- C/C++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
    cmake-mode
    company
    (company-c-headers :toggle (configuration-layer/package-usedp 'company))
    company-ycmd
    flycheck
    gdb-mi
    ggtags
    helm-cscope
    helm-gtags
    realgud
    semantic
    srefactor
    stickyfunc-enhance
    ycmd
    xcscope
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
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window))))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "D" 'disaster)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "D" 'disaster))))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :if c-c++-enable-clang-support
    :init
    (when c-c++-enable-clang-format-on-save
      (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save
                              '(c-mode-hook c++-mode-hook)))))

(defun c-c++/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))))

(defun c-c++/post-init-company ()
  (when (configuration-layer/package-usedp 'cmake-mode)
    (spacemacs|add-company-backends :backends company-cmake :modes cmake-mode))
  (when c-c++-enable-clang-support
    (spacemacs|add-company-backends :backends company-clang
      :modes c-mode-common)
    (setq company-clang-prefix-guesser 'spacemacs/company-more-than-prefix-guesser)
    (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args
                            '(c-mode-hook c++-mode-hook))))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

(defun c-c++/post-init-flycheck ()
  (dolist (mode '(c-mode c++-mode))
    (spacemacs/enable-flycheck mode))
  (when c-c++-enable-clang-support
    (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args '(c-mode-hook c++-mode-hook))))

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

(defun c-c++/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'c-mode)
  (spacemacs/helm-gtags-define-keys-for-mode 'c++-mode))

(defun c-c++/init-realgud()
  (use-package realgud
    :defer t
    :commands (realgud:gdb)
    :init
    (progn
      (dolist (mode '(c-mode c++-mode))
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

(defun c-c++/post-init-semantic ()
  (spacemacs/add-to-hooks 'semantic-mode '(c-mode-hook c++-mode-hook)))

(defun c-c++/post-init-srefactor ()
  (spacemacs/set-leader-keys-for-major-mode 'c-mode "r" 'srefactor-refactor-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "r" 'srefactor-refactor-at-point)
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-srefactor '(c-mode-hook c++-mode-hook)))

(defun c-c++/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-stickyfunc-enhance '(c-mode-hook c++-mode-hook)))

(defun c-c++/post-init-ycmd ()
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (add-hook 'c-mode-hook 'ycmd-mode)
  (add-to-list 'spacemacs-jump-handlers-c++-mode '(ycmd-goto :async t))
  (add-to-list 'spacemacs-jump-handlers-c-mode '(ycmd-goto :async t))
  (dolist (mode '(c++-mode c-mode))
    (spacemacs/set-leader-keys-for-major-mode mode
      "gG" 'ycmd-goto-imprecise)))

(defun c-c++/post-init-company-ycmd ()
  (spacemacs|add-company-backends :backends company-ycmd :modes c-mode-common))

(defun c-c++/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "gi" 'cscope-index-files))))

(defun c-c++/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/setup-helm-cscope mode))))
