;;; packages.el --- C/C++ Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst c-c++-packages
  '(
    cc-mode
    clang-format
    company
    (company-c-headers :requires company)
    (cpp-auto-include
     :location (recipe :fetcher github
                       :repo "syohex/e-macs-cpp-auto-include"))
    disaster
    eldoc
    flycheck
    gdb-mi
    google-c-style
    helm-cscope
    org
    projectile
    realgud
    semantic
    srefactor
    stickyfunc-enhance
    xcscope
    ;; lsp
    (ccls :requires lsp-mode)
    dap-mode
    ;; rtags
    (company-rtags :requires (company rtags))
    counsel-gtags
    (flycheck-rtags :requires (flycheck rtags))
    ggtags
    helm-gtags
    (helm-rtags :requires (helm rtags))
    (ivy-rtags :requires (ivy rtags))
    rtags
    ;; ycmd
    (company-ycmd :requires company)
    (flycheck-ycmd :requires flycheck)
    ycmd))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-hook 'c-mode-local-vars-hook #'space-macs//c-c++-setup-backend)
      (add-hook 'c++-mode-local-vars-hook #'space-macs//c-c++-setup-backend)
      (put 'c-c++-backend 'safe-local-variable 'symbolp)
      (when c-c++-default-mode-for-headers
        (add-to-list 'auto-mode-alist
                     `("\\.h\\'" . ,c-c++-default-mode-for-headers)))
      (when c-c++-enable-auto-newline
        (add-hook 'c-mode-common-hook 'space-macs//c-toggle-auto-newline)))
    :config
    (progn
      (require 'compile)
      (dolist (mode c-c++-modes)
        (space-macs/declare-prefix-for-mode mode "mc" "compile")
        (space-macs/declare-prefix-for-mode mode "mg" "goto")
        (space-macs/declare-prefix-for-mode mode "mp" "project")
        (space-macs/set-leader-keys-for-major-mode mode
          "ga" 'projectile-find-other-file
          "gA" 'projectile-find-other-file-other-window)))))

(defun c-c++/init-ccls ()
  (use-package ccls
    :defer t))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :init (space-macs//c-c++-setup-clang-format)))

(defun c-c++/post-init-company ()
  (add-hook 'c-mode-local-vars-hook #'space-macs//c-c++-setup-company)
  (add-hook 'c++-mode-local-vars-hook #'space-macs//c-c++-setup-company))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (space-macs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

(defun c-c++/init-company-rtags ()
  (use-package company-rtags
    :defer t))

(defun c-c++/init-company-ycmd ()
  (use-package company-ycmd
    :defer t
    :commands company-ycmd))

(defun c-c++/post-init-counsel-gtags ()
  (dolist (mode c-c++-modes)
    (space-macs/counsel-gtags-define-keys-for-mode mode)))

(defun c-c++/init-cpp-auto-include ()
  (use-package cpp-auto-include
    :defer t
    :init
    (progn
      (when c++-enable-organize-includes-on-save
        (add-hook 'c++-mode-hook #'space-macs/c++-organize-includes-on-save))

      (space-macs/declare-prefix-for-mode 'c++-mode
        "mr" "refactor")
      (space-macs/set-leader-keys-for-major-mode 'c++-mode
        "ri" #'space-macs/c++-organize-includes))))

(defun c-c++/pre-init-dap-mode ()
  (pcase (space-macs//c-c++-backend)
    (`lsp-clangd (add-to-list 'space-macs--dap-supported-modes 'c-mode)
                 (add-to-list 'space-macs--dap-supported-modes 'c++-mode))
    (`lsp-ccls (add-to-list 'space-macs--dap-supported-modes 'c-mode)
               (add-to-list 'space-macs--dap-supported-modes 'c++-mode)))
  (add-hook 'c-mode-local-vars-hook #'space-macs//c-c++-setup-dap)
  (add-hook 'c++-mode-local-vars-hook #'space-macs//c-c++-setup-dap))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (dolist (mode c-c++-modes)
        (space-macs/set-leader-keys-for-major-mode mode
          "D" 'disaster)))))

(defun c-c++/post-init-eldoc ()
  (add-hook 'c-mode-local-vars-hook #'space-macs//c-c++-setup-eldoc)
  (add-hook 'c++-mode-local-vars-hook #'space-macs//c-c++-setup-eldoc))

(defun c-c++/post-init-flycheck ()
  (add-hook 'c-mode-local-vars-hook #'space-macs//c-c++-setup-flycheck)
  (add-hook 'c++-mode-local-vars-hook #'space-macs//c-c++-setup-flycheck))

(defun c-c++/init-flycheck-rtags ()
  (use-package flycheck-rtags
    :defer t))

(defun c-c++/init-flycheck-ycmd ()
  (use-package flycheck-ycmd
    :defer t))

(defun c-c++/post-init-ggtags ()
  (add-hook 'c-mode-local-vars-hook #'space-macs/ggtags-mode-enable)
  (add-hook 'c++-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun c-c++/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun c-c++/init-google-c-style ()
  (use-package google-c-style
    :defer t
    :init
    (progn
      (when c-c++-enable-google-style
        (add-hook 'c-mode-common-hook 'google-set-c-style))
      (when c-c++-enable-google-newline
        (add-hook 'c-mode-common-hook 'google-make-newline-indent)))))

(defun c-c++/pre-init-helm-cscope ()
  (space-macs|use-package-add-hook xcscope
    :post-init
    (dolist (mode c-c++-modes)
      (space-macs/setup-helm-cscope mode))))

(defun c-c++/post-init-helm-gtags ()
  (dolist (mode c-c++-modes)
    (space-macs/helm-gtags-define-keys-for-mode mode)))

(defun c-c++/init-helm-rtags ()
  (use-package helm-rtags
    :defer t
    :init (setq rtags-display-result-backend 'helm)))

(defun c-c++/init-ivy-rtags ()
  (use-package ivy-rtags
    :defer t
    :init (setq rtags-display-result-backend 'ivy)))

(defun c-c++/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(C . t))))

(defun c-c++/pre-init-projectile ()
  (space-macs|use-package-add-hook projectile
    :post-config
    (progn
      (when c-c++-adopt-subprojects
        (setq projectile-project-root-files-top-down-recurring
              (append '("compile_commands.json"
                        ".ccls")
                      projectile-project-root-files-top-down-recurring))))))

(defun c-c++/init-rtags ()
  ;; config in `funcs.el'
  (use-package rtags
    :defer t))

(defun c-c++/post-init-realgud()
  (dolist (mode c-c++-modes)
    (space-macs/add-realgud-debugger mode "gdb")))

(defun c-c++/post-init-semantic ()
  (add-hook 'c-mode-local-vars-hook #'space-macs//c-c++-setup-semantic)
  (add-hook 'c++-mode-local-vars-hook #'space-macs//c-c++-setup-semantic))

(defun c-c++/post-init-srefactor ()
  (dolist (mode c-c++-modes)
    (space-macs/set-leader-keys-for-major-mode mode "r." 'srefactor-refactor-at-point))
  (space-macs/add-to-hooks 'space-macs/load-srefactor c-c++-mode-hooks))

(defun c-c++/post-init-stickyfunc-enhance ()
  (space-macs/add-to-hooks 'space-macs/load-stickyfunc-enhance c-c++-mode-hooks))

(defun c-c++/pre-init-xcscope ()
  (space-macs|use-package-add-hook xcscope
    :post-init
    (dolist (mode c-c++-modes)
      (space-macs/set-leader-keys-for-major-mode mode "gi" 'cscope-index-files))))

(defun c-c++/init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (progn
      (unless (boundp 'ycmd-global-config)
        (setq-default ycmd-global-config
                      (concat (configuration-layer/get-layer-path 'ycmd)
                              "global_conf.py")))
      (setq-default ycmd-parse-conditions '(save mode-enabled)))))


