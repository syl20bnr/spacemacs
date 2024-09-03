;;; packages.el --- C/C++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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


(defconst c-c++-packages
  '(
    cc-mode
    clang-format
    company
    (company-c-headers :requires company)
    cpp-auto-include
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
    xcscope
    ;; lsp
    (ccls :requires lsp-mode)
    dap-mode
    ;; rtags
    (company-rtags :requires (company rtags))
    counsel-gtags
    (flycheck-rtags :requires (flycheck rtags))
    ggtags
    (helm-rtags :requires (helm rtags))
    (ivy-rtags :requires (ivy rtags))
    rtags
    ;; ycmd
    (company-ycmd :requires company)
    (flycheck-ycmd :requires flycheck)
    (gendoxy :location (recipe
                        :fetcher github
                        :repo "cormacc/gendoxy"
                        :branch "provides"))
    ycmd))

(defun c-c++/init-gendoxy ()
  "Initialise gendoxy (doxygen package)"
  (use-package gendoxy
    :defer t
    :init (dolist (mode c-c++-modes)
            (spacemacs/declare-prefix-for-mode mode "mi" "insert")
            (spacemacs/set-leader-keys-for-major-mode mode
              "ih" 'gendoxy-header
              "id" 'gendoxy-tag
              "iD" 'gendoxy-tag-header
              "ig" 'gendoxy-group
              "iG" 'gendoxy-group-header
              "is" 'gendoxy-group-start
              "ie" 'gendoxy-group-end))))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-backend)
    (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-backend)
    (put 'c-c++-backend 'safe-local-variable 'symbolp)
    (when c-c++-default-mode-for-headers
      (add-to-list 'auto-mode-alist
                   `("\\.h\\'" . ,c-c++-default-mode-for-headers)))
    (when c-c++-enable-auto-newline
      (add-hook 'c-mode-common-hook 'spacemacs//c-toggle-auto-newline))
    :config
    (require 'compile)
    (dolist (mode c-c++-modes)
      (spacemacs/declare-prefix-for-mode mode "mc" "compile")
      (spacemacs/declare-prefix-for-mode mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode mode "mp" "project")
      (spacemacs/set-leader-keys-for-major-mode mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window))))

(defun c-c++/init-ccls ()
  (use-package ccls
    :defer t))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :defer t
    :init (spacemacs//c-c++-setup-clang-format)))

(defun c-c++/post-init-company ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-company)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-company))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

(defun c-c++/init-company-rtags ()
  (use-package company-rtags
    :defer t))

(defun c-c++/init-company-ycmd ()
  (use-package company-ycmd
    :defer t
    :commands company-ycmd))

(defun c-c++/post-init-counsel-gtags nil)

(defun c-c++/init-cpp-auto-include ()
  (use-package cpp-auto-include
    :defer t
    :init
    (when c-c++-enable-organize-includes-on-save
      (add-hook 'c++-mode-hook #'spacemacs/c-c++-organize-includes-on-save))

    (spacemacs/declare-prefix-for-mode 'c++-mode
      "mr" "refactor")

    (spacemacs/set-leader-keys-for-major-mode 'c++-mode
      "ri" #'spacemacs/c-c++-organize-includes)))

(defun c-c++/pre-init-dap-mode ()
  (pcase c-c++-backend
    ('lsp-clangd (add-to-list 'spacemacs--dap-supported-modes 'c-mode)
                 (add-to-list 'spacemacs--dap-supported-modes 'c++-mode))
    ('lsp-ccls (add-to-list 'spacemacs--dap-supported-modes 'c-mode)
               (add-to-list 'spacemacs--dap-supported-modes 'c++-mode)))
  (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-dap)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-dap))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (dolist (mode c-c++-modes)
      (spacemacs/set-leader-keys-for-major-mode mode
        "D" 'disaster))))

(defun c-c++/post-init-eldoc ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-eldoc)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-eldoc))

(defun c-c++/post-init-flycheck ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-flycheck)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-flycheck))

(defun c-c++/init-flycheck-rtags ()
  (use-package flycheck-rtags
    :defer t))

(defun c-c++/init-flycheck-ycmd ()
  (use-package flycheck-ycmd
    :defer t))

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

(defun c-c++/init-google-c-style ()
  (use-package google-c-style
    :defer t
    :init
    (when c-c++-enable-google-style
      (add-hook 'c-mode-common-hook 'google-set-c-style))
    (when c-c++-enable-google-newline
      (add-hook 'c-mode-common-hook 'google-make-newline-indent))))

(defun c-c++/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode c-c++-modes)
      (spacemacs/setup-helm-cscope mode))))

(defun c-c++/init-helm-rtags ()
  (use-package helm-rtags
    :defer t
    :init (setq rtags-display-result-backend 'helm)))

(defun c-c++/init-ivy-rtags ()
  (use-package ivy-rtags
    :defer t
    :init (setq rtags-display-result-backend 'ivy)))

(defun c-c++/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(C . t))))

(defun c-c++/pre-init-projectile ()
  (spacemacs|use-package-add-hook projectile
    :post-config
    (when c-c++-adopt-subprojects
      (setq projectile-project-root-files-top-down-recurring
            (append '("compile_commands.json"
                      ".ccls")
                    projectile-project-root-files-top-down-recurring)))))

(defun c-c++/init-rtags ()
  ;; config in `funcs.el'
  (use-package rtags
    :defer t))

(defun c-c++/post-init-realgud ()
  (dolist (mode c-c++-modes)
    (spacemacs/add-realgud-debugger mode "gdb")))

(defun c-c++/post-init-semantic ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs//c-c++-setup-semantic)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs//c-c++-setup-semantic))

(defun c-c++/post-init-srefactor ()
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode "r." 'srefactor-refactor-at-point))
  (spacemacs/add-to-hooks 'spacemacs/load-srefactor c-c++-mode-hooks))

(defun c-c++/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode c-c++-modes)
      (spacemacs/set-leader-keys-for-major-mode mode "gi" 'cscope-index-files))))

(defun c-c++/init-ycmd ()
  (use-package ycmd
    :defer t
    :init
    (unless (boundp 'ycmd-global-config)
      (setq-default ycmd-global-config
                    (concat (configuration-layer/get-layer-path 'ycmd)
                            "global_conf.py")))
    (setq-default ycmd-parse-conditions '(save mode-enabled))))
