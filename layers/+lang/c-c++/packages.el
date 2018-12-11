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
        clang-format
        company
        (company-c-headers :requires company)
        (company-rtags :requires company rtags)
        company-ycmd
        counsel-gtags
        disaster
        flycheck
        (flycheck-rtags :requires flycheck rtags)
        gdb-mi
        ggtags
        google-c-style
        helm-cscope
        helm-gtags
        (helm-rtags :requires helm rtags)
        (ivy-rtags :requires ivy rtags)
        org
        realgud
        rtags
        semantic
        srefactor
        stickyfunc-enhance
        xcscope
        ycmd
        ;;lsp-backend
        (cquery :requires lsp-mode)
        (ccls :requires lsp-mode)
        projectile
        ))


(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist
        `("\\.h\\'" . ,c-c++-default-mode-for-headers))
      (when c-c++-enable-auto-newline
        (add-hook 'c-mode-common-hook 'spacemacs//c-toggle-auto-newline)))
    :config
    (progn
      (require 'compile)
      (dolist (mode c-c++-modes)
        (spacemacs/declare-prefix-for-mode mode "mc" "compile")
        (spacemacs/declare-prefix-for-mode mode "mg" "goto")
        (spacemacs/declare-prefix-for-mode mode "mp" "project")
        (spacemacs/set-leader-keys-for-major-mode mode
          "ga" 'projectile-find-other-file
          "gA" 'projectile-find-other-file-other-window)))))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :if (or c-c++-enable-clang-support (spacemacs//c-c++-lsp-enabled))
    :init
    (progn
      (when c-c++-enable-clang-format-on-save
        (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save c-c++-mode-hooks))
      (dolist (mode c-c++-modes)
        (spacemacs/declare-prefix-for-mode mode "m=" "format")
        (spacemacs/set-leader-keys-for-major-mode mode
          "==" 'spacemacs/clang-format-region-or-buffer
          "=f" 'spacemacs/clang-format-function)))))

(defun c-c++/post-init-company ()
  (when (configuration-layer/package-used-p 'cmake-mode)
    (spacemacs|add-company-backends :backends company-cmake :modes cmake-mode))
  (when c-c++-enable-clang-support
    (if (spacemacs//c-c++-lsp-enabled)
      (display-warning :error "`c-c++-enable-clang-support' ignored when using lsp backend")
      (progn
        (spacemacs|add-company-backends :backends company-clang :modes c-mode-common)
        (setq company-clang-prefix-guesser 'spacemacs/company-more-than-prefix-guesser)
        (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args c-c++-mode-hooks)
        ()))))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-c-headers
            :modes c-mode-common)))

(defun c-c++/init-company-rtags ()
  (use-package company-rtags
    :if (and (eq c-c++-backend 'rtags) c-c++-enable-rtags-completion)
    :defer t
    :init
    (progn
      (setq rtags-completions-enabled t)
      (spacemacs|add-company-backends
        :backends company-rtags
        :modes c-mode-common))))

(defun c-c++/post-init-company-ycmd ()
  (spacemacs|add-company-backends :backends company-ycmd :modes c-mode-common))

(defun c-c++/post-init-counsel-gtags ()
  (dolist (mode c-c++-modes)
    (spacemacs/counsel-gtags-define-keys-for-mode mode)))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (dolist (mode c-c++-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "D" 'disaster)))))

(defun c-c++/post-init-flycheck ()
  (dolist (mode c-c++-modes)
    (spacemacs/enable-flycheck mode))
  (when c-c++-enable-clang-support
    (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args c-c++-mode-hooks)))

;; TODO lazy load this package
(defun c-c++/init-flycheck-rtags ()
  (use-package flycheck-rtags
    :if (eq c-c++-backend 'rtags)))

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
    :if (or 'c-c++-enable-google-style 'c-c++-enable-google-newline)
    :config (progn
              (when c-c++-enable-google-style (add-hook 'c-mode-common-hook 'google-set-c-style))
              (when c-c++-enable-google-newline (add-hook 'c-mode-common-hook 'google-make-newline-indent)))))

(defun c-c++/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode c-c++-modes)
      (spacemacs/setup-helm-cscope mode))))

(defun c-c++/post-init-helm-gtags ()
  (dolist (mode c-c++-modes)
    (spacemacs/helm-gtags-define-keys-for-mode mode)))

;; TODO lazy load this package
(defun c-c++/init-helm-rtags ()
  (use-package helm-rtags
    :if (eq c-c++-backend 'rtags)
    :init (setq rtags-display-result-backend 'helm)))

;; TODO lazy load this package
(defun c-c++/init-ivy-rtags ()
  (use-package ivy-rtags
    :if (eq c-c++-backend 'rtags)
    :init (setq rtags-display-result-backend 'ivy)))

;; TODO lazy load this package
(defun c-c++/init-rtags ()
  (use-package rtags
    :if (eq c-c++-backend 'rtags)
    :init
    (progn
      (setq rtags-autostart-diagnostics t)
      (add-hook 'rtags-jump-hook 'evil-set-jump)
      (rtags-diagnostics)
      ;; key bindings
      (evil-define-key 'normal rtags-mode-map
        (kbd "RET")   'rtags-select-other-window
        (kbd "M-RET") 'rtags-select
        (kbd "q")     'rtags-bury-or-delete)
      ;; TODO check for consistency with gtags key bindings
      ;; see https://github.com/syl20bnr/spacemacs/blob/develop/layers/+tags/gtags/funcs.el#L70
      (dolist (mode c-c++-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "g." 'spacemacs/c-c++-tags-find-symbol-at-point
          "g," 'spacemacs/c-c++-tags-find-references-at-point
          "g;" 'spacemacs/c-c++-tags-find-file
          "g/" 'rtags-find-all-references-at-point
          "g[" 'rtags-location-stack-back
          "g]" 'rtags-location-stack-forward
          "g>" 'spacemacs/c-c++-tags-find-symbol
          "g<" 'spacemacs/c-c++-tags-find-references
          "gB" 'rtags-show-rtags-buffer
          "gd" 'rtags-print-dependencies
          "gD" 'rtags-diagnostics
          "ge" 'rtags-reparse-file
          "gE" 'rtags-preprocess-file
          "gF" 'rtags-fixit
          "gG" 'rtags-guess-function-at-point
          "gh" 'rtags-print-class-hierarchy
          "gI" 'spacemacs/c-c++-tags-imenu
          "gL" 'rtags-copy-and-print-current-location
          "gM" 'rtags-symbol-info
          "gO" 'rtags-goto-offset
          "gp" 'rtags-set-current-project
          "gR" 'rtags-rename-symbol
          "gs" 'rtags-print-source-arguments
          "gS" 'rtags-display-summary
          "gT" 'rtags-taglist
          "gv" 'rtags-find-virtuals-at-point
          "gV" 'rtags-print-enum-value-at-point
          "gX" 'rtags-fix-fixit-at-point
          "gY" 'rtags-cycle-through-diagnostics)))))

(defun c-c++/post-init-realgud()
  (dolist (mode c-c++-modes)
    (spacemacs/add-realgud-debugger mode "gdb")))

(defun c-c++/post-init-semantic ()
  (spacemacs/add-to-hooks 'semantic-mode c-c++-mode-hooks))

(defun c-c++/post-init-srefactor ()
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode "r." 'srefactor-refactor-at-point))
  (spacemacs/add-to-hooks 'spacemacs/load-srefactor c-c++-mode-hooks))

(defun c-c++/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/load-stickyfunc-enhance c-c++-mode-hooks))

(defun c-c++/post-init-ycmd ()
  (spacemacs/add-to-hooks 'ycmd-mode c-c++-mode-hooks)
  (add-to-list 'spacemacs-jump-handlers-c++-mode '(ycmd-goto :async t))
  (add-to-list 'spacemacs-jump-handlers-c-mode '(ycmd-goto :async t))
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode
      "gG" 'ycmd-goto-imprecise)))

(defun c-c++/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(C . t))))

(defun c-c++/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode c-c++-modes)
      (spacemacs/set-leader-keys-for-major-mode mode "gi" 'cscope-index-files))))

;; BEGIN LSP BACKEND PACKAGES
;; See also https://github.com/cquery-project/cquery/wiki/Emacs
(defun c-c++/init-cquery ()
  (use-package cquery
    :if (eq c-c++-backend 'lsp-cquery)
    :config
    (spacemacs//c-c++-lsp-config)
    :hook ((c-mode c++-mode) .
            (lambda () (cl-pushnew #'company-lsp company-backends) (require 'cquery) (remhash 'clangd lsp-clients) (lsp)))))

;; See also https://github.com/MaskRay/ccls/wiki/Emacs
(defun c-c++/init-ccls ()
  (use-package ccls
    :if (eq c-c++-backend 'lsp-ccls)
    :config
    (spacemacs//c-c++-lsp-config)
    :hook ((c-mode c++-mode) .
            (lambda () (cl-pushnew #'company-lsp company-backends) (require 'ccls) (remhash 'clangd lsp-clients) (lsp)))))

;;Intentionally adding both cquery and ccls cache dirs to ignore list, to facilitate switching between
;;two without multiple caches polluting projectile find file results
(defun c-c++/pre-init-projectile ()
  (spacemacs|use-package-add-hook projectile
    :post-config
    (progn
      (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
      (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
      (when c-c++-lsp-cache-dir
        (add-to-list 'projectile-globally-ignored-directories c-c++-lsp-cache-dir))
      (when c-c++-adopt-subprojects
        (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                     ".cquery")
            projectile-project-root-files-top-down-recurring))))))

;; END LSP BACKEND PACKAGES
