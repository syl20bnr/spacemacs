;;; funcs.el --- C/C++ Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'cl-lib)
(require 'subr-x)

(defun spacemacs//c-c++-backend ()
  "Returns selected backend."
  (if c-c++-backend
      c-c++-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp-clangd)
     (t nil))))

(defun spacemacs//c-c++-setup-backend ()
  "Conditionally setup c-c++ backend."
  (pcase (spacemacs//c-c++-backend)
    (`lsp-clangd (spacemacs//c-c++-setup-lsp-clangd))
    (`lsp-ccls (spacemacs//c-c++-setup-lsp-ccls))
    (`rtags (spacemacs//c-c++-setup-rtags))
    (`ycmd (spacemacs//c-c++-setup-ycmd))))

(defun spacemacs//c-c++-setup-company ()
  "Conditionally setup C/C++ company integration based on backend."
  (pcase (spacemacs//c-c++-backend)
    (`rtags (spacemacs//c-c++-setup-rtags-company))
    (`ycmd (spacemacs//c-c++-setup-ycmd-company))))

(defun spacemacs//c-c++-setup-dap ()
  "Conditionally setup C/C++ DAP integration based on backend."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//c-c++-backend)
    (`lsp-clangd (spacemacs//c-c++-setup-lsp-dap))
    (`lsp-ccls (spacemacs//c-c++-setup-lsp-dap))))

(defun spacemacs//c-c++-setup-eldoc ()
  "Conditionally setup C/C++ eldoc integration based on backend."
  (pcase (spacemacs//c-c++-backend)
    ;; lsp setup eldoc on its own
    (`ycmd (spacemacs//c-c++-setup-ycmd-eldoc))))

(defun spacemacs//c-c++-setup-flycheck ()
  "Conditionally setup C/C++ flycheck integration based on backend."
  (pcase (spacemacs//c-c++-backend)
    (`rtags (spacemacs//c-c++-setup-rtags-flycheck))
    (`ycmd (spacemacs//c-c++-setup-ycmd-flycheck))))

(defun spacemacs//c-c++-setup-format ()
  "Conditionally setup format based on backend."
  (pcase (spacemacs//c-c++-backend)
    (`lsp-clangd (spacemacs//c-c++-setup-clang-format))
    (`lsp-ccls (spacemacs//c-c++-setup-clang-format))))

(defun spacemacs//c-c++-setup-semantic ()
  "Conditionally setup semantic based on backend."
  (pcase (spacemacs//c-c++-backend)
    (`rtags (spacemacs//c-c++-setup-rtags-semantic))
    (`ycmd (spacemacs//c-c++-setup-ycmd-semantic))))


;; lsp

;; clang

(defun spacemacs//c-c++-setup-lsp-clangd ()
  "Setup LSP clangd."
  ;; extensions
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-clangd"
   'clangd-other-file "textDocument/switchSourceHeader" 'buffer-file-name)
  (set (make-local-variable 'lsp-disabled-clients) '(ccls))
  (lsp))

;; ccls

(defun spacemacs//c-c++-setup-lsp-ccls ()
  "Setup LSP ccls."
  (require 'ccls)

  ;; semantic highlight
  (when c-c++-lsp-enable-semantic-highlight
    (setq ccls-sem-highlight-method c-c++-lsp-semantic-highlight-method)
    (when (eq 'rainbow c-c++-lsp-enable-semantic-highlight)
      (ccls-use-default-rainbow-sem-highlight)))

  ;; extensions
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'refs-address "textDocument/references" '(:role 128))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'refs-read "textDocument/references" '(:role 8))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'refs-write "textDocument/references" '(:role 16))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'callers "$ccls/call")
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'callees "$ccls/call" '(:callee t))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'base "$ccls/inheritance")

  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'derived "$ccls/inheritance" '(:derived t))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'member-types "$ccls/member" `(:kind 2))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'member-functions "$ccls/member" `(:kind 3))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'member-vars "$ccls/member" `(:kind 0))

  ;; key bindings
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode
      ;; backend
      "bf" 'ccls-reload
      "bp" 'ccls-preprocess-file
      ;; goto
      "gf" 'find-file-at-point
      "gF" 'ffap-other-window
      ;; hierarchy
      "ghc" 'ccls-call-hierarchy
      "ghC" 'spacemacs/c-c++-lsp-ccls-call-hierarchy-inv
      "ghi" 'ccls-inheritance-hierarchy
      "ghI" 'spacemacs/c-c++-lsp-ccls-inheritance-hierarchy-inv
      ;; members
      "gmh" 'ccls-member-hierarchy)

    (spacemacs/lsp-bind-extensions-for-mode
     mode "c-c++" "lsp-ccls"
     "&" 'refs-address
     "R" 'refs-read
     "W" 'refs-write
     "c" 'callers
     "C" 'callees
     "v" 'vars
     "hb" 'base)

    (spacemacs/lsp-bind-extensions-for-mode
     mode "c-c++" "lsp-ccls"
     "hd" 'derived
     "mt" 'member-types
     "mf" 'member-functions
     "mv" 'member-vars))

  ;;(evil-set-initial-state 'ccls--tree-mode 'emacs)
  ;;(evil-make-overriding-map 'ccls-tree-mode-map)
  (set (make-local-variable 'lsp-disabled-clients) '(clangd))
  (lsp))

(defun spacemacs//c-c++-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-gdb-lldb))


;; rtags

(defun spacemacs//c-c++-setup-rtags ()
  "Setup rtags backend."
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
      "gY" 'rtags-cycle-through-diagnostics)))

(defun spacemacs//c-c++-setup-rtags-company ()
  "Setup rtags auto-completion."
  (setq rtags-completions-enabled t)
  (spacemacs|add-company-backends
    :backends company-rtags
    :modes c-mode-common
    :append-hooks nil
    :call-hooks t))

(defun spacemacs//c-c++-setup-rtags-flycheck ()
  "Setup rtags syntax checking."
  (when (or (spacemacs/enable-flycheck 'c-mode)
            (spacemacs/enable-flycheck 'c++-mode))
    (require 'flycheck-rtags)
    (flycheck-mode)))

(defun spacemacs//c-c++-setup-rtags-semantic ()
  "Setup semantic for rtags."
  (semantic-mode)
  (spacemacs//disable-semantic-idle-summary-mode))

(defun spacemacs/c-c++-use-rtags (&optional useFileManager)
  "Return non-nil if rtags function should be used."
  ;; this function is used to fallback on gtags function if rtags is not
  ;; supported. So if gtags layer is not used we disable the fallback by
  ;; returning always t.
  (or (not (configuration-layer/layer-used-p 'gtags))
      (and (rtags-executable-find "rc")
           (cond ((not (gtags-get-rootpath)) t)
                 ((and (not (eq major-mode 'c++-mode))
                       (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
                 (useFileManager (rtags-has-filemanager))
                 (t (rtags-is-indexed))))))

(defun spacemacs/c-c++-tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-symbol-at-point prefix))
           rtags-last-request-not-indexed)
      (gtags-find-tag)))

(defun spacemacs/c-c++-tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-references-at-point prefix))
           rtags-last-request-not-indexed)
      (gtags-find-rtag)))

(defun spacemacs/c-c++-tags-find-symbol ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags)
                          'rtags-find-symbol 'gtags-find-symbol)))

(defun spacemacs/c-c++-tags-find-references ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags)
                          'rtags-find-references 'gtags-find-rtag)))

(defun spacemacs/c-c++-tags-find-file ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags t)
                          'rtags-find-file 'gtags-find-file)))

(defun spacemacs/c-c++-tags-imenu ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags t)
                          'rtags-imenu 'idomenu)))


;; ycmd

(defun spacemacs//c-c++-setup-ycmd ()
  "Setup ycmd backend."
  (add-to-list 'spacemacs-jump-handlers-c++-mode '(ycmd-goto :async t))
  (add-to-list 'spacemacs-jump-handlers-c-mode '(ycmd-goto :async t))
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode
      "gG" 'ycmd-goto-imprecise))
  (ycmd-mode))

(defun spacemacs//c-c++-setup-ycmd-company ()
  "Setup ycmd auto-completion."
  (spacemacs|add-company-backends
    :backends company-ycmd
    :modes c-mode-common
    :append-hooks nil
    :call-hooks t))

(defun spacemacs//c-c++-setup-ycmd-eldoc ()
  "Setup ycmd eldoc integration."
  (ycmd-eldoc-setup))

(defun spacemacs//c-c++-setup-ycmd-flycheck ()
  "Setup ycmd syntax checking."
  (when (or (spacemacs/enable-flycheck 'c-mode)
            (spacemacs/enable-flycheck 'c++-mode))
    (flycheck-ycmd-setup)
    (flycheck-mode)))

(defun spacemacs//c-c++-setup-ycmd-semantic ()
  "Setup semantic for ycmd."
  (semantic-mode))


;; style

(defun spacemacs//c-toggle-auto-newline ()
  "Toggle auto-newline."
  (c-toggle-auto-newline 1))


;; clang

(defun spacemacs//c-c++-setup-clang-format ()
  "Setup clang format."
  (when c-c++-enable-clang-format-on-save
    (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save c-c++-mode-hooks))
  (dolist (mode c-c++-modes)
    (spacemacs/declare-prefix-for-mode mode "m=" "format")
    (spacemacs/set-leader-keys-for-major-mode mode
      "==" 'spacemacs/clang-format-region-or-buffer
      "=f" 'spacemacs/clang-format-function)))

(defun spacemacs/clang-format-function (&optional style)
  "Format the current function with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (c-mark-function)
    (clang-format (region-beginning) (region-end) style)
    (deactivate-mark) ; If the function is already formatted, then remove the mark
    (message "Formatted function %s" (c-defun-name))))

(defun spacemacs/clang-format-region-or-buffer (&optional style)
  "Format the current region or buffer with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (clang-format-region (region-beginning) (region-end) style)
          (message "Formatted region"))
      (progn
        (clang-format-buffer style)
        (message "Formatted buffer %s" (buffer-name))))))

(defun spacemacs//clang-format-on-save ()
  "Format the current buffer with clang-format on save when
`c-c++-enable-clang-format-on-save' is non-nil."
  (when c-c++-enable-clang-format-on-save
    (spacemacs/clang-format-region-or-buffer)))

(defun spacemacs/clang-format-on-save ()
  "Add before-save hook for clang-format."
  (add-hook 'before-save-hook 'spacemacs//clang-format-on-save nil t))


;; ccls

(defun spacemacs/c-c++-lsp-ccls-call-hierarchy-inv ()
  (interactive)
  (ccls-call-hierarchy t))

(defun spacemacs/c-c++-lsp-ccls-inheritance-hierarchy-inv ()
  (interactive)
  (ccls-inheritance-hierarchy t))


;; cpp-auto-include

(defalias 'spacemacs/c++-organize-includes 'cpp-auto-include)

(defun spacemacs//c++-organize-includes-on-save ()
  "Organize the includes on save when `c++-enable-organize-includes-on-save'
is non-nil."
  (when c++-enable-organize-includes-on-save
    (spacemacs/c++-organize-includes)))

(defun spacemacs/c++-organize-includes-on-save ()
  "Add before-save hook for c++-organize-includes."
  (add-hook 'before-save-hook
            #'spacemacs//c++-organize-includes-on-save nil t))
