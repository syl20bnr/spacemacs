;;; config.el --- Language Server Protocol Layer config file for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar lsp-remap-xref-keybindings nil "When non-nil, xref keybindings remapped to lsp-ui-peek-find-*")
(defvar lsp-navigation 'both
  "If `simple' binds lightweight navigation functions under `SPC m g'.
If `peek' binds lsp-ui navigation functions under `SPC m g'.
If `both', binds lightweight navigation functions under `SPC m g' and lsp-ui functions under `SPC m G'")

(defvar lsp-prefer-flymake nil
  "If nil, prefer the lsp flycheck checker.
If non-nil, prefer the lsp flymake checker.
If :none, use neither flycheck nor flymake.")

;; These are config variables exposed by the lsp-ui package
;; They all have toggles bound under 't' in spacemacs/lsp-define-keys-for-mode
(defvar lsp-ui-doc-enable t "Enable/disable lsp-ui-doc overlay")
(defvar lsp-ui-doc-include-signature nil "When non-nil, type signature included in the lsp-ui-doc overlay")
(defvar lsp-ui-sideline-enable t "Enable/disable lsp-ui-sideline overlay")
(defvar lsp-ui-sideline-show-symbol nil "When non-nil, sideline includes symbol info (largely redundant for c modes)")  ; don't show symbol on the right of info
(defvar lsp-ui-sideline-ignore-duplicate t "Ignore duplicates")
(defvar lsp-layer--active-mode-list nil "internal variable to store active major modes")
