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

;; ;; These all have toggles bound under 't' in spacemacs/lsp-define-keys-for-mode
(defvar lsp-ui-remap-xref-keybindings nil "When non-nil, xref keybindings remapped to lsp-ui-peek-find-*")
(defvar lsp-ui-doc-enable t "Enable/disable lsp-ui-doc overlay")
(defvar lsp-ui-doc-include-signature nil "When non-nil, type signature included in the lsp-ui-doc overlay")
(defvar lsp-ui-sideline-enable t "Enable/disable lsp-ui-sideline overlay")
(defvar lsp-ui-sideline-show-symbol nil "When non-nil, sideline includes symbol info (largely redundant for c modes)")  ; don't show symbol on the right of info
(defvar lsp-ui-sideline-ignore-duplicate t "Ignore duplicates")
