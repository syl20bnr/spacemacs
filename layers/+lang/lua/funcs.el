;;; funcs.el --- Lua Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Lin Sun <sunlin7@yahoo.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//lua-setup-backend ()
  "Conditionally setup lua backend."
  (setq lua-indent-level 2
        lua-indent-string-contents t)
  (space-macs/declare-prefix-for-mode 'lua-mode "mh" "help")
  (space-macs/declare-prefix-for-mode 'lua-mode "ms" "REPL")
  (space-macs/declare-prefix-for-mode 'lua-mode "mg" "goto")
  (space-macs/set-leader-keys-for-major-mode 'lua-mode
    "hd" 'lua-search-documentation
    "sb" 'lua-send-buffer
    "sf" 'lua-send-defun
    "sl" 'lua-send-current-line
    "sr" 'lua-send-region
    "'" 'lua-show-process-buffer)
  (pcase lua-backend
    (`lsp-emmy (space-macs//lua-setup-lsp-emmy))))

(defun space-macs//lua-setup-company ()
  "Conditionally setup company based on backend."
  (pcase lua-backend
    (_ (space-macs//lua-setup-company-lua))))


;; LSP Lua
(defun space-macs//lua-setup-lsp-emmy ()
  "Setup LSP Lua."
  (when lua-lsp-emmy-java-path
    (setq lsp-clients-emmy-lua-java-path lua-lsp-emmy-java-path))
  (when lua-lsp-emmy-jar-path
    (setq lsp-clients-emmy-lua-jar-path (expand-file-name lua-lsp-emmy-jar-path)))
  (setq lsp-enable-file-watchers lua-lsp-emmy-enable-file-watchers)
  (lsp))


;; Lua mode
(defun space-macs//lua-setup-company-lua ()
  (space-macs|add-company-backends
    :backends company-lua
    :modes lua-mode)
  (company-mode))


