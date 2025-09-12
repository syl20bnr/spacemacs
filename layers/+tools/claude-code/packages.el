;;; packages.el --- Claude Code Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: sunlin7 <sunlin7 AT hotmail.com>
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


(defconst claude-code-packages
  '((claude-code-ide :location (recipe
                                 :fetcher github
                                 :repo "manzaltu/claude-code-ide.el")
                     :requires transient)))

(defun claude-code/init-claude-code-ide ()
  "Initialize Claude Code IDE package."
  (use-package claude-code-ide
    :defer t
    :init
    (spacemacs/declare-prefix "$d" "Claude")
    (spacemacs/declare-prefix "$dd" "Debug")
    (spacemacs/set-leader-keys
      "$dy" 'claude-code-ide-menu
      ;; Session Management
      "$ds" 'claude-code-ide
      "$dc" 'claude-code-ide-continue
      "$dr" 'claude-code-ide-resume
      "$dq" 'claude-code-ide-stop
      "$dl" 'claude-code-ide-list-sessions
      ;;  Navigation
      "$db" 'claude-code-ide-switch-to-buffer
      "$db" 'claude-code-ide-toggle-window
      ;; Interaction
      "$di" 'claude-code-ide-insert-at-mentioned
      "$dp" 'claude-code-ide-send-prompt
      "$de" 'claude-code-ide-send-escape
      "$dn" 'claude-code-ide-insert-newline
      ;; Submenus for configuration and debuging
      ;; The config function are private functions, use
      ;; `claude-code-ide-config-menu' as entry point
      "$dC" 'claude-code-ide-config-menu
      ;; Debug Status
      "$ddS" 'claude-code-ide-check-status
      "$ddv" 'claude-code-ide-show-version-info
      ;; Debug Settings
      "$ddd" 'claude-code-ide-toggle-debug-mode
      ;; Debug Logs
      "$ddl" 'claude-code-ide-show-debug
      "$ddc" 'claude-code-ide-clear-debug
      ;; Debug MCP Server
      "$ddm" 'claude-code-ide-show-mcp-sessions
      "$ddp" 'claude-code-ide-show-active-ports
      ;; Assistant functions
      "$d*" 'spacemacs/claude-code-explain-symbol-at-point)))
