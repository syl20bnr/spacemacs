;;; packages.el --- Codex Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2026 Sylvain Benner & Contributors
;;
;; Author: Spacemacs Maintainers
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


(defconst codex-packages
  '((codex-ide :location (recipe
                          :fetcher github
                          :repo "dgillis/emacs-codex-ide")
               :requires transient)))

(defun codex/init-codex-ide ()
  "Initialize Codex IDE package."
  (use-package codex-ide
    :defer t
    :init
    (spacemacs/declare-prefix "$x" "Codex")
    (spacemacs/declare-prefix "$xd" "Debug")
    (spacemacs/set-leader-keys
      "$xy" 'codex-ide-menu
      ;; Session Management
      "$xs" 'codex-ide
      "$xc" 'codex-ide-continue
      "$xr" 'codex-ide-reset-current-session
      "$xq" 'codex-ide-stop
      "$xl" 'codex-ide-status
      ;; Navigation
      "$xb" 'codex-ide-switch-to-buffer
      ;; Interaction
      "$xp" 'codex-ide-prompt
      "$xi" 'codex-ide-submit-image
      "$xI" 'codex-ide-submit-clipboard-image
      ;; Manage
      "$xL" 'codex-ide-session-buffer-list
      "$xD" 'codex-ide-session-diff-open
      ;; Submenus for configuration and debugging
      "$xC" 'codex-ide-agent-config-menu
      "$xdd" 'codex-ide-debug-menu
      "$xds" 'codex-ide-show-cli-info
      "$xdv" 'codex-ide-show-debug-info
      ;; Assistant functions
      "$x*" 'spacemacs/codex-explain-symbol-at-point)))
