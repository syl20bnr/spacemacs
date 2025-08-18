;;; packages.el --- Aider Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
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


(defconst aider-packages
  '((aidermacs :requires transient)))

(defun aider/init-aidermacs ()
  "Initialize required packages."
  (use-package aidermacs
    :defer t
    :init
    ;; follow the aidermacs transient menu to bind keys
    (spacemacs/declare-prefix "$a" "Aider")
    (spacemacs/set-leader-keys          ; the functions were autoload
      "$ay" 'aidermacs-transient-menu
      ;; Core
      "$aa" 'aidermacs-run
      "$a." 'aidermacs-run-in-current-dir
      "$a:" 'aidermacs-run-in-directory
      ;; Code Others
      "$ai" 'aidermacs-implement-todo
      "$at" 'aidermacs-write-unit-test
      "$aT" 'aidermacs-fix-failing-test-under-cursor)
    :config
    (spacemacs/set-leader-keys          ; the functions rely on aider instance
      "$ay" 'aidermacs-transient-menu
      ;; Core
      "$al" 'aidermacs-clear-chat-history
      "$as" 'aidermacs-reset
      "$ax" 'aidermacs-exit
      ;; Peersistent Modes
      "$a1" 'aidermacs-switch-to-code-mode
      "$a2" 'aidermacs-switch-to-ask-mode
      "$a3" 'aidermacs-switch-to-architect-mode
      "$a4" 'aidermacs-switch-to-help-mode
      ;; Utilities
      "$a^" 'aidermacs-magit-show-last-commit
      "$au" 'aidermacs-undo-last-commit
      "$aC" 'aidermacs-commit-with-auto-message
      "$aR" 'aidermacs-refresh-repo-map
      "$ah" 'aidermacs-show-output-history
      "$ao" 'aidermacs-change-model
      "$av" 'aidermacs-send-voice
      "$aW" 'aidermacs-web
      "$a?" 'aidermacs-help
      ;; File Actions
      "$af" 'aidermacs-add-file
      "$ap" 'aidermacs-add-project-file
      "$aF" 'aidermacs-add-current-file
      "$ad" 'aidermacs-add-same-type-files-under-dir
      "$aw" 'aidermacs-add-files-in-current-window
      "$am" 'aidermacs-batch-add-dired-marked-files
      ;; File Drop
      "$aj" 'aidermacs-drop-file
      "$aJ" 'aidermacs-drop-current-file
      "$ak" 'aidermacs-batch-drop-dired-marked-files
      "$aK" 'aidermacs-drop-all-files
      ;; File Others
      "$aS" 'aidermacs-create-session-scratchpad
      "$aG" 'aidermacs-add-file-to-session
      "$aA" 'aidermacs-list-added-files
      ;; Code Actions
      "$ac" 'aidermacs-direct-change
      "$ae" 'aidermacs-question-code
      "$ar" 'aidermacs-architect-this-code
      ;; Code Question
      "$aq" 'aidermacs-question-general
      "$a*" 'aidermacs-question-this-symbol
      "$ag" 'aidermacs-accept-change
      ;; Code Others
      "$a!" 'aidermacs-debug-exception)
    ))

