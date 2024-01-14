;;; evil-collection-distel.el --- Bindings for `distel' -*- lexical-binding: t -*-

;; Copyright (C) 2021 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, elisp, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Bindings for `distel'.

;;; Code:
(require 'distel nil t)
(require 'evil-collection)

(defconst evil-collection-distel-maps '(erlang-extended-mode-map
                                        edb-attach-mode-map
                                        edb-monitor-mode-map
                                        edb-variables-mode-map))

;;;###autoload
(defun evil-collection-distel-setup ()
  "Set up `evil' bindings for `distel'."
  (evil-collection-define-key 'normal 'erlang-extended-mode-map
    "gd" 'erl-find-source-under-point
    (kbd "C-t") 'erl-find-source-unwind
    "gz" 'erl-ie-show-session
    "K" 'erl-find-doc-under-point)

  (when evil-collection-setup-debugger-keys
    (evil-collection-define-key 'normal 'erlang-extended-mode-map
      [f5] 'edb-toggle-interpret
      [f9] 'edb-toggle-breakpoint
      ;; ("\C-c\C-ds" edb-synch-breakpoints)
      ;; ("\C-c\C-dS" edb-save-dbg-state)
      ;; ("\C-c\C-dR" edb-restore-dbg-state)
      ;; ("\C-c\C-dm" edb-monitor)
      )

    (evil-collection-define-key 'normal 'edb-monitor-mode-map
      (kbd "RET") 'edb-attach-command
      "q" 'erl-bury-viewer)

    (evil-collection-define-key 'normal 'edb-variables-mode-map
      (kbd "RET") 'edb-show-variable)

    (evil-collection-define-key 'normal 'edb-attach-mode-map
      "i" 'edb-attach-step ;; Step Into
      "n" 'edb-attach-next
      "c" 'edb-attach-continue
      "U" 'edb-attach-up
      "D" 'edb-attach-down
      "H" 'edb-attach-finish
      "q" 'erl-quit-viewer
      "g?" 'edb-attach-help
      "b" 'edb-toggle-breakpoint))

  (when evil-collection-want-find-usages-bindings
    (evil-collection-define-key 'normal 'erlang-extended-mode-map
      "gr" 'erl-who-calls))

  (add-hook 'erlang-extended-mode-hook 'evil-normalize-keymaps))

(provide 'evil-collection-distel)
;;; evil-collection-distel.el ends here
