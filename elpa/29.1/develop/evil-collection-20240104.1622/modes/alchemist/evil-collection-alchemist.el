;;; evil-collection-alchemist.el --- Bindings for `alchemist' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, alchemist, tools

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
;;; Bindings for `alchemist'.

;;; Code:
(require 'evil-collection)
(require 'alchemist nil t)

(defconst evil-collection-alchemist-maps '(alchemist-compile-mode-map
                                           alchemist-eval-mode-map
                                           alchemist-execute-mode-map
                                           alchemist-message-mode-map
                                           alchemist-help-minor-mode-map
                                           alchemist-macroexpand-mode-map
                                           alchemist-mix-mode-map
                                           alchemist-test-report-mode-map
                                           alchemist-mode-map))


;;;###autoload
(defun evil-collection-alchemist-setup ()
  "Set up `evil' bindings for `alchemist'."
  (evil-set-initial-state 'alchemist-compile-mode 'normal)
  (evil-set-initial-state 'alchemist-eval-mode 'normal)
  (evil-set-initial-state 'alchemist-execute-mode 'normal)
  (evil-set-initial-state 'alchemist-message-mode 'normal)
  (evil-set-initial-state 'alchemist-help-minor-mode 'normal)
  (evil-set-initial-state 'alchemist-macroexpand-mode 'normal)
  (evil-set-initial-state 'alchemist-refcard-mode 'normal)
  (evil-set-initial-state 'alchemist-mix-mode 'normal)
  (evil-set-initial-state 'alchemist-test-mode 'normal)
  (evil-set-initial-state 'alchemist-test-report-mode 'normal)

  (evil-collection-define-key 'normal 'alchemist-compile-mode-map
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-eval-mode-map
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-execute-mode-map
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-message-mode-map
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-help-minor-mode-map
    "q" 'quit-window
    "K" 'alchemist-help-search-at-point
    "m" 'alchemist-help-module
    "s" 'alchemist-help
    "gh" 'alchemist-help-history
    "gd" 'alchemist-goto-definition-at-point
    "g?" 'alchemist-help-minor-mode-key-binding-summary)

  (evil-collection-define-key 'normal 'alchemist-macroexpand-mode-map
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-refcard-mode-map
    "gd" 'alchemist-refcard--describe-funtion-at-point
    "g?" 'alchemist-refcard--describe-funtion-at-point
    "q" 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-mix-mode-map
    "q" 'quit-window
    "i" 'alchemist-mix-send-input-to-mix-process
    "gr" 'alchemist-mix-rerun-last-task)

  (evil-collection-define-key 'normal 'alchemist-test-report-mode-map
    "q" 'quit-window
    "t" 'toggle-truncate-lines
    "gr" 'alchemist-mix-rerun-last-test
    "gj" 'alchemist-test-next-result
    "gk" 'alchemist-test-previous-result
    (kbd "C-j") 'alchemist-test-next-result
    (kbd "C-k") 'alchemist-test-previous-result
    "]]" 'alchemist-test-next-stacktrace-file
    "[[" 'alchemist-test-previous-stacktrace-file
    (kbd "C-c C-k") 'alchemist-report-interrupt-current-process)

  (evil-collection-define-key 'normal 'alchemist-mode-map
    "gz" 'alchemist-iex-run
    "K" 'alchemist-help-search-at-point
    "gd" 'alchemist-goto-definition-at-point
    (kbd "C-t") 'alchemist-goto-jump-back
    "g?" 'alchemist-help
    (kbd "C-j") 'alchemist-goto-jump-to-next-def-symbol
    (kbd "C-k") 'alchemist-goto-jump-to-previous-def-symbol))

(provide 'evil-collection-alchemist)
;;; evil-collection-alchemist.el ends here
