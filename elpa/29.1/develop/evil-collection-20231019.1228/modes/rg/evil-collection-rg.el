;;; evil-collection-rg.el --- Evil bindings for rg -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, rg, tools

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
;; Evil bindings for rg.

;;; Code:
(require 'evil-collection)
(require 'rg nil t)

(defconst evil-collection-rg-maps '(rg-mode-map))

;;;###autoload
(defun evil-collection-rg-setup ()
  "Set up `evil' bindings for `rg'."
  (evil-set-initial-state 'rg-mode 'normal)

  (evil-collection-define-key 'normal 'rg-mode-map
    ;; rerun
    "c" 'rg-rerun-toggle-case
    "d" 'rg-rerun-change-dir
    "f" 'rg-rerun-change-files
    "I" 'rg-rerun-toggle-ignore ;; was i
    "r" 'rg-rerun-change-regexp
    "t" 'rg-rerun-change-literal

    ;; save
    "s" 'rg-save-search
    "S" 'rg-save-search-as-name

    ;; action
    "m" 'rg-menu
    "i" 'wgrep-change-to-wgrep-mode    ;; was w/e
    "F" 'next-error-follow-minor-mode  ;; C-c C-f
    "L" 'rg-list-searches              ;; was l
    (kbd "C-p") 'rg-back-history       ;; C-c <
    (kbd "C-n") 'rg-forward-history    ;; C-c >

    ;; navigation
    "{" 'rg-prev-file
    "}" 'rg-next-file
    "gk" 'previous-error-no-select
    "gj" 'next-error-no-select
    (kbd "C-k") 'previous-error-no-select
    (kbd "C-j") 'next-error-no-select

    "e" 'evil-forward-word-end    ;; shadows wgrep-change-to-wgrep-mode
    "l" 'evil-forward-char        ;; shadows rg-list-searches
    "w" 'evil-forward-word-begin  ;; shadows wgrep-change-to-wgrep-mode
    "gg" 'evil-goto-first-line
    "gr" 'rg-recompile

    ;; quit
    "q" 'quit-window)

  (evil-collection-define-key 'motion 'rg-mode-map
    "e" 'evil-forward-word-end    ;; shadows wgrep-change-to-wgrep-mode
    "l" 'evil-forward-char        ;; shadows rg-list-searches
    "w" 'evil-forward-word-begin  ;; shadows wgrep-change-to-wgrep-mode
    "gg" 'evil-goto-first-line)

  (evil-collection-define-key 'visual 'rg-mode-map
    "e" 'evil-forward-word-end    ;; shadows wgrep-change-to-wgrep-mode
    "l" 'evil-forward-char        ;; shadows rg-list-searches
    "w" 'evil-forward-word-begin  ;; shadows wgrep-change-to-wgrep-mode
    "gg" 'evil-goto-first-line))

(provide 'evil-collection-rg)
;;; evil-collection-rg.el ends here
