;;; evil-collection-eldoc.el --- Bindings for `eldoc' -*- lexical-binding: t -*-

;; Copyright (C) 2022 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, emacs, tools

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
;;; Bindings for `eldoc'.

;;; Code:
(require 'evil-collection)
(require 'eldoc nil t)

(defconst evil-collection-eldoc-maps
  '(evil-collection-eldoc-doc-buffer-mode-map))

(defvar eldoc--doc-buffer) ;; Internal to `eldoc'.

(defvar evil-collection-eldoc-doc-buffer-mode-map (make-sparse-keymap))

(define-minor-mode evil-collection-eldoc-doc-buffer-mode
  "A minor mode to attach to `eldoc-doc-buffer' buffers."
  :group 'evil-collection-eldoc-mode
  :keymap evil-collection-eldoc-doc-buffer-mode-map
  :lighter nil)

(defun evil-collection-eldoc-setup-doc-buffer-mode (&rest _)
  "Set up `evil-collection-eldoc-doc-buffer-mode'."
  (with-current-buffer eldoc--doc-buffer
    (evil-collection-eldoc-doc-buffer-mode +1)
    (evil-normalize-keymaps)))

;;;###autoload
(defun evil-collection-eldoc-setup ()
  "Set up `evil' bindings for `eldoc'."
  (evil-collection-define-key 'normal 'evil-collection-eldoc-doc-buffer-mode-map
    "q" #'quit-window
    "ZZ" #'quit-window
    "ZQ" #'evil-quit)

  ;; 'special-mode' is used in eldoc since
  ;; https://github.com/emacs-mirror/emacs/commit/97abe8511a829861f6efb865209ac2dd0e7ae129
  (when (< emacs-major-version 29)
    ;; Enable a separate minor mode so that we can bind keys to it.
    (advice-add 'eldoc-doc-buffer
                :after 'evil-collection-eldoc-setup-doc-buffer-mode)))

(provide 'evil-collection-eldoc)
;;; evil-collection-eldoc.el ends here
