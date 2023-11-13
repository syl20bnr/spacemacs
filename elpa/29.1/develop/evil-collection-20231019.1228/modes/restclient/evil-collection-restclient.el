;;; evil-collection-restclient.el --- Bindings for `restclient' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "26.3"))
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
;;; Bindings for `restclient'.

;;; Code:
(require 'evil-collection)
(require 'restclient nil t)

(defconst evil-collection-restclient-maps
  '(evil-collection-restclient-mode-map
    restclient-mode-map))

(defvar evil-collection-restclient-mode-map (make-sparse-keymap))

(define-minor-mode evil-collection-restclient-result-mode
  "A minor mode to attach to `restclient' results"
  :group 'evil-collection-restclient-mode
  :keymap evil-collection-restclient-mode-map
  :lighter nil)

(defun evil-collection-restclient-setup-result-mode ()
  "Set up `evil-collection-restclient-result-mode'."
  (evil-collection-restclient-result-mode)
  (evil-normalize-keymaps))

;;;###autoload
(defun evil-collection-restclient-setup ()
  "Set up `evil' bindings for `restclient'."
  (evil-collection-define-key 'normal 'restclient-mode-map
    "[[" 'restclient-jump-prev
    "]]" 'restclient-jump-next)
  (evil-collection-define-key 'normal 'evil-collection-restclient-mode-map
    "q" #'quit-window
    "ZZ" #'quit-window
    "ZQ" #'evil-quit)

  ;; Enable a separate minor mode so that we can bind keys to it.
  (add-hook 'restclient-response-loaded-hook
            #'evil-collection-restclient-setup-result-mode))

(provide 'evil-collection-restclient)
;;; evil-collection-restclient.el ends here
