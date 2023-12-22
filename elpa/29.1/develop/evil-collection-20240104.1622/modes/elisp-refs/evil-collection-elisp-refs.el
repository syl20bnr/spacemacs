;;; evil-collection-elisp-refs.el --- Evil bindings for Elisp Refs -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, elisp-refs, tools

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
;; This package provides a sane set of defaults for `elisp-refs' when using
;; `evil-mode'.

;;; Code:
(require 'evil-collection)
(require 'elisp-refs nil t)

(defconst evil-collection-elisp-refs-maps '(elisp-refs-mode-map))

;;;###autoload
(defun evil-collection-elisp-refs-setup ()
  "Set up `evil' bindings for `elisp-refs'."
  (evil-collection-define-key 'normal 'elisp-refs-mode-map
    (kbd "<tab>") 'elisp-refs-next-match
    (kbd "<backtab>") 'elisp-refs-prev-match
    (kbd "C-j") 'elisp-refs-next-match
    (kbd "C-k") 'elisp-refs-prev-match
    "gj" 'elisp-refs-next-match
    "gk" 'elisp-refs-prev-match
    (kbd "RET") 'elisp-refs-visit-match

    ;; quit
    "q" 'kill-this-buffer))

(provide 'evil-collection-elisp-refs)
;;; evil-collection-elisp-refs.el ends here
