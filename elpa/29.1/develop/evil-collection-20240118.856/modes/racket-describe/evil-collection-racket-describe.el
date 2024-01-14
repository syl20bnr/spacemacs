;;; evil-collection-racket-describe.el --- Bindings for `racket-describe' -*- lexical-binding: t -*-

;; Copyright (C) 2020 Andre Goderich

;; Author: Andre Goderich <yuhsien77@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
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
;; Bindings for `racket-describe'.

;;; Code:
(require 'evil-collection)
(require 'racket-describe nil t)

(defconst evil-collection-racket-describe-maps '(racket-describe-mode-map))

;;;###autoload
(defun evil-collection-racket-describe-setup ()
  "Set up `evil' bindings for `racket-describe'."
  (evil-collection-inhibit-insert-state 'racket-describe-mode-map)
  (evil-set-initial-state 'racket-describe-mode 'normal)
  (evil-collection-define-key 'normal 'racket-describe-mode-map
    "q" 'quit-window
    "g?" 'describe-mode))

(provide 'evil-collection-racket-describe)
;;; evil-collection-racket-describe.el ends here
