;;; evil-collection-git-timemachine.el --- Bindings for `git-timemachine' -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, git, tools

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
;; Evil keybindings for `git-timemachine' that conform to the principles
;; outlines in evil-collection

;;; Code:
(require 'evil-collection)
(require 'git-timemachine nil t)

(defvar git-timemachine-mode-map)
(defconst evil-collection-git-timemachine-map '(git-timemachine-mode-map))

;;;###autoload
(defun evil-collection-git-timemachine-setup ()
  "Setup `evil' keybindings for `git-timemachine'."
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    "\C-k" 'git-timemachine-show-previous-revision
    "\C-j" 'git-timemachine-show-next-revision
    "q"    'git-timemachine-quit
    "gtg"  'git-timemachine-show-nth-revision
    "gtt"  'git-timemachine-show-revision-fuzzy
    "gty"  'git-timemachine-kill-abbreviated-revision
    "gtY"  'git-timemachine-kill-revision
    "gtb"  'git-timemachine-blame))

(provide 'evil-collection-git-timemachine)
;;; evil-collection-git-timemachine.el ends here
