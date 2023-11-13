;;; evil-collection-mu4e-conversation.el --- Evil bindings for mu4e-conversation -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (evil "1.2.10"))
;; Keywords: evil, mu4e, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for mu4e-conversation.

;;; Code:

(require 'evil-collection)
(require 'mu4e-conversation nil t)

(defconst evil-collection-mu4e-conversation-maps '(mu4e-conversation-map
                                                   mu4e-conversation-thread-map))

(defvar evil-collection-mu4e-conversation--local-map-p nil
  "Non-nil if last position was on a local-map property.")

(defun evil-collection-mu4e-conversation--switch ()
  "Re-compute the bindings if point has moved between the thread
  area and the composition area."
  (let ((local-map-here (get-text-property (point) 'local-map)))
    (when (or (and evil-collection-mu4e-conversation--local-map-p
                   (not local-map-here))
              (and (not evil-collection-mu4e-conversation--local-map-p)
                   local-map-here))
      (evil-normalize-keymaps))
    (setq evil-collection-mu4e-conversation--local-map-p local-map-here)))

(defun evil-collection-mu4e-conversation--update-local-map ()
  (setq evil-collection-mu4e-conversation--local-map-p (get-text-property (point) 'local-map))
  (evil-normalize-keymaps)
  (add-hook 'post-command-hook 'evil-collection-mu4e-conversation--switch nil t))

;;;###autoload
(defun evil-collection-mu4e-conversation-setup ()
  "Set up `evil' bindings for `mu4e-conversation'."
  ;; Evil does not update its current keymap state when it the point hits a
  ;; local-map property is used.  See
  ;; https://github.com/emacs-evil/evil/issues/301.  Thus we force the update
  ;; with a technique similar to what `org~mu4e-mime-switch-headers-or-body'
  ;; does.
  (add-hook 'mu4e-conversation-hook 'evil-collection-mu4e-conversation--update-local-map)
  (evil-collection-define-key 'normal 'mu4e-conversation-map
    " " 'evil-scroll-page-down
    (kbd "S-SPC") 'evil-scroll-page-up
    "[[" 'mu4e-conversation-previous-message
    "]]" 'mu4e-conversation-next-message
    "zv" 'mu4e-conversation-toggle-view
    "za" 'mu4e-conversation-toggle-hide-cited
    "q" 'mu4e-conversation-quit))

(provide 'evil-collection-mu4e-conversation)
;;; evil-collection-mu4e-conversation.el ends here
