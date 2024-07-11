;;; packages.el --- Chrome Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Ben Hayden <hayden767@gmail.com>
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


(setq chrome-packages '(
                        edit-server
                        gmail-message-mode
                        flymd
                        markdown-mode
                        ))

(defun chrome/init-edit-server ()
  (use-package edit-server
    :init (edit-server-start)
    :config (setq edit-server-default-major-mode 'markdown-mode)))

(defun chrome/init-gmail-message-mode ()
  (use-package gmail-message-mode
    :defer t
    :config
    (when (configuration-layer/layer-used-p 'markdown)
      (spacemacs/set-markdown-keybindings
       'gmail-message-client-mode gmail-message-client-mode-map))))

(defun chrome/init-flymd ()
  (use-package flymd
    :defer t
    :init (setq flymd-browser-open-function
                'spacemacs//flymd-browser-function)))

(defun chrome/pre-init-markdown-mode ()
  (spacemacs|use-package-add-hook markdown-mode
    :pre-config
    (when (configuration-layer/package-used-p 'gmail-message-mode)
      (add-to-list 'markdown--key-bindings-modes 'gmail-message-client-mode))))
