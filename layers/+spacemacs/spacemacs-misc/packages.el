;;; packages.el --- Spacemacs Misc. Layer packages File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(setq spacemacs-misc-packages
      '(
        devdocs
        dumb-jump
        request))


(defun spacemacs-misc/init-dumb-jump ()
  (use-package dumb-jump
    :defer t
    :init
    ;; Use Helm or Ivy as the selector for dumb-jump.
    (cond
     ((configuration-layer/layer-used-p 'ivy)
      (setq dumb-jump-selector 'ivy))
     ((configuration-layer/layer-used-p 'helm)
      (setq dumb-jump-selector 'helm)))

    ;; Enable xref-backend of dumb-jump. It's chosen only when no better
    ;; options is available
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))

(defun spacemacs-misc/init-request ()
  (setq request-storage-directory
        (concat spacemacs-cache-directory "request/")))

(defun spacemacs-misc/init-devdocs ()
  (use-package devdocs
    :defer t
    :init
    (defalias 'spacemacs/browse-docs-online-at-point 'devdocs-search)
    (spacemacs/set-leader-keys "hbd" #'spacemacs/browse-docs-online-at-point)))
