;;; funcs.el --- Markdown Layer Functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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


(defun spacemacs/activate-mmm-mode ()
  (unless (bound-and-true-p git-commit-mode)
    ;; Enable `mmm-mode'.
    (mmm-mode 1)))

;; Insert key for org-mode and markdown a la C-h k
;; Based on SE https://emacs.stackexchange.com/a/2208
(defun spacemacs/insert-keybinding-markdown (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((tag "~%s~"))
    (if (null (equal key "\r"))
        (insert
         (format tag (help-key-description key nil)))
      (insert (format tag ""))
      (forward-char -6))))

;; from Jason Blevins http://jblevins.org/log/mmm
(defun markdown/mmm-auto-class (lang)
  (let* ((l (if (listp lang) (car lang) lang))
         (s (if (listp lang) (cadr lang) lang))
         (class (intern (concat "markdown-" l)))
         (submode (intern (concat s "-mode")))
         (front (concat "^```" l "[\n\r]+"))
         (back "^```$"))
    (mmm-add-classes (list (list class
                                 :submode submode
                                 :front front
                                 :back back)))
    (dolist (mode markdown--key-bindings-modes)
      (mmm-add-mode-ext-class mode nil class))))

(defun spacemacs//markdown-hjkl-promotion-demotion (style)
  "Set promotion/demotiion on 'hjkl' for the given editing STYLE."
  (when (or (eq 'vim style)
            (and (eq 'hybrid style)
                 hybrid-style-enable-hjkl-bindings))
    (dolist (s '(normal insert))
      (evil-define-key s markdown-mode-map
        (kbd "M-h") 'markdown-promote
        (kbd "M-j") 'markdown-move-down
        (kbd "M-k") 'markdown-move-up
        (kbd "M-l") 'markdown-demote))))
