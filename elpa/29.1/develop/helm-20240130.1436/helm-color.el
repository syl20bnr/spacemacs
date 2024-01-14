;;; helm-color.el --- colors and faces -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

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

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'helm-elisp)

(declare-function list-colors-display "facemenu")

;;; Customize Face
;;
;;
(defun helm-custom-faces-init ()
  "Initialize buffer for `helm-source-customize-face'."
  (unless (helm-candidate-buffer)
    (save-selected-window
      (list-faces-display)
      (message nil))
    (helm-init-candidates-in-buffer
        'global
      (with-current-buffer (get-buffer "*Faces*")
        (buffer-substring
         (next-single-char-property-change (point-min) 'category)
         (point-max))))
    (kill-buffer "*Faces*")))

(defvar helm-source-customize-face
  (helm-build-in-buffer-source "Customize Face"
    :init 'helm-custom-faces-init
    :get-line 'buffer-substring
    :persistent-action (lambda (candidate)
                         (helm-elisp--persistent-help
                          (intern (car (split-string candidate)))
                          'helm-describe-face))
    :persistent-help "Describe face"
    :action '(("Customize"
               . (lambda (line)
                   (customize-face (intern (car (split-string line))))))
              ("Copy name"
               . (lambda (line)
                   (kill-new (car (split-string line " " t)))))))
  "See (info \"(emacs)Faces\")")

;;; Colors browser
;;
;;
(defun helm-colors-init ()
  (require 'facemenu)
  (unless (helm-candidate-buffer)
    (save-selected-window
      (list-colors-display)
      (message nil))
    (helm-init-candidates-in-buffer
        'global
      (with-current-buffer (get-buffer "*Colors*")
        (buffer-string)))
    (kill-buffer "*Colors*")))

(defun helm-color-insert-name (candidate)
  (with-helm-current-buffer
    (insert (helm-colors-get-name candidate))))

(defun helm-color-kill-name (candidate)
  (kill-new (helm-colors-get-name candidate)))

(defun helm-color-insert-rgb (candidate)
  (with-helm-current-buffer
    (insert (helm-colors-get-rgb candidate))))

(defun helm-color-kill-rgb (candidate)
  (kill-new (helm-colors-get-rgb candidate)))

(helm-make-command-from-action helm-color-run-insert-name
  "Insert name of color from `helm-source-colors'."
  'helm-color-insert-name)

(helm-make-command-from-action helm-color-run-kill-name
  "Kill name of color from `helm-source-colors'."
  'helm-color-kill-name)

(helm-make-command-from-action helm-color-run-insert-rgb
  "Insert RGB of color from `helm-source-colors'."
  'helm-color-insert-rgb)

(helm-make-command-from-action helm-color-run-kill-rgb
  "Kill RGB of color from `helm-source-colors'."
  'helm-color-kill-rgb)

(defvar helm-color-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c n") #'helm-color-run-insert-name)
    (define-key map (kbd "C-c N") #'helm-color-run-kill-name)
    (define-key map (kbd "C-c r") #'helm-color-run-insert-rgb)
    (define-key map (kbd "C-c R") #'helm-color-run-kill-rgb)
    map))

(defvar helm-source-colors
  (helm-build-in-buffer-source "Colors"
    :init 'helm-colors-init
    :get-line 'buffer-substring
    :keymap helm-color-map
    :persistent-help "Kill entry in RGB format."
    :persistent-action 'helm-color-kill-rgb
    :help-message 'helm-colors-help-message
    :action
    '(("Copy Name (C-c N)" . helm-color-kill-name)
      ("Copy RGB (C-c R)" . helm-color-kill-rgb)
      ("Insert Name (C-c n)" . helm-color-insert-name)
      ("Insert RGB (C-c r)" . helm-color-insert-rgb))))

(defun helm-colors-get-name (candidate)
  "Get color name."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-min))
     (search-forward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-max))
     (buffer-string))))

(defun helm-colors-get-rgb (candidate)
  "Get color RGB."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-max))
     (search-backward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-min))
     (buffer-string))))

;;;###autoload
(defun helm-colors ()
  "Preconfigured `helm' for color."
  (interactive)
  (helm :sources '(helm-source-colors helm-source-customize-face)
        :buffer "*helm colors*"))

(provide 'helm-color)

;;; helm-color.el ends here
