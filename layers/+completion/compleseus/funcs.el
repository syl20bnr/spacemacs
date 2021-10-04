;;; funcs.el --- compleseus Layer functions File for Spacemacs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
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


(defun spacemacs//compleseus-selectrum-hjkl-navigation (style)
  "Set navigation on 'hjkl' for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-style-enable-hjkl-bindings))

    (dolist (map (list selectrum-minibuffer-map))
      (define-key map (kbd "C-j") 'selectrum-next-candidate)
      (define-key map (kbd "C-k") 'selectrum-previous-candidate)))
   (t
    (define-key selectrum-minibuffer-map (kbd "C-j") 'selectrum-next-candidate)
    (define-key selectrum-minibuffer-map (kbd "C-k") 'selectrum-previous-candidate))))


(defun spacemacs/compleseus-switch-to-buffer ()
  "`consult-buffer' with buffers provided by persp."
  (interactive)
  (with-persp-buffer-list ()
                          (consult-buffer)))


(defun spacemacs/compleseus-search (use-initial-input initial-directory)
  (let* ((initial-input (if use-initial-input
                            (if (region-active-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))
                              (thing-at-point 'symbol t))
                          ""))
         (default-directory
           (or initial-directory (read-directory-name "Start from directory: "))))
    (consult-ripgrep default-directory initial-input)))

(defun spacemacs/consult-line ()
  (interactive)
  (consult-line
   (if (region-active-p)
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     (thing-at-point 'symbol t))))

(defun spacemacs/consult-line-multi ()
  (interactive)
  (consult-line-multi
   nil
   (if (region-active-p)
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     (thing-at-point 'symbol t))))

(defun spacemacs/compleseus-search-auto ()
  "Choose folder to search."
  (interactive)
  (spacemacs/compleseus-search t nil))

(defun spacemacs/compleseus-search-dir ()
  "Search current folder."
  (interactive)
  (spacemacs/compleseus-search t default-directory))

(defun spacemacs/compleseus-search-projectile ()
  "Search in current project."
  (interactive)
  (spacemacs/compleseus-search t (projectile-project-root)))

(defun spacemacs/compleseus-search-default ()
  "Search."
  (interactive)
  (spacemacs/compleseus-search-projectile))

(defun spacemacs/compleseus-search-projectile-auto ()
  "Search in current project."
  (interactive)
  (spacemacs/compleseus-search nil (projectile-project-root)))

(defun spacemacs/compleseus-search-from (input)
  "Embark action to start ripgrep search from candidate's directory."
  (interactive "s")
  (message "The first input %s." input)
  (let ((dir (if (file-directory-p input)
                 input
               (file-name-directory input))))
    (consult-ripgrep dir)))

(defun spacemacs/compleseus-find-file ()
  "This solves the problem:
Binding a key to: `find-file' calls: `ido-find-file'"
  (interactive)
  (call-interactively 'find-file))

;; persp-mode stuff
(defun spacemacs/compleseus-spacemacs-layout-layouts ()
  (interactive)
  (spacemacs//create-persp-with-home-buffer
   (completing-read "Layouts:" (persp-names))))

;; vertico
(defun spacemacs/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))

(defun spacemacs/next-candidate-preview (&optional n)
  "Go forward N candidates and preview"
  (interactive)
  (vertico-next (or n 1))
  (spacemacs/embark-preview))

(defun spacemacs/previous-candidate-preview (&optional n)
  "Go backward N candidates and preview"
  (interactive)
  (selec-previous (or n 1))
  (spacemacs/embark-preview))

;; selectrum

(defun spacemacs/selectrum-next-candidate-preview (&optional n)
  "Go forward N candidates and preview"
  (interactive)
  (selectrum-next-candidate (or n 1))
  (spacemacs/embark-preview))

(defun spacemacs/selectrum-previous-candidate-preview (&optional n)
  "Go backward N candidates and preview"
  (interactive)
  (selectrum-previous-candidate (or n 1))
  (spacemacs/embark-preview))
