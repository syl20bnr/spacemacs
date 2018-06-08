;;; funcs.el --- Markdown Layer Functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/activate-mmm-mode ()
  ;; Enable `mmm-mode'.
  (mmm-mode 1))

;; stolen from http://stackoverflow.com/a/26297700
;; makes markdown tables saner via orgtbl-mode
(defun spacemacs//cleanup-org-tables ()
  (require 'org-table)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))

(defun spacemacs//cleanup-org-tables-on-save ()
  (add-hook 'before-save-hook 'spacemacs//cleanup-org-tables nil 'local))

;; Insert key for org-mode and markdown a la C-h k
;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
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
