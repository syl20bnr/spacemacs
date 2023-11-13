;;; evil-matchit-evil-setup.el --- setup for evil -*- lexical-binding: t -*-

;; Author: Chen Bin <chenbin.sh@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides setup for evil-mode.

;;; Code:

(require 'evil)
(eval-when-compile (require 'evil-macros))

(defcustom evilmi-may-jump-by-percentage t
  "Simulate built-in jump item command in evil.
For example, `50%' jumps to 50 percentage of buffer.
If nil, `50%' jumps 50 times."
  :group 'evil-matchit
  :type 'boolean)

(defcustom evilmi-shortcut "%"
  "The keybinding of `evilmi-jump-items' and then text object shortcut.
Some people prefer using \"m\" instead."
  :group 'evil-matchit
  :type 'string)

;; {{ make linter happy
(defvar evil-visual-char)
(defvar evil-visual-direction)
(defvar evil-this-type-modified)
;; }}

(evil-define-text-object evilmi-inner-text-object (&optional num begin end type)
  "Inner text object describing the region selected when pressing %."
  :type line
  (let* ((selected-region (funcall 'evilmi--region-to-select-or-delete num t)))
    (evil-range (car selected-region) (cadr selected-region) 'line)))

(evil-define-text-object evilmi-outer-text-object (&optional num begin end type)
  "Outer text object describing the region selected when pressing %."
  :type line
  (let ((selected-region (funcall 'evilmi--region-to-select-or-delete num)))
    (evil-range (car selected-region) (cadr selected-region) 'line)))

(define-key evil-inner-text-objects-map evilmi-shortcut 'evilmi-inner-text-object)
(define-key evil-outer-text-objects-map evilmi-shortcut 'evilmi-outer-text-object)

;;;###autoload
(defun evilmi-jump-to-percentage (num)
  "Like Vim %, NUM is the percentage of location."
  (interactive "P")
  (let* (dst)
    (when (and num (> num 0))
      (setq dst (let ((size (- (point-max) (point-min))))
                  (+ (point-min)
                     (if (> size 80000)
                         (* num (/ size 100))
                       (/ (* num size) 100)))))
      (cond
       ((< dst (point-min))
        (setq dst (point-min)))
       ((> dst (point-max))
        (setq dst (point-max))))
      (goto-char dst)
      (back-to-indentation))))

;;;###autoload (autoload 'evilmi-jump-items "evil-matchit" nil t)
(evil-define-command evilmi-jump-items (&optional num)
  "Jump between items NUM times."
  :repeat nil
  :jump t
  (interactive "P")
  (cond
   ((and evilmi-may-jump-by-percentage num)
    (evilmi-jump-to-percentage num))
   (t
    (funcall 'evilmi-jump-items-internal num))))

(defvar evil-matchit-mode-map (make-sparse-keymap)
  "Keymap used by the minor mode.")

;;;###autoload
(define-minor-mode evil-matchit-mode
  "Buffer-local minor mode to emulate matchit.vim."
  :keymap (make-sparse-keymap)
  :group 'evil-matchit
  ;; get correct value of `(point)` in visual-line mode
  ;; @see https://bitbucket.org/lyro/evil/issues/540/get-the-char-under-cusor-in-visual-line
  (evil-set-command-property 'evilmi-jump-items :keep-visual t)
  (cond
   ((fboundp 'evilmi-customize-keybinding)
    ;; user's own key bindings
    (evilmi-customize-keybinding))

   (t
    ;; fallback to default key bindings
    (evil-define-key 'normal evil-matchit-mode-map evilmi-shortcut 'evilmi-jump-items)
    (evil-define-key 'visual evil-matchit-mode-map evilmi-shortcut 'evilmi-jump-items)))

  (evil-normalize-keymaps))

;;;###autoload
(defun turn-on-evil-matchit-mode ()
  "Enable the minor mode in the current buffer."
  (evil-matchit-mode 1))

;;;###autoload
(defun turn-off-evil-matchit-mode ()
  "Disable the minor mode in the current buffer."
  (evil-matchit-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-matchit-mode
  evil-matchit-mode turn-on-evil-matchit-mode
  "Global minor mode to emulate matchit.vim.")

(provide 'evil-matchit-evil-setup)
;;; evil-matchit-evil-setup.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
