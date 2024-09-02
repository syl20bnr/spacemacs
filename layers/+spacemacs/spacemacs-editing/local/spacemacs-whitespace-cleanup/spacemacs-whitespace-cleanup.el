;;; spacemacs-whitespace-cleanup.el --- Cleanup whitspace automatically.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: editing, whitespace, spacemacs
;; Version: 0.2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package adds a minor mode to automatically cleanup whitespace.
;; It requires Spacemacs and depends on the value of the variable
;; `dotspacemacs-whitespace-cleanup' (see documentation in dotfile).

;;; Code:

;;;###autoload
(define-minor-mode spacemacs-whitespace-cleanup-mode
  "Minor mode to clean whitespace.

The minor mode is based on the value of the dotfile variable
 `dotspacemacs-whitespace-cleanup' to determine the behavior
of the cleanup."
  :lighter " CleanW"
  :group 'spacemacs
  (if spacemacs-whitespace-cleanup-mode
      (if (eq dotspacemacs-whitespace-cleanup 'changed)
          (ws-butler-mode)
        (add-hook 'before-save-hook 'spacemacs-whitespace-cleanup/clean-up nil t))
    ;; Always disable everything because `dotspacemacs-whitespace-cleanup' could
    ;; have changed and the configuration reloaded.
    (when (fboundp 'ws-butler-mode)
        (ws-butler-mode -1))
    (remove-hook 'before-save-hook 'spacemacs-whitespace-cleanup/clean-up t)))

(define-global-minor-mode global-spacemacs-whitespace-cleanup-mode
  spacemacs-whitespace-cleanup-mode
  spacemacs-whitespace-cleanup-mode
  :group 'spacemacs
  :require 'spacemacs-whitespace-cleanup-mode
  :predicate '(not markdown-mode))

(defun spacemacs-whitespace-cleanup/clean-up (&optional called-interactively)
  "When `dotspacemacs-whitespace-cleanup' is set
to `all' or `trailing', this function is automatically called as
part of the `before-save-hook'. It can also be called manually.
Note that it has no effect if `dotspacemacs-whitespace-cleanup'
is `changed'."
  (interactive "p")
  (pcase dotspacemacs-whitespace-cleanup
    ('all
     (whitespace-cleanup))
    ('trailing
     (delete-trailing-whitespace))
    ('changed
     (if called-interactively
         (user-error "`spacemacs-whitespace-cleanup/clean-up' has no effect because
`dotspacemacs-whitespace-cleanup' is `changed'. Hence whitespace
cleanup is exclusively handled by `ws-butler-mode'. Consider
changing this dotfile variable, or directly using
`whitespace-cleanup' or `delete-trailing-whitespace'."))
     (error "Error: `spacemacs-whitespace-cleanup/clean-up' was called even
though the value of `dotspacemacs-whitespace-cleanup' is
`changed'."))
    (x (user-error
        "%s is not a valid option for 'dotspacemacs-whitespace-cleanup'"
        x))))

(defun spacemacs-whitespace-cleanup/on-message (&optional global)
  "Return a string to display when the mode is activated."
  (format "whitespace-cleanup enabled%s (%s)"
          (if global " globally" "")
          (pcase dotspacemacs-whitespace-cleanup
            ('all "all whitespace")
            ('trailing "trailing whitespace")
            ('changed "changed lines")
            (x (user-error
                "%s is not a valid option for 'dotspacemacs-whitespace-cleanup'"
                x)))))

(provide 'spacemacs-whitespace-cleanup)
;;; spacemacs-whitespace-cleanup.el ends here.
