;;; packages.el --- Asm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
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


(setq asm-packages
      '(
        ;; package names go here
        asm-mode
        company
        electric-indent-mode
        ggtags
        counsel-gtags
        nasm-mode
        x86-lookup
        ))

(defun asm/init-asm-mode ()
  (use-package asm-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'asm-mode "h" 'x86-lookup)
    :config
    ;; We need to insert a non-indented line, otherwise it's annoying
    ;; every time we insert a comment for a routine
    (define-key asm-mode-map (kbd "C-j") 'newline)
    (add-hook 'asm-mode-hook #'asm-generic-setup)))

(defun asm/post-init-electric-indent-mode ()
  (spacemacs/add-to-hooks 'asm-electric-indent-local-mode-off
                   '(asm-mode-hook nasm-mode-hook)))

(defun asm/init-nasm-mode ()
  "Setup for built-in `nasm-mode', which could be thought as improved `asm-mode'"
  (use-package nasm-mode
    :init
    (add-hook 'nasm-mode-hook #'asm-generic-setup)
    (add-to-list 'auto-mode-alist '("\\.[n]*\\(asm\\|s\\)\\'" . nasm-mode))
    (spacemacs/set-leader-keys-for-major-mode 'nasm-mode "h" 'x86-lookup)
    :config
    ;; We need to insert a non-indented line, otherwise it's annoying
    ;; every time we insert a comment for a routine
    (define-key nasm-mode-map (kbd "C-j") 'newline)
    ;; we use the advised `asm-colon' because `nasm-colon indents the whole line, even
    ;; inside a comment
    (define-key nasm-mode-map (kbd ":") 'asm-colon)))

(defun asm/init-x86-lookup ()
  (use-package x86-lookup
    :init
    ;; when a user installed `pdf-tools', use it for viewing PDF document.
    (when (package-installed-p 'pdf-tools)
      (setq x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools))))

(defun asm/post-init-company ()
  (spacemacs|add-company-backends :modes asm-mode nasm-mode))

(defun asm/post-init-ggtags ()
  (add-hook 'asm-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun asm/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'asm-mode))
