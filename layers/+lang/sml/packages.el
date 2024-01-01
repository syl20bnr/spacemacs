;;; packages.el --- sml Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Keith Simmons <keith@the-simmons.net>
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


(setq sml-packages
      '(
        sml-mode
        ob-sml
        smartparens
        ))

(defun sml/init-sml-mode ()
  (use-package sml-mode
    :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
    :defer t
    :commands run-sml
    :init (spacemacs/register-repl 'sml-mode 'run-sml "sml")
    :config
    (defun spacemacs/sml-prog-proc-send-buffer-and-focus ()
      "Send buffer to REPL and switch to it in `insert state'."
      (interactive)
      (sml-prog-proc-send-buffer t)
      (evil-insert-state))

    (defun spacemacs/sml-prog-proc-send-region-and-focus (start end)
      "Send region to REPL and switch to it in `insert state'."
      (interactive "r")
      (sml-prog-proc-send-region start end t)
      (evil-insert-state))

    (defun spacemacs/sml-send-function-and-focus ()
      "Send function at point to REPL and switch to it in `insert state'."
      (interactive)
      (sml-send-function t)
      (evil-insert-state))

    (spacemacs/set-leader-keys-for-major-mode 'sml-mode
      ;; REPL
      "'"  'run-sml
      "sb" 'sml-prog-proc-send-buffer
      "sB" 'spacemacs/sml-prog-proc-send-buffer-and-focus
      "sf" 'sml-send-function
      "sF" 'spacemacs/sml-send-function-and-focus
      "si" 'run-sml
      "sr" 'sml-prog-proc-send-region
      "sR" 'spacemacs/sml-prog-proc-send-region-and-focus
      "ss" 'run-sml)
    (define-key sml-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
    (define-key sml-mode-map (kbd "M-SPC") 'sml-electric-space)
    (define-key sml-mode-map (kbd "|") 'sml-electric-pipe)))

(defun sml/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
    (sp-local-pair 'sml-mode "'" nil :actions nil)
    (sp-local-pair 'sml-mode "`" nil :actions nil)))

(defun sml/pre-init-ob-sml ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-sml
      :init (add-to-list 'org-babel-load-languages '(sml . t)))))
(defun sml/init-ob-sml ())
