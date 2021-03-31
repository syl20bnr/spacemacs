;;; funcs.el --- ESS Layer functions File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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



;; R

(defun spacemacs//ess-r-backend ()
  "Returns selected backend."
  (if ess-r-backend
      ess-r-backend
    (cond ((configuration-layer/layer-used-p 'lsp) 'lsp)
          (t 'ess))))

(defun spacemacs//ess-may-setup-r-lsp ()
  "Conditionally setup LSP based on backend."
  (when (eq (spacemacs//ess-r-backend) 'lsp)
    (spacemacs//ess-setup-r-lsp)))

(defun spacemacs//ess-setup-r-lsp ()
  "Setup LSP backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; Key Bindings

(defun spacemacs//ess-bind-keys-for-mode (mode)
  "Bind the keys in MODE."
  (spacemacs/declare-prefix-for-mode mode "md" "debug")
  (spacemacs/declare-prefix-for-mode mode "mD" "devtools")
  (spacemacs/declare-prefix-for-mode mode "mDc" "check")
  (spacemacs/declare-prefix-for-mode mode "mE" "extra")
  (spacemacs/declare-prefix-for-mode mode "mh" "help")
  (spacemacs/set-leader-keys-for-major-mode mode
    "h" 'ess-doc-map              ;; help
    "d" 'ess-dev-map              ;; debug
    "D" 'ess-r-package-dev-map    ;; devtools
    "E" 'ess-extra-map            ;; extra
    ))

(defun spacemacs//ess-bind-repl-keys-for-mode (mode)
  "Set the REPL keys in MODE."
  (spacemacs/declare-prefix-for-mode mode "ms" "repl")
  (spacemacs/set-leader-keys-for-major-mode mode
    ","  #'ess-eval-region-or-function-or-paragraph-and-step
    "'"  #'spacemacs/ess-start-repl
    "si" #'spacemacs/ess-start-repl
    "ss" #'ess-switch-to-inferior-or-script-buffer
    "sS" #'ess-switch-process
    "sB" #'ess-eval-buffer-and-go
    "sb" #'ess-eval-buffer
    "sd" #'ess-eval-region-or-line-and-step
    "sD" #'ess-eval-function-or-paragraph-and-step
    "sL" #'ess-eval-line-and-go
    "sl" #'ess-eval-line
    "sQ" #'ess-quit
    "sR" #'ess-eval-region-and-go
    "sr" #'ess-eval-region
    "sF" #'ess-eval-function-and-go
    "sf" #'ess-eval-function))

(defun spacemacs/ess-bind-keys-for-julia ()
  (spacemacs//ess-bind-keys-for-mode 'ess-julia-mode)
  (spacemacs//ess-bind-repl-keys-for-mode 'ess-julia-mode))

(defun spacemacs/ess-bind-keys-for-r ()
  (when ess-assign-key
    (define-key ess-r-mode-map ess-assign-key #'ess-insert-assign))

  (spacemacs//ess-bind-keys-for-mode 'ess-r-mode)
  (spacemacs//ess-bind-repl-keys-for-mode 'ess-r-mode))

(defun spacemacs/ess-bind-keys-for-inferior ()
  (define-key inferior-ess-mode-map (kbd "C-j") #'comint-next-input)
  (define-key inferior-ess-mode-map (kbd "C-k") #'comint-previous-input)
  (when ess-assign-key
    (define-key inferior-ess-r-mode-map ess-assign-key #'ess-insert-assign))

  (spacemacs/declare-prefix-for-mode 'inferior-ess-mode "ms" "repl")
  (spacemacs/declare-prefix-for-mode 'inferior-ess-mode "me" "eval")
  (spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mg" "xref")
  (spacemacs/set-leader-keys-for-major-mode 'inferior-ess-mode
    ","  #'ess-smart-comma
    "ss" #'ess-switch-to-inferior-or-script-buffer))


;; REPL

(defun spacemacs/ess-start-repl ()
  "Start a REPL corresponding to the ess-language of the current buffer."
  (interactive)
  (cond
   ((string= "S" ess-language) (call-interactively 'R))
   ((string= "STA" ess-language) (call-interactively 'stata))
   ((string= "SAS" ess-language) (call-interactively 'SAS))
   ((string= "julia" ess-language) (call-interactively 'julia))))

(defun spacemacs//ess-fix-read-only-inferior-ess-mode ()
  "Fixes a bug when `comint-prompt-read-only' in non-nil.
See https://github.com/emacs-ess/ESS/issues/300"
  (setq-local comint-use-prompt-regexp nil)
  (setq-local inhibit-field-text-motion nil))
