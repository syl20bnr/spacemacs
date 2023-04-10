;;; packages.el --- F# Layer packages File for Spacemacs
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


(defconst fsharp-packages
  '(
    company
    counsel-gtags
    (eglot-fsharp :toggle (eq fsharp-backend 'eglot))
    flycheck
    fsharp-mode
    ggtags))

(defun fsharp/post-init-company ()
  (spacemacs//fsharp-setup-company))

(defun fsharp/post-init-flycheck ()
  (spacemacs/enable-flycheck 'fsharp-mode))

(defun fsharp/init-eglot-fsharp ()
  (use-package eglot-fsharp
    :defer t
    :init
    (progn
      (require 'f)
      (setq eglot-fsharp-server-install-dir
            (expand-file-name
             (locate-user-emacs-file (f-join ".cache" "eglot")))))))

(defun fsharp/init-fsharp-mode ()
  (use-package fsharp-mode
    :defer t
    :init
    (progn
      (when (eq fsharp-backend 'eglot)
        (require 'eglot-fsharp))
      (setq fsharp-doc-idle-delay .2)
      (spacemacs/register-repl 'fsharp-mode 'fsharp-show-subshell "F#")
      (add-hook 'fsharp-mode-hook #'spacemacs//fsharp-setup-backend))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'fsharp-mode "ms" "repl")
      (spacemacs/declare-prefix-for-mode 'fsharp-mode "mc" "compile")
      (when (eq fsharp-backend 'eglot)
        (spacemacs/declare-prefix-for-mode 'fsharp-mode "mg" "goto"))
      (spacemacs/set-leader-keys-for-major-mode 'fsharp-mode
        "cc" 'compile
        "ga" 'fsharp-find-alternate-file
        "sb" 'fsharp-load-buffer-file
        "sB" 'spacemacs/fsharp-load-buffer-file-focus
        "si" 'fsharp-show-subshell
        "sp" 'fsharp-eval-phrase
        "sP" 'spacemacs/fsharp-eval-phrase-focus
        "sr" 'fsharp-eval-region
        "sR" 'spacemacs/fsharp-eval-region-focus
        "'"  'fsharp-show-subshell))))

(defun fsharp/post-init-ggtags ()
  (add-hook 'fsharp-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun fsharp/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'fsharp-mode))
