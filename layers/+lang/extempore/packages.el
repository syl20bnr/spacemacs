;;; packages.el --- Extempore Layer packages File for Spacemacs
;;
;; Copyright (c) 2020 Sylvain Benner & Contributors
;;
;; Author: Ben Swift <ben@benswift.me>
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


(defconst extempore-packages
  '(extempore-mode))

(defun extempore/init-extempore-mode ()
  "Initialize extempore mode"
  (use-package extempore-mode
    :defer t
    :mode
    (("\\.xtm$" . extempore-mode))
    :init
    (spacemacs/register-repl 'extempore-mode 'extempore-repl "extempore")
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'extempore-mode "mc" "process")
      (spacemacs/declare-prefix-for-mode 'extempore-mode "me" "eval")

      (spacemacs/set-leader-keys-for-major-mode 'extempore-mode
        "'"  'extempore-repl
        ","  'lisp-state-toggle-lisp-state

        "cc" 'switch-to-extempore
        "cj" 'extempore-connect

        "ee" 'extempore-send-last-sexp
        "ef" 'extempore-send-definition
        "er" 'extempore-send-region
        "eb" 'extempore-send-buffer-or-region
        (setq extempore-tab-completion nil)

        (set-face-attribute 'extempore-blink-face nil :foreground "#272822" :background "#FD971F")
        (set-face-attribute 'extempore-sb-blink-face nil :foreground "#272822" :background "#39FF14")

        ;; stop the ' (quote) character being paired by smartparens
        (with-eval-after-load 'smartparens
          (sp-local-pair 'extempore-mode "'" nil :actions nil)
          (sp-local-pair 'extempore-mode "`" nil :actions nil))))))

(defun extempore/post-init-eldoc ()
  (add-hook 'extempore-mode-hook #'spacemacs//extempore-setup-eldoc))
