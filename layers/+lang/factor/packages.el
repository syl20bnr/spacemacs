;;; packages.el --- Factor Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: timor <timor.dd@googlemail.com>
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


(defconst factor-packages
  '(
    ;; Assume that factor is installed, and emacs lisp files are correctly
    ;; located in site-lisp
    (fuel :location site)
    yasnippet
    ))

;; for some reason, the following does not work
(defun factor/post-init-yasnippet ()
  (add-to-list 'yas-snippet-dirs (expand-file-name
                                 "snippets"
                                 (configuration-layer/get-layer-local-dir
                                  'factor))
               t)
  (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(factor-mode-hook fuel-mode-hook)))

(defun factor/init-fuel ()
  (use-package factor-mode
    :commands factor-mode run-factor fuel-mode
    :mode ("factor\\'" . factor-mode)
    :init
    (spacemacs/register-repl 'fuel-mode 'run-factor)
    :config
    (require 'fuel-mode)
    (mapc (lambda (x)
            (spacemacs/declare-prefix-for-mode 'factor-mode (car x) (cdr x)))
          '(("mh" . "help")
            ("me" . "eval")
            ("mc" . "compile")
            ("mg" . "nav")
            ("ms" . "repl")
            ("mS" . "scaffold")))
    (spacemacs/set-leader-keys-for-major-mode 'factor-mode
      "'" 'run-factor

      "cc" 'fuel-run-file

      "ef" 'fuel-eval-definition
      "er" 'fuel-eval-region
      "eR" 'fuel-eval-extended-region

      "gg" 'fuel-edit-word-at-point
      "ga" 'factor-visit-other-file

      "ta" 'fuel-test-vocab

      "rs" 'fuel-refactor-extract-sexp
      "rr" 'fuel-refactor-extract-region
      "rv" 'fuel-refactor-extract-vocab
      "ri" 'fuel-refactor-inline-word
      "rw" 'fuel-refactor-rename-word
      "ra" 'fuel-refactor-extract-article
      "rg" 'fuel-refactor-make-generic
      "ru" 'fuel-update-usings

      "ss" 'run-factor

      "hh" 'fuel-help
      "he" 'factor//fuel-stack-effect
      "hp" 'fuel-apropos
      "hv" 'fuel-show-file-words
      "h<" 'fuel-show-callers
      "h>" 'fuel-show-callees

      "Sv" 'fuel-scaffold-vocab
      "Sh" 'fuel-scaffold-help
      )

    (spacemacs/set-leader-keys-for-major-mode 'fuel-listener-mode
      "v" 'fuel-edit-vocabulary
      "r" 'fuel-refresh-all
      "Ts" 'fuel-stack-mode
      "h" 'fuel-help
      "Sv" 'fuel-scaffold-vocab
      )

    (evilified-state-evilify-map fuel-help-mode-map
      :mode fuel-help-mode)
    (dolist (mode '(fuel-debug-uses-mode fuel-debug-mode))
      (evil-set-initial-state mode 'insert))))
