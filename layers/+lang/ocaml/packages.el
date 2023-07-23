;;; packages.el --- ocaml Layer packages File for Spacemacs
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


(defconst ocaml-packages
  '(
    company
    dune
    evil-matchit
    flycheck
    (flycheck-ocaml :toggle (configuration-layer/layer-used-p 'syntax-checking))
    ggtags
    counsel-gtags
    imenu
    merlin
    merlin-company
    merlin-eldoc
    merlin-iedit
    ocamlformat
    ocp-indent
    smartparens
    tuareg
    utop))

(defun ocaml/post-init-company ()
  (when (and (configuration-layer/package-used-p 'merlin)
             (configuration-layer/package-used-p 'merlin-company))
    (spacemacs|add-company-backends
      :backends merlin-company-backend
      :modes merlin-mode
      :variables merlin-completion-with-doc t)))

(defun ocaml/init-dune ()
  (use-package dune
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
      "tP" 'dune-promote
      "tp" 'dune-runtest-and-promote)
    (spacemacs/declare-prefix-for-mode 'tuareg-mode "mt" "test")
    (spacemacs/declare-prefix-for-mode 'dune-mode "mc" "compile/check")
    (spacemacs/declare-prefix-for-mode 'dune-mode "mi" "insert-form")
    (spacemacs/declare-prefix-for-mode 'dune-mode "mt" "test")
    (spacemacs/set-leader-keys-for-major-mode 'dune-mode
      "cc" 'compile
      "ia" 'dune-insert-alias-form
      "ic" 'dune-insert-copyfiles-form
      "id" 'dune-insert-ignored-subdirs-form
      "ie" 'dune-insert-executable-form
      "ii" 'dune-insert-install-form
      "il" 'dune-insert-library-form
      "im" 'dune-insert-menhir-form
      "ip" 'dune-insert-ocamllex-form
      "ir" 'dune-insert-rule-form
      "it" 'dune-insert-tests-form
      "iv" 'dune-insert-env-form
      "ix" 'dune-insert-executables-form
      "iy" 'dune-insert-ocamlyacc-form
      "tP" 'dune-promote
      "tp" 'dune-runtest-and-promote)))

(defun ocaml/post-init-evil-matchit ()
  (add-hook 'tuareg-mode-hook #'evil-matchit-mode))

(defun ocaml/post-init-flycheck ()
  (spacemacs/enable-flycheck 'tuareg-mode))

(defun ocaml/init-flycheck-ocaml ()
  (use-package flycheck-ocaml
    :defer t
    :init
    (with-eval-after-load 'merlin
      (setq merlin-error-after-save nil)
      (flycheck-ocaml-setup))))

(defun ocaml/post-init-ggtags ()
  (add-hook 'ocaml-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun ocaml/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'ocaml-mode))

(defun ocaml/init-merlin ()
  (use-package merlin
    :defer t
    :init
    (add-to-list 'spacemacs-jump-handlers-tuareg-mode
                 'spacemacs/merlin-locate)
    (add-hook 'tuareg-mode-hook 'merlin-mode)

    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
      "cp" 'merlin-project-check
      "cv" 'merlin-goto-project-file
      "Ec" 'merlin-error-check
      "En" 'merlin-error-next
      "EN" 'merlin-error-prev
      "gb" 'merlin-pop-stack
      "gG" 'spacemacs/merlin-locate-other-window
      "gl" 'merlin-locate-ident
      "gi" 'merlin-switch-to-ml
      "gI" 'merlin-switch-to-mli
      "go" 'merlin-occurrences
      "hh" 'merlin-document
      "ht" 'merlin-type-enclosing
      "hT" 'merlin-type-expr
      "rc" 'merlin-construct
      "rd" 'merlin-destruct)
    (spacemacs/declare-prefix-for-mode 'tuareg-mode "mc" "compile/check")
    (spacemacs/declare-prefix-for-mode 'tuareg-mode "mE" "errors")
    (spacemacs/declare-prefix-for-mode 'tuareg-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'tuareg-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'tuareg-mode "mr" "refactor")))

(defun ocaml/init-merlin-company ()
  (use-package merlin-company
    :defer t))

(defun ocaml/init-merlin-iedit ()
  (use-package merlin-iedit
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
      "re" 'merlin-iedit-occurrences)))

(defun ocaml/post-init-imenu ()
  (add-hook 'merlin-mode-hook #'merlin-use-merlin-imenu))

(defun ocaml/init-merlin-eldoc ()
  (use-package merlin-eldoc
    :defer t
    :hook (merlin-mode . merlin-eldoc-setup)))

(defun ocaml/init-ocamlformat ()
  (use-package ocamlformat
    :defer t
    :init
    (when ocaml-format-on-save
      (add-hook 'before-save-hook 'ocamlformat-before-save))))

(defun ocaml/init-ocp-indent ()
  (use-package ocp-indent
    :defer t
    :init
    (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup)
    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
      "=" 'ocp-indent-buffer)))

(defun ocaml/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
    (sp-local-pair 'tuareg-mode "'" nil :actions nil)
    (sp-local-pair 'tuareg-mode "`" nil :actions nil)))

(defun ocaml/init-tuareg ()
  (use-package tuareg
    :bind (:map tuareg-mode-map
                ;; Workaround to preserve vim backspace in normal mode
                ([backspace] . nil))
    :mode (("\\.ml[ily]?$" . tuareg-mode)
           ("\\.topml$" . tuareg-mode))
    :defer t
    :init
    (spacemacs//init-ocaml-opam)
    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
      "ga" 'tuareg-find-alternate-file
      "cc" 'compile)
    ;; Make OCaml-generated files invisible to filename completion
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
      (add-to-list 'completion-ignored-extensions ext))))

(defun ocaml/init-utop ()
  (use-package utop
    :defer t
    :init
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    (spacemacs/register-repl 'utop 'utop "ocaml")
    :config
    (if (executable-find "opam")
        (setq utop-command "opam config exec -- utop -emacs")
      (spacemacs-buffer/warning "Cannot find \"opam\" executable."))

    (defun spacemacs/utop-eval-phrase-and-go ()
      "Send phrase to REPL and evaluate it and switch to the REPL in
`insert state'"
      (interactive)
      (utop-eval-phrase)
      (utop)
      (evil-insert-state))

    (defun spacemacs/utop-eval-buffer-and-go ()
      "Send buffer to REPL and evaluate it and switch to the REPL in
`insert state'"
      (interactive)
      (utop-eval-buffer)
      (utop)
      (evil-insert-state))

    (defun spacemacs/utop-eval-region-and-go (start end)
      "Send region to REPL and evaluate it and switch to the REPL in
`insert state'"
      (interactive "r")
      (utop-eval-region start end)
      (utop)
      (evil-insert-state))

    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
      "'"  'utop
      "sb" 'utop-eval-buffer
      "sB" 'spacemacs/utop-eval-buffer-and-go
      "si" 'utop
      "sp" 'utop-eval-phrase
      "sP" 'spacemacs/utop-eval-phrase-and-go
      "sr" 'utop-eval-region
      "sR" 'spacemacs/utop-eval-region-and-go)
    (spacemacs/declare-prefix-for-mode 'tuareg-mode "ms" "send")
    (define-key utop-mode-map (kbd "C-j") 'utop-history-goto-next)
    (define-key utop-mode-map (kbd "C-k") 'utop-history-goto-prev)))
