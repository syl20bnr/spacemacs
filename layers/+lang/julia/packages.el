;;; packages.el --- Julia layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Adam Beckmeyer <adam_git@thebeckmeyers.xyz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst julia-packages
  '(
    company-lsp
    evil-surround
    flycheck
    julia-mode
    julia-repl
    lsp-julia
    ))

(defun julia/init-julia-mode ()
  (use-package julia-mode
    :defer t
    :init
    (progn
      (add-hook 'julia-mode-hook #'spacemacs//julia-setup-buffer)
      (add-hook 'julia-mode-local-vars-hook #'spacemacs//julia-setup-lsp)
      (if (and (configuration-layer/layer-used-p 'ess)
               julia-mode-enable-ess)
          (add-to-list 'auto-mode-alist
                       '("\\.jl\\'" . ess-julia-mode)))
      (if (and (not (configuration-layer/layer-used-p 'ess))
               julia-mode-enable-ess)
          (message "`ess' layer is not installed. Please add `ess' layer to your dotfile.")))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'julia-mode "m=" "format")
      (spacemacs/set-leader-keys-for-major-mode 'julia-mode
        "l" 'julia-latexsub-or-indent
        "==" 'julia-indent-line
        "=d" 'julia-manual-deindent
        "=q" 'prog-indent-sexp)

      (when (configuration-layer/package-used-p 'helm)
        (spacemacs/declare-prefix-for-mode 'julia-mode "mi" "insert")
        (spacemacs/set-leader-keys-for-minor-mode 'julia-mode
          "ii" 'spacemacs//julia-helm-math-insert)))))

(defun julia/init-julia-repl ()
  (use-package julia-repl
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'julia-repl 'julia-repl "julia-repl"))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'julia-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'julia-mode "me" "eval")
      (spacemacs/declare-prefix-for-mode 'julia-mode "ms" "send")
      (spacemacs/set-leader-keys-for-minor-mode 'julia-repl-mode
        "'" 'julia-repl
        "r" 'julia-repl
        "hh" 'julia-repl-doc

        "sa" 'julia-repl-activate-parent
        "sd" 'julia-repl-cd
        "si" 'julia-repl
        "sb" 'julia-repl-send-buffer
        "st" 'julia-repl-includet-buffer
        "sl" 'julia-repl-send-line
        "ss" 'julia-repl-send-line
        "sr" 'julia-repl-send-region-or-line
        "sm" 'julia-repl-macroexpand
        "se" 'julia-repl-edit
        "sv" 'julia-repl-prompt-set-inferior-buffer-name-suffix

        "ea" 'julia-repl-activate-parent
        "ed" 'julia-repl-cd
        "eb" 'julia-repl-send-buffer
        "et" 'julia-repl-includet-buffer
        "el" 'julia-repl-send-line
        "es" 'julia-repl-send-line
        "er" 'julia-repl-send-region-or-line
        "em" 'julia-repl-macroexpand
        "ee" 'julia-repl-edit
        "ev" 'julia-repl-prompt-set-inferior-buffer-name-suffix))))


(defun julia/post-init-evil-surround ()
  (use-package evil-surround
    :config
    (progn
      (add-hook
       'julia-mode-hook
       #'(lambda ()
           (add-to-list 'evil-surround-pairs-alist '(?b . ("begin " . " end")))
           (add-to-list 'evil-surround-pairs-alist '(?q . ("quote " . " end")))
           (add-to-list 'evil-surround-pairs-alist '(?: . (":("     .    ")")))
           (add-to-list 'evil-surround-pairs-alist '(?l . ("let "   . " end"))))))))

(defun julia/init-lsp-julia ()
  (use-package lsp-julia
    :config
    (progn
      (push 'xref-find-definitions spacemacs-jump-handlers-julia-mode))))

(defun julia/post-init-company-lsp ()
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes julia-mode
    :variables
    company-minimum-prefix-length 0
    company-idle-delay 0.5))

(defun julia/post-init-flycheck ()
  (spacemacs/enable-flycheck 'julia-mode))
