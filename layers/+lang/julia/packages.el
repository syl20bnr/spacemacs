;;; packages.el --- julia layer packages file for Spacemacs.
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
    (julia-mode :location elpa)
    (julia-repl :location elpa)
    (lsp-julia :location (recipe :fetcher github
                                 :repo "non-Jedi/lsp-julia"))
    flycheck
    company-lsp
    evil-surround
    ))

(defun julia/init-julia-mode ()
  (use-package julia-mode
    :defer t
    :init (progn
            (add-hook 'julia-mode-hook #'spacemacs//julia-setup-buffer)
            (if (and (configuration-layer/layer-used-p 'ess)
                     julia-mode-enable-ess)
                (add-to-list 'auto-mode-alist
                             '("\\.jl\\'" . ess-julia-mode)))
            (if (and (not (configuration-layer/layer-used-p 'ess))
                     julia-mode-enable-ess)
                (message "`ess' layer is not installed. Please add `ess' layer to your dotfile.")))
    :config (progn
              (spacemacs/declare-prefix-for-mode 'julia-mode
                "mh" "help")
              (spacemacs/declare-prefix-for-mode 'julia-mode
                "me" "eval")
              (spacemacs/declare-prefix-for-mode 'julia-mode
                "m=" "format")
              (spacemacs/set-leader-keys-for-major-mode 'julia-mode
                "el" 'julia-latexsub
                "==" 'julia-indent-line))))

(defun julia/init-julia-repl ()
  (use-package julia-repl
    :defer t
    :init (progn
            (spacemacs/register-repl 'julia-repl 'julia-repl
                                     "julia-repl"))
    :config (progn
              (spacemacs/declare-prefix-for-mode 'julia-repl-mode
                "ms" "send")
              (spacemacs/set-leader-keys-for-minor-mode
                'julia-repl-mode
                "'" 'julia-repl-edit
                "hh" 'julia-repl-doc
                "w" 'julia-repl-workspace
                "em" 'julia-repl-macroexpand
                "r"  'julia-repl
                "si" 'julia-repl
                "sb" 'julia-repl-send-buffer
                "sl" 'julia-repl-send-line
                "sr" 'julia-repl-send-region-or-line
                "em" 'julia-repl-macroexpand))))

(defun julia/post-init-evil-surround ()
  (with-eval-after-load 'evil-surround
    (add-to-list 'evil-surround-pairs-alist '(?b . ("begin " . " end")))
    (add-to-list 'evil-surround-pairs-alist '(?q . ("quote " . " end")))
    (add-to-list 'evil-surround-pairs-alist '(?: . (":("     .    ")")))
    (add-to-list 'evil-surround-pairs-alist '(?l . ("let "   . " end")))))

(defun julia/init-lsp-julia ()
  (use-package lsp-julia
    :config (progn
              (push 'xref-find-definitions spacemacs-jump-handlers-julia-mode))
    :commands lsp-julia-enable))

(defun julia/post-init-company-lsp ()
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes julia-mode
    :variables
    company-minimum-prefix-length 0
    company-idle-delay 0.5))

(defun julia/post-init-flycheck ()
  (spacemacs/enable-flycheck 'julia-mode))
;;; packages.el ends here
