;;; packages.el --- Finance Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar markdown-packages
  '(
    markdown-mode
    markdown-toc
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun markdown/init-markdown-mode ()
  (use-package markdown-mode
    :mode ("\\.m[k]d" . markdown-mode)
    :defer t
    :init (add-hook 'markdown-mode-hook 'smartparens-mode)
    :config
    ;; Don't do terrible things with Github code blocks (```)
    (when (fboundp 'sp-local-pair)
      (sp-local-pair 'markdown-mode "`" nil :actions '(:rem autoskip))
      (sp-local-pair 'markdown-mode "'" nil :actions nil))
    (progn
      (evil-leader/set-key-for-mode 'markdown-mode
        ;; Element insertion
        "m\""   'markdown-insert-hr
        "mal"   'markdown-insert-link
        "maL"   'markdown-insert-reference-link-dwim
        "mau"   'markdown-insert-uri
        "maf"   'markdown-insert-footnote
        "maw"   'markdown-insert-wiki-link
        "mii"   'markdown-insert-image
        "miI"   'markdown-insert-reference-image
        "mth"   'markdown-insert-header-dwim
        "mtH"   'markdown-insert-header-setext-dwim
        "mt1"   'markdown-insert-header-atx-1
        "mt2"   'markdown-insert-header-atx-2
        "mt3"   'markdown-insert-header-atx-3
        "mt4"   'markdown-insert-header-atx-4
        "mt5"   'markdown-insert-header-atx-5
        "mt6"   'markdown-insert-header-atx-6
        "mt!"   'markdown-insert-header-setext-1
        "mt@"   'markdown-insert-header-setext-2
        "mss"   'markdown-insert-bold
        "mse"   'markdown-insert-italic
        "msc"   'markdown-insert-code
        "msb"   'markdown-insert-blockquote
        "msB"   'markdown-blockquote-region
        "msp"   'markdown-insert-pre
        "msP"   'markdown-pre-region
        ;; Element removal
        "mk"    'markdown-kill-thing-at-point
        ;; Promotion, Demotion, Completion, and Cycling
        "m="    'markdown-promote
        "m-"    'markdown-demote
        "m]"    'markdown-complete
        ;; Following and Jumping
        "mo"   'markdown-follow-thing-at-point
        "mj"   'markdown-jump
        ;; Indentation
        "m>"   'markdown-indent-region
        "m<"   'markdown-exdent-region
        ;; Header navigation
        "mn"   'outline-next-visible-heading
        "mp"   'outline-previous-visible-heading
        "mf"   'outline-forward-same-level
        "mb"   'outline-backward-same-level
        "mu"   'outline-up-heading
        ;; Buffer-wide commands
        "mc]"  'markdown-complete-buffer
        "mcm"  'markdown-other-window
        "mcp"  'markdown-preview
        "mce"  'markdown-export
        "mcv"  'markdown-export-and-preview
        "mco"  'markdown-open
        "mcw"  'markdown-kill-ring-save
        "mcc"  'markdown-check-refs
        "mcn"  'markdown-cleanup-list-numbers
        ;; List editing
        "mlk"  'markdown-move-up
        "mlj"  'markdown-move-down
        "mlh"  'markdown-promote
        "mll"  'markdown-demote
        "mli"  'markdown-insert-list-item
        ;; Movement
        "m{"   'markdown-backward-paragraph
        "m}"   'markdown-forward-paragraph
        "mN"   'markdown-next-link
        "mP"   'markdown-previous-link))))

(defun markdown/init-markdown-toc ()
  (use-package markdown-toc
    :defer t))
