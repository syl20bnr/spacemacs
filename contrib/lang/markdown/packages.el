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

(setq markdown-packages
  '(
    markdown-mode
    markdown-toc
    ))

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
        ;; Insertion of common elements
        "m-"   'markdown-insert-hr
        "mil"   'markdown-insert-link
        "miL"   'markdown-insert-reference-link-dwim
        "miu"   'markdown-insert-uri
        "mif"   'markdown-insert-footnote
        "miw"   'markdown-insert-wiki-link
        "mii"   'markdown-insert-image
        "miI"   'markdown-insert-reference-image
        ;; headings
        "mhh"   'markdown-insert-header-dwim
        "mhH"   'markdown-insert-header-setext-dwim
        "mh1"   'markdown-insert-header-atx-1
        "mh2"   'markdown-insert-header-atx-2
        "mh3"   'markdown-insert-header-atx-3
        "mh4"   'markdown-insert-header-atx-4
        "mh5"   'markdown-insert-header-atx-5
        "mh6"   'markdown-insert-header-atx-6
        "mh!"   'markdown-insert-header-setext-1
        "mh@"   'markdown-insert-header-setext-2
        ;; region manipulation
        "mrb"   'markdown-insert-bold
        "mri"   'markdown-insert-italic
        "mrc"   'markdown-insert-code
        "mrq"   'markdown-insert-blockquote
        "mrQ"   'markdown-blockquote-region
        "mrp"   'markdown-insert-pre
        "mrP"   'markdown-pre-region
        ;; Element removal
        "mk"    'markdown-kill-thing-at-point
        ;; Completion, and Cycling
        "m]"    'markdown-complete
        ;; Following and Jumping
        "mo"   'markdown-follow-thing-at-point
        "m <RET>"   'markdown-jump
        ;; Indentation
        "m>"   'markdown-indent-region
        "m<"   'markdown-exdent-region
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
        "mli"  'markdown-insert-list-item
        ;; Movement
        "m{"   'markdown-backward-paragraph
        "m}"   'markdown-forward-paragraph
        "mN"   'markdown-next-link
        "mP"   'markdown-previous-link)

      ;; Header navigation in normal state movements
      (evil-define-key 'normal markdown-mode-map
        "gj" 'outline-forward-same-level
        "gk" 'outline-backward-same-level
        "gh" 'outline-up-heading
        ;; next visible heading is not exactly what we want but close enough
        "gl" 'outline-next-visible-heading
        )

      ;; Promotion, Demotion
      (define-key markdown-mode-map (kbd "M-k") 'markdown-move-up)
      (define-key markdown-mode-map (kbd "M-j") 'markdown-move-down)
      (define-key markdown-mode-map (kbd "M-h") 'markdown-promote)
      (define-key markdown-mode-map (kbd "M-l") 'markdown-demote))))

(defun markdown/init-markdown-toc ()
  (use-package markdown-toc
    :defer t))
