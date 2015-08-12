;;; funcs.el --- Markdown Layer functions File for Spacemacs
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


(defun spacemacs/set-markdown-keybindings (mode keymap)
  "Set markdown keybindings for a specific mode and keymap.
   This is a function since the same keybindings are also used in
   the chrome layer and might be useful in other markdown based
   modes."
    (evil-leader/set-key-for-mode mode
        ;; Movement
        "m{"   'markdown-backward-paragraph
        "m}"   'markdown-forward-paragraph
        ;; Completion, and Cycling
        "m]"   'markdown-complete
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
        "mcr"  'gh-md-render-buffer
        ;; headings
        "mhi"  'markdown-insert-header-dwim
        "mhI"  'markdown-insert-header-setext-dwim
        "mh1"  'markdown-insert-header-atx-1
        "mh2"  'markdown-insert-header-atx-2
        "mh3"  'markdown-insert-header-atx-3
        "mh4"  'markdown-insert-header-atx-4
        "mh5"  'markdown-insert-header-atx-5
        "mh6"  'markdown-insert-header-atx-6
        "mh!"  'markdown-insert-header-setext-1
        "mh@"  'markdown-insert-header-setext-2
        ;; Insertion of common elements
        "m-"   'markdown-insert-hr
        "mif"  'markdown-insert-footnote
        "mii"  'markdown-insert-image
        "mik"  'spacemacs/insert-keybinding-markdown
        "miI"  'markdown-insert-reference-image
        "mil"  'markdown-insert-link
        "miL"  'markdown-insert-reference-link-dwim
        "miw"  'markdown-insert-wiki-link
        "miu"  'markdown-insert-uri
        ;; Element removal
        "mk"   'markdown-kill-thing-at-point
        ;; List editing
        "mli"  'markdown-insert-list-item
        ;; region manipulation
        "mxb"  'markdown-insert-bold
        "mxi"  'markdown-insert-italic
        "mxc"  'markdown-insert-code
        "mxq"  'markdown-insert-blockquote
        "mxQ"  'markdown-blockquote-region
        "mxp"  'markdown-insert-pre
        "mxP"  'markdown-pre-region
        ;; Following and Jumping
        "mN"   'markdown-next-link
        "mo"   'markdown-follow-thing-at-point
        "mP"   'markdown-previous-link
        "m <RET>" 'markdown-jump)

    ;; Header navigation in normal state movements
    (evil-define-key 'normal keymap
        "gj" 'outline-forward-same-level
        "gk" 'outline-backward-same-level
        "gh" 'outline-up-heading
        ;; next visible heading is not exactly what we want but close enough
        "gl" 'outline-next-visible-heading)

    ;; Promotion, Demotion
    (define-key keymap (kbd "M-h") 'markdown-promote)
    (define-key keymap (kbd "M-j") 'markdown-move-down)
    (define-key keymap (kbd "M-k") 'markdown-move-up)
    (define-key keymap (kbd "M-l") 'markdown-demote))
