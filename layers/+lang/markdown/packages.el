;;; packages.el --- Markdown Layer packages File for Spacemacs
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
    emoji-cheat-sheet-plus
    gh-md
    markdown-mode
    markdown-toc
    mmm-mode
    company
    company-emoji
    smartparens
    ))

(defun markdown/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'markdown-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun markdown/init-gh-md ()
  (use-package gh-md
    :defer t
    :init
    (evil-leader/set-key-for-mode 'markdown-mode
      "mcr"  'gh-md-render-buffer)))

(defun markdown/post-init-smartparens ()
  (add-hook 'markdown-mode-hook 'smartparens-mode))

(defun markdown/init-markdown-mode ()
  (use-package markdown-mode
    :mode ("\\.m[k]d" . markdown-mode)
    :defer t
    :config
    (progn
      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
      (defun spacemacs/insert-keybinding-markdown (key)
        "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
        (interactive "kType key sequence: ")
        (let* ((tag "~%s~"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -6))))
      (evil-leader/set-key-for-mode 'markdown-mode
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
        "mf"   'markdown-follow-thing-at-point
        "mP"   'markdown-previous-link
        "m <RET>" 'markdown-jump)

      ;; Header navigation in normal state movements
      (evil-define-key 'normal markdown-mode-map
        "gj" 'outline-forward-same-level
        "gk" 'outline-backward-same-level
        "gh" 'outline-up-heading
        ;; next visible heading is not exactly what we want but close enough
        "gl" 'outline-next-visible-heading)

      ;; Promotion, Demotion
      (define-key markdown-mode-map (kbd "M-h") 'markdown-promote)
      (define-key markdown-mode-map (kbd "M-j") 'markdown-move-down)
      (define-key markdown-mode-map (kbd "M-k") 'markdown-move-up)
      (define-key markdown-mode-map (kbd "M-l") 'markdown-demote))))

(defun markdown/init-markdown-toc ()
  (use-package markdown-toc
    :defer t))

(defun markdown/init-mmm-mode ()
  (use-package mmm-mode
    :commands mmm-parse-buffer
    :init
    (evil-leader/set-key-for-mode 'markdown-mode
      ;; Highlight code blocks
      "mcs"   'mmm-parse-buffer)
    :config
    (progn
      (mmm-add-classes '((markdown-python
                          :submode python-mode
                          :face mmm-declaration-submode-face
                          :front "^```python[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-html
                          :submode web-mode
                          :face mmm-declaration-submode-face
                          :front "^```html[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-java
                          :submode java-mode
                          :face mmm-declaration-submode-face
                          :front "^```java[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-ruby
                          :submode ruby-mode
                          :face mmm-declaration-submode-face
                          :front "^```ruby[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-c
                          :submode c-mode
                          :face mmm-declaration-submode-face
                          :front "^```c[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-c++
                          :submode c++-mode
                          :face mmm-declaration-submode-face
                          :front "^```c\+\+[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-elisp
                          :submode emacs-lisp-mode
                          :face mmm-declaration-submode-face
                          :front "^```elisp[\n\r]+"
                          :back "^```$")))
      (setq mmm-global-mode t)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-java)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ruby)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c++)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-elisp)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-html))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun markdown/post-init-company ()
    (spacemacs|add-company-hook markdown-mode)
    (push 'company-capf company-backends-markdown-mode))
  (defun markdown/post-init-company-emoji ()
    (push 'company-emoji company-backends-markdown-mode)))
