;;; packages.el --- Spacemacs Editing Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-editing-packages
      '(aggressive-indent
        avy
        bracketed-paste
        clean-aindent-mode
        eval-sexp-fu
        expand-region
        (hexl :location built-in)
        hungry-delete
        iedit
        link-hint
        lorem-ipsum
        move-text
        pcre2el
        smartparens
        uuidgen))

;; Initialization of packages

(defun spacemacs-editing/init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :init
    (progn
      (spacemacs|add-toggle aggressive-indent
        :status aggressive-indent-mode
        :on (aggressive-indent-mode)
        :off (aggressive-indent-mode -1)
        :documentation "Always keep code indented."
        :evil-leader "tI")
      (spacemacs|add-toggle aggressive-indent-globally
        :status aggressive-indent-mode
        :on (global-aggressive-indent-mode)
        :off (global-aggressive-indent-mode -1)
        :documentation "Always keep code indented globally."
        :evil-leader "t C-I"))
    :config
    (progn
      (add-hook 'diff-auto-refine-mode-hook 'spacemacs/toggle-aggressive-indent-off)
      (spacemacs|diminish aggressive-indent-mode " Ⓘ" " I"))))

(defun spacemacs-editing/init-avy ()
  (use-package avy
    :defer t
    :commands (spacemacs/avy-open-url spacemacs/avy-goto-url avy-pop-mark)
    :init
    (progn
      (setq avy-all-windows 'all-frames)
      (setq avy-background t)
      (spacemacs/set-leader-keys
        "jj" 'evil-avy-goto-char
        "jJ" 'evil-avy-goto-char-2
        "jl" 'evil-avy-goto-line
        "ju" 'avy-pop-mark
        "jU" 'spacemacs/avy-goto-url
        "jw" 'evil-avy-goto-word-or-subword-1
        "xo" 'spacemacs/avy-open-url))
    :config
    (progn
      (defun spacemacs/avy-goto-url()
        "Use avy to go to an URL in the buffer."
        (interactive)
        (avy--generic-jump "https?://" nil 'pre))
      (defun spacemacs/avy-open-url ()
        "Use avy to select an URL in the buffer and open it."
        (interactive)
        (save-excursion
          (spacemacs/avy-goto-url)
          (browse-url-at-point))))))

(defun spacemacs-editing/init-bracketed-paste ()
  (use-package bracketed-paste
    :defer t
    :init
    ;; Enable bracketed-paste for tty
    (add-hook 'tty-setup-hook 'bracketed-paste-enable)))

(defun spacemacs-editing/init-clean-aindent-mode ()
  (use-package clean-aindent-mode
    :defer t
    :init
    (add-hook 'prog-mode-hook 'clean-aindent-mode)))

(defun spacemacs-editing/init-eval-sexp-fu ()
  ;; ignore obsolete function warning generated on startup
  (let ((byte-compile-not-obsolete-funcs (append byte-compile-not-obsolete-funcs '(preceding-sexp))))
    (require 'eval-sexp-fu)))

(defun spacemacs-editing/init-expand-region ()
  (use-package expand-region
    :defer t
    :init (spacemacs/set-leader-keys "v" 'er/expand-region)
    :config
    (progn
      ;; add search capability to expand-region
      (when (configuration-layer/package-usedp 'helm-ag)
        (defadvice er/prepare-for-more-expansions-internal
            (around helm-ag/prepare-for-more-expansions-internal activate)
          ad-do-it
          (let ((new-msg (concat (car ad-return-value)
                                 ", / to search in project, "
                                 "f to search in files, "
                                 "b to search in opened buffers"))
                (new-bindings (cdr ad-return-value)))
            (cl-pushnew
             '("/" (lambda ()
                     (call-interactively
                      'spacemacs/helm-project-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("f" (lambda ()
                     (call-interactively
                      'spacemacs/helm-files-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("b" (lambda ()
                     (call-interactively
                      'spacemacs/helm-buffers-smart-do-search-region-or-symbol)))
             new-bindings)
            (setq ad-return-value (cons new-msg new-bindings)))))
      (setq expand-region-contract-fast-key "V"
            expand-region-reset-fast-key "r"))))

(defun spacemacs-editing/init-hexl ()
  (use-package hexl
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "fh" 'hexl-find-file)
      (spacemacs/set-leader-keys-for-major-mode 'hexl-mode
        "d" 'hexl-insert-decimal-char
        "c" 'hexl-insert-octal-char
        "x" 'hexl-insert-hex-char
        "X" 'hexl-insert-hex-string
        "g" 'hexl-goto-address)
      (evil-define-key 'motion hexl-mode-map
        "]]" 'hexl-end-of-1k-page
        "[[" 'hexl-beginning-of-1k-page
        "h" 'hexl-backward-char
        "l" 'hexl-forward-char
        "j" 'hexl-next-line
        "k" 'hexl-previous-line
        "$" 'hexl-end-of-line
        "^" 'hexl-beginning-of-line
        "0" 'hexl-beginning-of-line))))

(defun spacemacs-editing/init-hungry-delete ()
  (use-package hungry-delete
    :defer t
    :init
    (spacemacs|add-toggle hungry-delete
      :status hungry-delete-mode
      :on (hungry-delete-mode)
      :off (hungry-delete-mode -1)
      :documentation "Delete consecutive horizontal whitespace with a single key."
      :evil-leader "td")
    :config
    (progn
      (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
      (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
      (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char))))

(defun spacemacs-editing/init-iedit ()
  (use-package iedit
    :defer t
    :init
    (progn
      (setq iedit-current-symbol-default t
            iedit-only-at-symbol-boundaries t
            iedit-toggle-key-default nil))
    :config
    (progn
      (defun iedit-toggle-selection ()
        "Override default iedit function to be able to add arbitrary overlays.

It will toggle the overlay under point or create an overlay of one character."
        (interactive)
        (iedit-barf-if-buffering)
        (let ((ov (iedit-find-current-occurrence-overlay)))
          (if ov
              (iedit-restrict-region (overlay-start ov) (overlay-end ov) t)
            (save-excursion
              (push (iedit-make-occurrence-overlay (point) (1+ (point)))
                    iedit-occurrences-overlays))
            (setq iedit-mode
                  (propertize
                   (concat " Iedit:" (number-to-string
                                      (length iedit-occurrences-overlays)))
                   'face 'font-lock-warning-face))
            (force-mode-line-update)))))))

(defun spacemacs-editing/init-link-hint ()
  (use-package link-hint
    :defer t
    :init
    (spacemacs/set-leader-keys
      "xo" 'link-hint-open-link
      "xO" 'link-hint-open-multiple-links)))

(defun spacemacs-editing/init-lorem-ipsum ()
  (use-package lorem-ipsum
    :commands (lorem-ipsum-insert-list
               lorem-ipsum-insert-paragraphs
               lorem-ipsum-insert-sentences)
    :init
    (progn
      (spacemacs/declare-prefix "il" "lorem ipsum")
      (spacemacs/set-leader-keys
        "ill" 'lorem-ipsum-insert-list
        "ilp" 'lorem-ipsum-insert-paragraphs
        "ils" 'lorem-ipsum-insert-sentences))))

(defun spacemacs-editing/init-move-text ()
  (use-package move-text
    :defer t
    :init
    (spacemacs|define-transient-state move-text
      :title "Move Text Transient State"
      :bindings
      ("J" move-text-down "move down")
      ("K" move-text-up "move up"))
    (spacemacs/set-leader-keys
      "xJ" 'spacemacs/move-text-transient-state/move-text-down
      "xK" 'spacemacs/move-text-transient-state/move-text-up)))

(defun spacemacs-editing/init-pcre2el ()
  (use-package pcre2el
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "R" "pcre2el")
      (spacemacs/set-leader-keys
        "R/"  'rxt-explain
        "Rc"  'rxt-convert-syntax
        "Rx"  'rxt-convert-to-rx
        "R'"  'rxt-convert-to-strings
        "Rpe" 'rxt-pcre-to-elisp
        "R%"  'pcre-query-replace-regexp
        "Rpx" 'rxt-pcre-to-rx
        "Rps" 'rxt-pcre-to-sre
        "Rp'" 'rxt-pcre-to-strings
        "Rp/" 'rxt-explain-pcre
        "Re/" 'rxt-explain-elisp
        "Rep" 'rxt-elisp-to-pcre
        "Rex" 'rxt-elisp-to-rx
        "Res" 'rxt-elisp-to-sre
        "Re'" 'rxt-elisp-to-strings
        "Ret" 'rxt-toggle-elisp-rx
        "Rt"  'rxt-toggle-elisp-rx))))

(defun spacemacs-editing/init-smartparens ()
  (use-package smartparens
    :defer t
    :commands (sp-split-sexp sp-newline sp-up-sexp)
    :init
    (progn
      (spacemacs/add-to-hooks (if dotspacemacs-smartparens-strict-mode
                                  'smartparens-strict-mode
                                'smartparens-mode)
                              '(prog-mode-hook))

      ;; enable smartparens-mode in `eval-expression'
      (defun conditionally-enable-smartparens-mode ()
        "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
        (if (eq this-command 'eval-expression)
            (smartparens-mode)))

      (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

      (spacemacs|add-toggle smartparens
        :status smartparens-mode
        :on (smartparens-mode)
        :off (smartparens-mode -1)
        :documentation "Enable smartparens."
        :evil-leader "tp")

      (spacemacs|add-toggle smartparens-globally
        :status smartparens-mode
        :on (smartparens-global-mode)
        :off (smartparens-global-mode -1)
        :documentation "Enable smartparens globally."
        :evil-leader "t C-p")

      (setq sp-show-pair-delay 0.2
            ;; fix paren highlighting in normal mode
            sp-show-pair-from-inside t
            sp-cancel-autoskip-on-backward-movement nil)

      (spacemacs/set-leader-keys
        "js" 'sp-split-sexp
        "jn" 'sp-newline))
    :config
    (progn
      (require 'smartparens-config)
      (spacemacs|diminish smartparens-mode " ⓟ" " p")

      (show-smartparens-global-mode +1)

      (defun spacemacs/smartparens-pair-newline (id action context)
        (save-excursion
          (newline)
          (indent-according-to-mode)))

      (defun spacemacs/smartparens-pair-newline-and-indent (id action context)
        (spacemacs/smartparens-pair-newline id action context)
        (indent-according-to-mode))

      ;; don't create a pair with single quote in minibuffer
      (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

      (sp-pair "{" nil :post-handlers
               '(:add (spacemacs/smartparens-pair-newline-and-indent "RET")))
      (sp-pair "[" nil :post-handlers
               '(:add (spacemacs/smartparens-pair-newline-and-indent "RET")))

      (defun spacemacs/smart-closing-parenthesis ()
        (interactive)
        (let* ((sp-navigate-close-if-unbalanced t)
               (current-pos (point))
               (current-line (line-number-at-pos current-pos))
               (next-pos (save-excursion
                           (sp-up-sexp)
                           (point)))
               (next-line (line-number-at-pos next-pos)))
          (cond
           ((and (= current-line next-line)
                 (not (= current-pos next-pos)))
            (sp-up-sexp))
           (t
            (insert-char ?\))))))
      (when dotspacemacs-smart-closing-parenthesis
          (define-key evil-insert-state-map ")" 'spacemacs/smart-closing-parenthesis)))))

(defun spacemacs-editing/init-uuidgen ()
  (use-package uuidgen
    :commands (uuidgen-1 uuidgen-4)
    :init
    (progn
      (spacemacs/declare-prefix "iU" "uuid")
      (spacemacs/set-leader-keys
        "iU1" 'spacemacs/uuidgen-1
        "iU4" 'spacemacs/uuidgen-4
        "iUU" 'spacemacs/uuidgen-4))))

