;;; packages.el --- Spacemacs Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-packages
      '(
        ;; default
        ace-link
        ace-window
        adaptive-wrap
        aggressive-indent
        auto-highlight-symbol
        avy
        bracketed-paste
        buffer-move
        (centered-cursor :location local)
        clean-aindent-mode
        define-word
        desktop
        doc-view
        eval-sexp-fu
        evil-anzu
        evil-args
        evil-exchange
        evil-iedit-state
        evil-indent-plus
        evil-jumper
        evil-lisp-state
        ;; for testing purpose, contribute by reporting bugs and sending PRs
        ;; to https://github.com/gabesoft/evil-mc
        ;; To enable it add `(global-evil-mc-mode)' to user-config function
        evil-mc
        evil-nerd-commenter
        evil-matchit
        evil-numbers
        evil-search-highlight-persist
        ;; Temporarily disabled, pending the resolution of
        ;; https://github.com/7696122/evil-terminal-cursor-changer/issues/8
        (evil-terminal-cursor-changer :excluded t)
        evil-tutor
        (evil-unimpaired :skip-install t)
        expand-region
        fancy-battery
        flx-ido
        golden-ratio
        google-translate
        highlight-indentation
        highlight-numbers
        highlight-parentheses
        ;; waiting for an overlay bug to be fixed
        ;; see https://github.com/syl20bnr/spacemacs/issues/2529
        (hl-anything :excluded t)
        hungry-delete
        info+
        iedit
        indent-guide
        open-junk-file
        leuven-theme
        linum-relative
        lorem-ipsum
        move-text
        neotree
        pcre2el
        rainbow-delimiters
        recentf
        smartparens
        smooth-scrolling
        spaceline
        vi-tilde-fringe
        volatile-highlights
        window-numbering
        (zoom-frm :location local)
        ))

;; Paradox from MELPA is not compatible with 24.3, so we use
;; a local paradox with 24.3
(if  (version< emacs-version "24.4")
    (push '(paradox :location local) spacemacs-packages)
  (push 'paradox spacemacs-packages))

;; Initialization of packages

(defun spacemacs/init-ace-link ()
  (use-package ace-link
    :commands spacemacs/ace-buffer-links
    :init
    (progn
      (define-key spacemacs-buffer-mode-map "o" 'spacemacs/ace-buffer-links)
      (with-eval-after-load 'info
        (define-key Info-mode-map "o" 'ace-link-info))
      (with-eval-after-load 'help-mode
        (define-key help-mode-map "o" 'ace-link-help))
      (with-eval-after-load 'eww
        (define-key eww-link-keymap "o" 'ace-link-eww)
        (define-key eww-mode-map "o" 'ace-link-eww)))
    :config
    (progn
      (defvar spacemacs--link-pattern "~?/.+\\|\s\\[")
      (defun spacemacs//collect-spacemacs-buffer-links ()
        (let ((end (window-end))
              points)
          (save-excursion
            (goto-char (window-start))
            (while (re-search-forward spacemacs--link-pattern end t)
              (push (+ (match-beginning 0) 1) points))
            (nreverse points))))
      (defun spacemacs/ace-buffer-links ()
        "Ace jump to links in `spacemacs' buffer."
        (interactive)
        (let ((res (avy--with-avy-keys spacemacs/ace-buffer-links
                                       (avy--process
                                        (spacemacs//collect-spacemacs-buffer-links)
                                        #'avy--overlay-pre))))
          (when res
            (goto-char (1+ res))
            (widget-button-press (point))))))))


(defun spacemacs/init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "bM"  'ace-swap-window
        "wC"  'ace-delete-window
        "w <SPC>"  'ace-window)
      ;; set ace-window keys to home-row
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))))

(defun spacemacs/init-adaptive-wrap ()
  (use-package adaptive-wrap
    :config
    (progn
      (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))))

(defun spacemacs/init-aggressive-indent ()
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

(defun spacemacs/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :init
    (progn
      (setq ahs-case-fold-search nil
            ahs-default-range 'ahs-range-whole-buffer
            ;; by default disable auto-highlight of symbol
            ;; current symbol can always be highlighted with <SPC> s h
            ahs-idle-timer 0
            ahs-idle-interval 0.25
            ahs-inhibit-face-list nil)

      (spacemacs|add-toggle automatic-symbol-highlight
        :status (timerp ahs-idle-timer)
        :on (progn
              (auto-highlight-symbol-mode)
              (setq ahs-idle-timer
                    (run-with-idle-timer ahs-idle-interval t
                                         'ahs-idle-function)))
        :off (when (timerp ahs-idle-timer)
               (auto-highlight-symbol-mode)
               (cancel-timer ahs-idle-timer)
               (setq ahs-idle-timer 0))
        :documentation "Automatic highlight of current symbol."
        :evil-leader "tha")
      (spacemacs/add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                                            markdown-mode-hook)))
    :config
    (progn
      (spacemacs|hide-lighter auto-highlight-symbol-mode)
      (defvar-local spacemacs-last-ahs-highlight-p nil
        "Info on the last searched highlighted symbol.")
      (defvar-local spacemacs--ahs-searching-forward t)

      (defun spacemacs/goto-last-searched-ahs-symbol ()
        "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
        (interactive)
        (if spacemacs-last-ahs-highlight-p
            (progn (goto-char (nth 1 spacemacs-last-ahs-highlight-p))
                   (spacemacs/ahs-highlight-now-wrapper)
                   (spacemacs/symbol-highlight-micro-state))
          (message "No symbol has been searched for now.")))

      (defun spacemacs/integrate-evil-search (forward)
        ;; isearch-string is last searched item.  Next time
        ;; "n" is hit we will use this.
        (setq isearch-string
              (concat "\\<" (evil-find-thing forward 'symbol) "\\>")
              isearch-regexp
              (concat "\\<" (evil-find-thing forward 'symbol) "\\>"))
        ;; Next time "n" is hit, go the correct direction.
        (setq isearch-forward forward)
        ;; ahs does a case sensitive search.  We could set
        ;; this, but it would break the user's current
        ;; sensitivity settings.  We could save the setting,
        ;; then next time the user starts a search we could
        ;; restore the setting.
        ;;(setq case-fold-search nil)
        ;; Place the search term into the search rings.
        (isearch-update-ring isearch-string t)
        (evil-push-search-history isearch-string forward)
        ;; Use this search term for empty pattern "%s//replacement/"
        ;; Append case sensitivity
        (setq evil-ex-last-was-search nil
              evil-ex-substitute-pattern `(,(concat isearch-string "\\C")
                                           nil (0 0))))

      (defun spacemacs/ensure-ahs-enabled-locally ()
        "Ensures ahs is enabled for the local buffer."
        (unless
            (bound-and-true-p ahs-mode-line)
          (auto-highlight-symbol-mode)
          ))

      (defun spacemacs/ahs-highlight-now-wrapper ()
        "Safe wrapper for ahs-highlight-now"
        (eval '(progn
                 (spacemacs/ensure-ahs-enabled-locally)
                 (ahs-highlight-now)) nil))

      (defun spacemacs/enter-ahs-forward ()
        "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (setq spacemacs--ahs-searching-forward t)
        (spacemacs/quick-ahs-forward))

      (defun spacemacs/enter-ahs-backward ()
        "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (setq spacemacs--ahs-searching-forward nil)
        (spacemacs/quick-ahs-forward))

      (defun spacemacs/quick-ahs-forward ()
        "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (spacemacs//quick-ahs-move t))

      (defun spacemacs/quick-ahs-backward ()
        "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (spacemacs//quick-ahs-move nil))

      (defun spacemacs//quick-ahs-move (forward)
        "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"

        (if (eq forward spacemacs--ahs-searching-forward)
            (progn
              (spacemacs/integrate-evil-search t)
              (spacemacs/ahs-highlight-now-wrapper)
              (when (configuration-layer/package-usedp 'evil-jumper)
                (evil-set-jump))
              (spacemacs/symbol-highlight-micro-state)
              (ahs-forward)
              )
          (progn
            (spacemacs/integrate-evil-search nil)
            (spacemacs/ahs-highlight-now-wrapper)
            (when (configuration-layer/package-usedp 'evil-jumper)
              (evil-set-jump))
            (spacemacs/symbol-highlight-micro-state)
            (ahs-backward)
            )))

      (with-eval-after-load 'evil
        (define-key evil-motion-state-map (kbd "*")
          'spacemacs/enter-ahs-forward)
        (define-key evil-motion-state-map (kbd "#")
          'spacemacs/enter-ahs-backward))

      (defun spacemacs/symbol-highlight ()
        "Highlight the symbol under point with `auto-highlight-symbol'."
        (interactive)
        (spacemacs/ahs-highlight-now-wrapper)
        (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))
        (spacemacs/symbol-highlight-micro-state)
        (spacemacs/integrate-evil-search nil))

      (defun spacemacs//ahs-ms-on-exit ()
        ;; Restore user search direction state as ahs has exitted in a state
        ;; good for <C-s>, but not for 'n' and 'N'"
        (setq isearch-forward spacemacs--ahs-searching-forward))

      (defun spacemacs/symbol-highlight-reset-range ()
        "Reset the range for `auto-highlight-symbol'."
        (interactive)
        (ahs-change-range ahs-default-range))

      (spacemacs/set-leader-keys
        "sh" 'spacemacs/symbol-highlight
        "sH" 'spacemacs/goto-last-searched-ahs-symbol)

      ;; micro-state to easily jump from a highlighted symbol to the others
      (dolist (sym '(ahs-forward
                     ahs-forward-definition
                     ahs-backward
                     ahs-backward-definition
                     ahs-back-to-start
                     ahs-change-range))
        (let* ((advice (intern (format "spacemacs/%s" (symbol-name sym)))))
          (eval `(defadvice ,sym (around ,advice activate)
                   (spacemacs/ahs-highlight-now-wrapper)
                   ad-do-it
                   (spacemacs/ahs-highlight-now-wrapper)
                   (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))))))

      (spacemacs|define-micro-state symbol-highlight
        :doc (let* ((i 0)
                    (overlay-count (length ahs-overlay-list))
                    (overlay (format "%s" (nth i ahs-overlay-list)))
                    (current-overlay (format "%s" ahs-current-overlay))
                    (st (ahs-stat))
                    (plighter (ahs-current-plugin-prop 'lighter))
                    (plugin (format " <%s> " (cond ((string= plighter "HS") "D")
                                                   ((string= plighter "HSA") "B")
                                                   ((string= plighter "HSD") "F"))))
                    (propplugin (propertize plugin 'face
                                            `(:foreground "#ffffff"
                                                          :background ,(face-attribute
                                                                        'ahs-plugin-defalt-face :foreground)))))
               (while (not (string= overlay current-overlay))
                 (setq i (1+ i))
                 (setq overlay (format "%s" (nth i ahs-overlay-list))))
               (let* ((x/y (format "(%s/%s)" (- overlay-count i) overlay-count))
                      (propx/y (propertize x/y 'face ahs-plugin-whole-buffer-face))
                      (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" ""))
                      (prophidden (propertize hidden 'face '(:weight bold))))
                 (format "%s %s%s [n] find next [N/p] find previous [e] edit [r] range [R] reset [d/D] definition [/] find in project [f] find in files [b] find in opened buffers [q] exit"
                         propplugin propx/y prophidden)))
        :on-exit  (spacemacs//ahs-ms-on-exit)
        :bindings
        ("d" ahs-forward-definition)
        ("D" ahs-backward-definition)
        ("e" nil
         :post (if (configuration-layer/package-usedp 'evil-iedit-state)
                   (evil-iedit-state/iedit-mode)
                 (ahs-edit-mode t))
         :exit t)
        ("n" spacemacs/quick-ahs-forward)
        ("N" spacemacs/quick-ahs-backward)
        ("p" spacemacs/quick-ahs-backward)
        ("R" ahs-back-to-start)
        ("r" ahs-change-range)
        ("/" spacemacs/helm-project-smart-do-search-region-or-symbol :exit t)
        ("b" spacemacs/helm-buffers-smart-do-search-region-or-symbol :exit t)
        ("f" spacemacs/helm-files-smart-do-search-region-or-symbol :exit t)
        ("q" nil :exit t)))))

(defun spacemacs/init-avy ()
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

(defun spacemacs/init-bracketed-paste ()
  (use-package bracketed-paste
    :defer t
    :init
    ;; Enable bracketed-paste for tty
    (add-hook 'tty-setup-hook 'bracketed-paste-enable)))

(defun spacemacs/init-buffer-move ()
  (use-package buffer-move
    :defer t
    :init
    (spacemacs/set-leader-keys
      "bmh" 'buf-move-left
      "bmj" 'buf-move-down
      "bmk" 'buf-move-up
      "bml" 'buf-move-right)))

(defun spacemacs/init-centered-cursor ()
  (use-package centered-cursor-mode
    :commands (centered-cursor-mode
               global-centered-cursor-mode)
    :init
    (progn
      (spacemacs|add-toggle centered-point
        :status centered-cursor-mode
        :on (centered-cursor-mode)
        :off (centered-cursor-mode -1)
        :documentation
        "Keep point at the center of the window."
        :evil-leader "t-")
      (spacemacs|add-toggle centered-point-globally
        :status centered-cursor-mode
        :on (global-centered-cursor-mode)
        :off (global-centered-cursor-mode -1)
        :documentation
        "Keep point at the center of the window globally."
        :evil-leader "t C--"))
    :config
    (progn
      (setq ccm-recenter-at-end-of-file t
            ccm-ignored-commands '(mouse-drag-region
                                   mouse-set-point
                                   widget-button-click
                                   scroll-bar-toolkit-scroll
                                   evil-mouse-drag-region))
      (spacemacs|diminish centered-cursor-mode " ⊝" " -"))))

(defun spacemacs/init-clean-aindent-mode ()
  (use-package clean-aindent-mode
    :defer t
    :init
    (add-hook 'prog-mode-hook 'clean-aindent-mode)))

(defun spacemacs/init-desktop ()
  (use-package desktop
    :defer t
    :config
    (progn
      (setq desktop-dirname spacemacs-cache-directory)
      (push spacemacs-cache-directory desktop-path))))

(defun spacemacs/init-define-word ()
  (use-package define-word
    :defer t
    :init
    (spacemacs/set-leader-keys
      "xwd" 'define-word-at-point)))

(defun spacemacs/init-doc-view ()
  (use-package doc-view
    :defer t
    :init
    (evilified-state-evilify doc-view-mode doc-view-mode-map
      "/"  'spacemacs/doc-view-search-new-query
      "?"  'spacemacs/doc-view-search-new-query-backward
      "gg" 'doc-view-first-page
      "G"  'doc-view-last-page
      "gt" 'doc-view-goto-page
      "h"  'doc-view-previous-page
      "j"  'doc-view-next-line-or-next-page
      "k"  'doc-view-previous-line-or-previous-page
      "K"  'doc-view-kill-proc-and-buffer
      "l"  'doc-view-next-page
      "n"  'doc-view-search
      "N"  'doc-view-search-backward
      (kbd "C-d") 'doc-view-scroll-up-or-next-page
      (kbd "C-k") 'doc-view-kill-proc
      (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
    :config
    (progn
      (defun spacemacs/doc-view-search-new-query ()
        "Initiate a new query."
        (interactive)
        (doc-view-search 'newquery))

      (defun spacemacs/doc-view-search-new-query-backward ()
        "Initiate a new query."
        (interactive)
        (doc-view-search 'newquery t))

      ;; fixed a weird issue where toggling display does not
      ;; swtich to text mode
      (defadvice doc-view-toggle-display
          (around spacemacs/doc-view-toggle-display activate)
        (if (eq major-mode 'doc-view-mode)
            (progn
              ad-do-it
              (text-mode)
              (doc-view-minor-mode))
          ad-do-it)))))

(defun spacemacs/init-eval-sexp-fu ()
  ;; ignore obsolete function warning generated on startup
  (let ((byte-compile-not-obsolete-funcs (append byte-compile-not-obsolete-funcs '(preceding-sexp))))
    (require 'eval-sexp-fu)))

(defun spacemacs/init-evil-anzu ()
  (use-package evil-anzu
    :init
    (global-anzu-mode t)
    :config
    (progn
      (spacemacs|hide-lighter anzu-mode)
      (setq anzu-search-threshold 1000
            anzu-cons-mode-line-p nil)
      ;; powerline integration
      (when (configuration-layer/package-usedp 'spaceline)
        (defun spacemacs/anzu-update-mode-line (here total)
          "Custom update function which does not propertize the status."
          (when anzu--state
            (let ((status (cl-case anzu--state
                            (search (format "(%s/%d%s)"
                                            (anzu--format-here-position here total)
                                            total (if anzu--overflow-p "+" "")))
                            (replace-query (format "(%d replace)" total))
                            (replace (format "(%d/%d)" here total)))))
              status)))
        (setq anzu-mode-line-update-function 'spacemacs/anzu-update-mode-line)))))

(defun spacemacs/init-evil-args ()
  (use-package evil-args
    :init
    (progn
      ;; bind evil-args text objects
      (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
      (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))))

(defun spacemacs/init-evil-exchange ()
  (use-package evil-exchange
    :init (evil-exchange-install)))

(defun spacemacs/init-evil-iedit-state ()
  (use-package evil-iedit-state
    :commands (evil-iedit-state evil-iedit-state/iedit-mode)
    :init (spacemacs/set-leader-keys "se" 'evil-iedit-state/iedit-mode)
    :config
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)))

(defun spacemacs/init-evil-indent-plus ()
  (use-package evil-indent-plus
    :init (evil-indent-plus-default-bindings)))

(defun spacemacs/init-evil-jumper ()
  (use-package evil-jumper
    :init (evil-jumper-mode t)))

(defun spacemacs/init-evil-lisp-state ()
  (use-package evil-lisp-state
    :init (setq evil-lisp-state-global t)
    :config (spacemacs/set-leader-keys "k" evil-lisp-state-map)))

(defun spacemacs/init-evil-mc ()
  (use-package evil-mc
    :defer t))

;; other commenting functions in funcs.el with keybinds in keybindings.el
(defun spacemacs/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :commands evilnc-comment-operator
    :init
    (progn
      ;; double all the commenting functions so that the inverse operations
      ;; can be called without setting a flag
      (defun spacemacs/comment-or-uncomment-lines-inverse (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line t))
          (evilnc-comment-or-uncomment-lines arg)))

      (defun spacemacs/comment-or-uncomment-lines (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line nil))
          (evilnc-comment-or-uncomment-lines arg)))

      (defun spacemacs/copy-and-comment-lines-inverse (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line t))
          (evilnc-copy-and-comment-lines arg)))

      (defun spacemacs/copy-and-comment-lines (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line nil))
          (evilnc-copy-and-comment-lines arg)))

      (defun spacemacs/quick-comment-or-uncomment-to-the-line-inverse
          (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line t))
          (evilnc-comment-or-uncomment-to-the-line arg)))

      (defun spacemacs/quick-comment-or-uncomment-to-the-line (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line nil))
          (evilnc-comment-or-uncomment-to-the-line arg)))

      (defun spacemacs/comment-or-uncomment-paragraphs-inverse (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line t))
          (evilnc-comment-or-uncomment-paragraphs arg)))

      (defun spacemacs/comment-or-uncomment-paragraphs (&optional arg)
        (interactive "p")
        (let ((evilnc-invert-comment-line-by-line nil))
          (evilnc-comment-or-uncomment-paragraphs arg)))

      (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
      (define-key evil-normal-state-map "gy" 'spacemacs/copy-and-comment-lines)

      (spacemacs/set-leader-keys
        ";"  'evilnc-comment-operator
        "cl" 'spacemacs/comment-or-uncomment-lines
        "cL" 'spacemacs/comment-or-uncomment-lines-inverse
        "cp" 'spacemacs/comment-or-uncomment-paragraphs
        "cP" 'spacemacs/comment-or-uncomment-paragraphs-inverse
        "ct" 'spacemacs/quick-comment-or-uncomment-to-the-line
        "cT" 'spacemacs/quick-comment-or-uncomment-to-the-line-inverse
        "cy" 'spacemacs/copy-and-comment-lines
        "cY" 'spacemacs/copy-and-comment-lines-inverse))))

(defun spacemacs/init-evil-matchit ()
  (use-package evil-matchit
    :defer t))

(defun spacemacs/init-evil-numbers ()
  (use-package evil-numbers
    :config
    (progn
      (defun spacemacs/evil-numbers-micro-state-doc ()
        "Display a short documentation in the mini buffer."
        (spacemacs/echo "+/= to increase the value or - to decrease it"))

      (defun spacemacs/evil-numbers-micro-state-overlay-map ()
        "Set a temporary overlay map to easily increase or decrease a number"
        (set-temporary-overlay-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "+") 'spacemacs/evil-numbers-increase)
           (define-key map (kbd "=") 'spacemacs/evil-numbers-increase)
           (define-key map (kbd "-") 'spacemacs/evil-numbers-decrease)
           map) t)
        (spacemacs/evil-numbers-micro-state-doc))

      (defun spacemacs/evil-numbers-increase (amount &optional no-region)
        "Increase number at point."
        (interactive "p*")
        (evil-numbers/inc-at-pt amount no-region)
        (spacemacs/evil-numbers-micro-state-overlay-map))
      (defun spacemacs/evil-numbers-decrease (amount)
        "Decrease number at point."
        (interactive "p*")
        (evil-numbers/dec-at-pt amount)
        (spacemacs/evil-numbers-micro-state-overlay-map))
      (spacemacs/set-leader-keys "n+" 'spacemacs/evil-numbers-increase)
      (spacemacs/set-leader-keys "n=" 'spacemacs/evil-numbers-increase)
      (spacemacs/set-leader-keys "n-" 'spacemacs/evil-numbers-decrease))))

(defun spacemacs/init-evil-search-highlight-persist ()
  (use-package evil-search-highlight-persist
    :init
    (progn
      (global-evil-search-highlight-persist)
      ;; (set-face-attribute )
      (spacemacs/set-leader-keys "sc" 'evil-search-highlight-persist-remove-all)
      (define-key evil-search-highlight-persist-map (kbd "C-x SPC") 'rectangle-mark-mode)
      (evil-ex-define-cmd "nohlsearch"
                          'evil-search-highlight-persist-remove-all)
      (defun spacemacs/adaptive-evil-highlight-persist-face ()
        (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
                            :inherit 'region
                            :background nil
                            :foreground nil))
      (spacemacs/adaptive-evil-highlight-persist-face))))

(defun spacemacs/init-evil-terminal-cursor-changer ()
  (use-package evil-terminal-cursor-changer
    :if (not (display-graphic-p))
    :init (setq evil-visual-state-cursor 'box
                evil-insert-state-cursor 'bar
                evil-emacs-state-cursor 'hbar)))

(defun spacemacs/init-evil-tutor ()
  (use-package evil-tutor
    :commands (evil-tutor-start
               evil-tutor-resume)
    :init
    (progn
      (setq evil-tutor-working-directory
            (concat spacemacs-cache-directory ".tutor/"))
      (spacemacs/set-leader-keys "hT" 'evil-tutor-start))))

(defun spacemacs/init-evil-unimpaired ()

  (defun evil-unimpaired//find-relative-filename (offset)
    (when buffer-file-name
      (let* ((directory (f-dirname buffer-file-name))
             (files (f--files directory (not (s-matches? "^\\.?#" it))))
             (index (+ (-elem-index buffer-file-name files) offset))
             (file (and (>= index 0) (nth index files))))
        (when file
          (f-expand file directory)))))

  (defun evil-unimpaired/previous-file ()
    (interactive)
    (-if-let (filename (evil-unimpaired//find-relative-filename -1))
        (find-file filename)
      (user-error "No previous file")))

  (defun evil-unimpaired/next-file ()
    (interactive)
    (-if-let (filename (evil-unimpaired//find-relative-filename 1))
        (find-file filename)
      (user-error "No next file")))

  (defun evil-unimpaired/paste-above ()
    (interactive)
    (evil-insert-newline-above)
    (evil-paste-after 1))

  (defun evil-unimpaired/paste-below ()
    (interactive)
    (evil-insert-newline-below)
    (evil-paste-after 1))

  (defun evil-unimpaired/insert-space-above ()
    (interactive)
    (evil-insert-newline-above)
    (forward-line))

  (defun evil-unimpaired/insert-space-below ()
    (interactive)
    (evil-insert-newline-below)
    (forward-line -1))

  (defun evil-unimpaired/next-frame ()
    (interactive)
    (raise-frame (next-frame)))

  (defun evil-unimpaired/previous-frame ()
    (interactive)
    (raise-frame (previous-frame)))

  ;; from tpope's unimpaired
  (define-key evil-normal-state-map (kbd "[ SPC")
    'evil-unimpaired/insert-space-above)
  (define-key evil-normal-state-map (kbd "] SPC")
    'evil-unimpaired/insert-space-below)
  (define-key evil-normal-state-map (kbd "[ e") 'move-text-up)
  (define-key evil-normal-state-map (kbd "] e") 'move-text-down)
  (define-key evil-visual-state-map (kbd "[ e") ":move'<--1")
  (define-key evil-visual-state-map (kbd "] e") ":move'>+1")
  ;; (define-key evil-visual-state-map (kbd "[ e") 'move-text-up)
  ;; (define-key evil-visual-state-map (kbd "] e") 'move-text-down)
  (define-key evil-normal-state-map (kbd "[ b") 'spacemacs/previous-useful-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'spacemacs/next-useful-buffer)
  (define-key evil-normal-state-map (kbd "[ f") 'evil-unimpaired/previous-file)
  (define-key evil-normal-state-map (kbd "] f") 'evil-unimpaired/next-file)
  (define-key evil-normal-state-map (kbd "] l") 'spacemacs/next-error)
  (define-key evil-normal-state-map (kbd "[ l") 'spacemacs/previous-error)
  (define-key evil-normal-state-map (kbd "[ h") 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd "] h") 'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd "[ t") 'evil-unimpaired/previous-frame)
  (define-key evil-normal-state-map (kbd "] t") 'evil-unimpaired/next-frame)
  (define-key evil-normal-state-map (kbd "[ w") 'previous-multiframe-window)
  (define-key evil-normal-state-map (kbd "] w") 'next-multiframe-window)
  ;; select pasted text
  (define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))
  ;; paste above or below with newline
  (define-key evil-normal-state-map (kbd "[ p") 'evil-unimpaired/paste-above)
  (define-key evil-normal-state-map (kbd "] p") 'evil-unimpaired/paste-below))

(defun spacemacs/init-expand-region ()
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

(defun spacemacs/init-fancy-battery ()
  (use-package fancy-battery
    :defer t
    :init
    (progn
      (spacemacs|add-toggle mode-line-battery
        :status fancy-battery-mode
        :on (fancy-battery-mode)
        :off (fancy-battery-mode -1)
        :documentation "Display battery info in mode-line."
        :evil-leader "tmb")
      (setq-default fancy-battery-show-percentage t))))

(defun spacemacs/init-flx-ido ()
  (use-package flx-ido
    :init (flx-ido-mode 1)))

(defun spacemacs/init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :init
    (spacemacs|add-toggle golden-ratio
      :status golden-ratio-mode
      :on (golden-ratio-mode) (golden-ratio)
      :off (golden-ratio-mode -1) (balance-windows)
      :documentation "Resize the focused window using the golden ratio."
      :evil-leader "tg")
    :config
    (progn
      (setq golden-ratio-exclude-modes '("bs-mode"
                                         "calc-mode"
                                         "ediff-mode"
                                         "dired-mode"
                                         "gud-mode"
                                         "gdb-locals-mode"
                                         "gdb-registers-mode"
                                         "gdb-breakpoints-mode"
                                         "gdb-threads-mode"
                                         "gdb-frames-mode"
                                         "gdb-inferior-io-mode"
                                         "gud-mode"
                                         "gdb-inferior-io-mode"
                                         "gdb-disassembly-mode"
                                         "gdb-memory-mode"
                                         "restclient-mode"
                                         "speedbar-mode"
                                         ))

      (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

      (setq golden-ratio-extra-commands
            (append golden-ratio-extra-commands
                    '(ace-window
                      ace-delete-window
                      ace-select-window
                      ace-swap-window
                      ace-maximize-window
                      avy-pop-mark
                      evil-avy-goto-word-or-subword-1
                      evil-avy-goto-line
                      windmove-left
                      windmove-right
                      windmove-up
                      windmove-down
                      evil-window-delete
                      evil-window-split
                      evil-window-vsplit
                      evil-window-left
                      evil-window-right
                      evil-window-up
                      evil-window-down
                      evil-window-bottom-right
                      evil-window-top-left
                      evil-window-mru
                      evil-window-next
                      evil-window-prev
                      evil-window-new
                      evil-window-vnew
                      evil-window-rotate-upwards
                      evil-window-rotate-downwards
                      evil-window-move-very-top
                      evil-window-move-far-left
                      evil-window-move-far-right
                      evil-window-move-very-bottom
                      select-window-0
                      select-window-1
                      select-window-2
                      select-window-3
                      select-window-4
                      select-window-5
                      select-window-6
                      select-window-7
                      select-window-8
                      select-window-9
                      buf-move-left
                      buf-move-right
                      buf-move-up
                      buf-move-down
                      ess-eval-buffer-and-go
                      ess-eval-function-and-go
                      ess-eval-line-and-go)))

      ;; Disable auto-resizing for some buffers
      (defun spacemacs/no-golden-ratio-for-buffers (bufname)
        "Disable golden-ratio if BUFNAME is the name of a visible buffer."
        (and (get-buffer bufname) (get-buffer-window bufname 'visible)))
      (defun spacemacs/no-golden-ratio-guide-key ()
        "Disable golden-ratio for guide-key popwin buffer."
        (or (spacemacs/no-golden-ratio-for-buffers " *guide-key*")
            (spacemacs/no-golden-ratio-for-buffers " *popwin-dummy*")))
      (add-to-list 'golden-ratio-inhibit-functions
                   'spacemacs/no-golden-ratio-guide-key)
      (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
      (add-to-list 'golden-ratio-exclude-buffer-names "*LV*")
      (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")

      (spacemacs|diminish golden-ratio-mode " ⓖ" " g"))))

(defun spacemacs/init-google-translate ()
  (use-package google-translate
    :commands (google-translate-query-translate
               google-translate-at-point
               google-translate-query-translate-reverse
               google-translate-at-point-reverse)
    :init
    (progn
      (defun spacemacs/set-google-translate-languages (source target)
        "Set source language for google translate.
For instance pass En as source for English."
        (interactive
         "sEnter source language (ie. en): \nsEnter target language (ie. en): "
         source target)
        (message
         (format "Set google translate source language to %s and target to %s"
                 source target))
        (setq google-translate-default-source-language (downcase source))
        (setq google-translate-default-target-language (downcase target)))
      (spacemacs/set-leader-keys
        "xgQ" 'google-translate-query-translate-reverse
        "xgq" 'google-translate-query-translate
        "xgT" 'google-translate-at-point-reverse
        "xgt" 'google-translate-at-point))
    :config
    (progn
      (require 'google-translate-default-ui)
      (setq google-translate-enable-ido-completion t)
      (setq google-translate-show-phonetic t)
      (setq google-translate-default-source-language "en")
      (setq google-translate-default-target-language "fr"))))


(defun spacemacs/init-highlight-indentation ()
  (use-package highlight-indentation
    :defer t
    :init
    (progn
      (spacemacs|add-toggle highlight-indentation
        :status highlight-indentation-mode
        :on (highlight-indentation-mode)
        :off (highlight-indentation-mode -1)
        :documentation "Highlight indentation levels."
        :evil-leader "thi")
      (spacemacs|add-toggle highlight-indentation-current-column
        :status highlight-indentation-current-column-mode
        :on (highlight-indentation-current-column-mode)
        :off (highlight-indentation-current-column-mode -1)
        :documentation "Highlight indentation level at point."
        :evil-leader "thc"))
    :config
    (progn
      (spacemacs|diminish highlight-indentation-mode " ⓗi" " hi")
      (spacemacs|diminish highlight-indentation-current-column-mode " ⓗc" " hc"))))

(defun spacemacs/init-highlight-numbers ()
  (use-package highlight-numbers
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-numbers-mode)
      (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1))))))

(defun spacemacs/init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :init
    (progn
      (when (member dotspacemacs-highlight-delimiters '(all current))
        (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
      (setq hl-paren-delay 0.2)
      (spacemacs/set-leader-keys "tCp" 'highlight-parentheses-mode)
      (setq hl-paren-colors '("Springgreen3"
                              "IndianRed1"
                              "IndianRed3"
                              "IndianRed4")))
    :config
    (spacemacs|hide-lighter highlight-parentheses-mode)
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)))

(defun spacemacs/init-hl-anything ()
  (use-package hl-anything
    :init
    (progn
      (hl-highlight-mode)
      (setq-default hl-highlight-save-file
                    (concat spacemacs-cache-directory ".hl-save"))
      (spacemacs/set-leader-keys
        "hc"  'hl-unhighlight-all-local
        "hC"  'hl-unhighlight-all-global
        "hh"  'hl-highlight-thingatpt-local
        "hH"  'hl-highlight-thingatpt-global
        "hn"  'hl-find-next-thing
        "hN"  'hl-find-prev-thing
        "hr"  'hl-restore-highlights
        "hs"  'hl-save-highlights))
    :config (spacemacs|hide-lighter hl-highlight-mode)))

(defun spacemacs/init-hungry-delete ()
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

(defun spacemacs/init-iedit ()
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

(defun spacemacs/init-indent-guide ()
  (use-package indent-guide
    :defer t
    :init
    (progn
      (setq indent-guide-delay 0.3)
      (spacemacs|add-toggle indent-guide
        :status indent-guide-mode
        :on (indent-guide-mode)
        :off (indent-guide-mode -1)
        :documentation
        "Highlight indentation level at point. (alternative to highlight-indentation)."
        :evil-leader "ti")
      (spacemacs|add-toggle indent-guide-globally
        :status indent-guide-mode
        :on (indent-guide-global-mode)
        :off (indent-guide-global-mode -1)
        :documentation
        "Highlight indentation level at point globally. (alternative to highlight-indentation)."
        :evil-leader "t TAB"))
    :config
    (spacemacs|diminish indent-guide-mode " ⓘ" " i")))

(defun spacemacs/init-open-junk-file ()
  (use-package open-junk-file
    :defer t
    :commands (open-junk-file)
    :init
    (setq open-junk-file-format (concat spacemacs-cache-directory "junk/%Y/%m/%d-%H%M%S."))
    (defun spacemacs/open-junk-file (&optional arg)
      "Open junk file Open junk file using helm or ivy depending
on whether the spacemacs-ivy layer is used or not, with
`prefix-arg' search in junk files"
      (interactive "P")
      (let* ((fname (format-time-string open-junk-file-format (current-time)))
             (rel-fname (file-name-nondirectory fname))
             (junk-dir (file-name-directory fname))
             (default-directory junk-dir))
        (cond ((and arg (configuration-layer/layer-usedp 'spacemacs-ivy))
               (spacemacs/counsel-search dotspacemacs-search-tools nil junk-dir))
              ((configuration-layer/layer-usedp 'spacemacs-ivy)
               (require 'counsel)
               (counsel-find-file rel-fname))
              (arg
               (require 'helm)
               (let (helm-ff-newfile-prompt-p)
                 (spacemacs/helm-files-smart-do-search)))
              (t
               (require 'helm)
               (let (helm-ff-newfile-prompt-p)
                 (helm-find-files-1 fname))))))
    (spacemacs/set-leader-keys "fJ" 'spacemacs/open-junk-file)))

(defun spacemacs/init-info+ ()
  (use-package info+
    :defer t
    :init
    (progn
      (with-eval-after-load 'info
        (require 'info+))
      (setq Info-fontify-angle-bracketed-flag nil))))

(defun spacemacs/init-leuven-theme ()
  (use-package leuven-theme
    :defer t
    :init (setq org-fontify-whole-heading-line t)))

(defun spacemacs/init-linum-relative ()
  (use-package linum-relative
    :commands (linum-relative-toggle linum-relative-on)
    :init
    (progn
      (when (eq dotspacemacs-line-numbers 'relative)
        (linum-relative-on))
      (spacemacs/set-leader-keys "tr" 'linum-relative-toggle))
    :config
    (progn
      (setq linum-relative-current-symbol ""))))

(defun spacemacs/init-lorem-ipsum ()
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

(defun spacemacs/init-move-text ()
  (use-package move-text
    :defer t
    :init
    (spacemacs|define-micro-state move-text
      :doc "[J] move down [K] move up"
      :use-minibuffer t
      :execute-binding-on-enter t
      :evil-leader "xJ" "xK"
      :bindings
      ("J" move-text-down)
      ("K" move-text-up))))

(defun spacemacs/init-neotree ()
  (use-package neotree
    :defer t
    :commands neo-global--window-exists-p
    :init
    (progn
      (setq neo-window-width 32
            neo-create-file-auto-open t
            neo-banner-message nil
            neo-show-updir-line nil
            neo-mode-line-type 'neotree
            neo-smart-open t
            neo-dont-be-alone t
            neo-persist-show nil
            neo-show-hidden-files t
            neo-auto-indent-point t
            neo-modern-sidebar t
            neo-vc-integration nil)

      (defun spacemacs/neotree-expand-or-open ()
        "Collapse a neotree node."
        (interactive)
        (let ((node (neo-buffer--get-filename-current-line)))
          (when node
            (if (file-directory-p node)
                (progn
                  (neo-buffer--set-expand node t)
                  (neo-buffer--refresh t)
                  (when neo-auto-indent-point
                    (next-line)
                    (neo-point-auto-indent)))
              (call-interactively 'neotree-enter)))))

      (defun spacemacs/neotree-collapse ()
        "Collapse a neotree node."
        (interactive)
        (let ((node (neo-buffer--get-filename-current-line)))
          (when node
            (when (file-directory-p node)
              (neo-buffer--set-expand node nil)
              (neo-buffer--refresh t))
            (when neo-auto-indent-point
              (neo-point-auto-indent)))))

      (defun spacemacs/neotree-collapse-or-up ()
        "Collapse an expanded directory node or go to the parent node."
        (interactive)
        (let ((node (neo-buffer--get-filename-current-line)))
          (when node
            (if (file-directory-p node)
                (if (neo-buffer--expanded-node-p node)
                    (spacemacs/neotree-collapse)
                  (neotree-select-up-node))
              (neotree-select-up-node)))))

      (defun neotree-find-project-root ()
        (interactive)
        (if (neo-global--window-exists-p)
            (neotree-hide)
          (let ((origin-buffer-file-name (buffer-file-name)))
            (neotree-find (projectile-project-root))
            (neotree-find origin-buffer-file-name))))

      (defun spacemacs//neotree-key-bindings ()
        "Set the key bindings for a neotree buffer."
        (evilified-state-evilify-map neotree-mode-map
          :mode neotree-mode
          :bindings
          (kbd "TAB")  'neotree-stretch-toggle
          (kbd "RET") 'neotree-enter
          (kbd "|") 'neotree-enter-vertical-split
          (kbd "-") 'neotree-enter-horizontal-split
          (kbd "?") 'evil-search-backward
          (kbd "c") 'neotree-create-node
          (kbd "d") 'neotree-delete-node
          (kbd "gr") 'neotree-refresh
          (kbd "h") 'spacemacs/neotree-collapse-or-up
          (kbd "H") 'neotree-select-previous-sibling-node
          (kbd "J") 'neotree-select-down-node
          (kbd "K") 'neotree-select-up-node
          (kbd "l") 'spacemacs/neotree-expand-or-open
          (kbd "L") 'neotree-select-next-sibling-node
          (kbd "q") 'neotree-hide
          (kbd "r") 'neotree-rename-node
          (kbd "R") 'neotree-change-root
          (kbd "s") 'neotree-hidden-file-toggle))

      (spacemacs/set-leader-keys
        "ft" 'neotree-toggle
        "pt" 'neotree-find-project-root))

    :config
    (spacemacs//neotree-key-bindings)))

(defun spacemacs/init-pcre2el ()
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

(defun spacemacs/init-paradox ()
  (use-package paradox
    :commands paradox-list-packages
    :init
    (progn
      (setq paradox-execute-asynchronously nil)
      (defun spacemacs/paradox-list-packages ()
        "Load depdendencies for auth and open the package list."
        (interactive)
        (require 'epa-file)
        (require 'auth-source)
        (when (and (not (boundp 'paradox-github-token))
                   (file-exists-p "~/.authinfo.gpg"))
          (let ((authinfo-result (car (auth-source-search
                                       :max 1
                                       :host "github.com"
                                       :port "paradox"
                                       :user "paradox"
                                       :require '(:secret)))))
            (let ((paradox-token (plist-get authinfo-result :secret)))
              (setq paradox-github-token (if (functionp paradox-token)
                                             (funcall paradox-token)
                                           paradox-token)))))
        (paradox-list-packages nil))

      (evilified-state-evilify paradox-menu-mode paradox-menu-mode-map
        "H" 'paradox-menu-quick-help
        "J" 'paradox-next-describe
        "K" 'paradox-previous-describe
        "L" 'paradox-menu-view-commit-list
        "o" 'paradox-menu-visit-homepage)
      (spacemacs/set-leader-keys
        "ak" 'spacemacs/paradox-list-packages))))

(defun spacemacs/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "tCd" 'rainbow-delimiters-mode)
      (when (member dotspacemacs-highlight-delimiters '(any all))
        (spacemacs/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook))))))

(defun spacemacs/init-smartparens ()
  (use-package smartparens
    :defer t
    :commands (sp-split-sexp sp-newline)
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
               '(:add (spacemacs/smartparens-pair-newline-and-indent "RET"))))))

(defun spacemacs/init-smooth-scrolling ()
  (defun spacemacs//unset-scroll-margin ()
    "Set scroll-margin to zero."
    (setq-local scroll-margin 0))

  (use-package smooth-scrolling
    :if dotspacemacs-smooth-scrolling
    :init (setq smooth-scroll-margin 5
                scroll-conservatively 101
                scroll-preserve-screen-position t
                auto-window-vscroll nil)
    :config
    (progn
      (setq scroll-margin 5)
      ;; add hooks here only for emacs built-in packages that are not owned
      ;; by a layer.
      (spacemacs/add-to-hooks 'spacemacs//unset-scroll-margin
                              '(messages-buffer-mode-hook))))

  (unless dotspacemacs-smooth-scrolling
    ;; deactivate smooth-scrolling advices
    (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
    (ad-activate 'previous-line)
    (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
    (ad-activate 'next-line)
    (ad-disable-advice 'isearch-repeat 'after 'isearch-smooth-scroll)
    (ad-activate 'isearch-repeat)))

(defun spacemacs/init-spaceline ()
  (use-package spaceline-config
    :init
    (progn
      (spacemacs|do-after-display-system-init
       (setq-default powerline-default-separator
                     (if (display-graphic-p) 'wave 'utf-8)))
      (defun spacemacs//set-powerline-for-startup-buffers ()
        "Set the powerline for buffers created when Emacs starts."
        (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
          (when (and (get-buffer buffer)
                     (configuration-layer/package-usedp 'spaceline))
            (spacemacs//restore-powerline buffer))))
      (add-hook 'emacs-startup-hook
                'spacemacs//set-powerline-for-startup-buffers))
    :config
    (progn
      (defun spacemacs/customize-powerline-faces ()
        "Alter powerline face to make them work with more themes."
        (set-face-attribute 'powerline-inactive2 nil
                            :inherit 'font-lock-comment-face))
      (spacemacs/customize-powerline-faces)

      (dolist (spec '((minor-modes "tmm")
                      (major-mode "tmM")
                      (version-control "tmv")
                      (new-version "tmV")
                      (point-position "tmp")
                      (org-clock "tmc")))
        (let* ((segment (car spec))
               (status-var (intern (format "spaceline-%S-p" segment))))
          (eval `(spacemacs|add-toggle ,(intern (format "mode-line-%S" segment))
                   :status ,status-var
                   :on (setq ,status-var t)
                   :off (setq ,status-var nil)
                   :documentation ,(format "Show %s in the mode-line."
                                           (replace-regexp-in-string
                                            "-" " " (format "%S" segment)))
                   :evil-leader ,(cadr spec)))))
      (setq spaceline-org-clock-p nil)

      (defun spacemacs//evil-state-face ()
        (if (bound-and-true-p evil-state)
            (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
              (intern (format "spacemacs-%S-face" state)))
          'face-of-god))
      (setq spaceline-highlight-face-func 'spacemacs//evil-state-face)

      (let ((unicodep (dotspacemacs|symbol-value
                       dotspacemacs-mode-line-unicode-symbols)))
        (setq spaceline-window-numbers-unicode unicodep)
        (setq spaceline-workspace-numbers-unicode unicodep))

      (defpowerline spacemacs-powerline-new-version
        (propertize
         spacemacs-version-check-lighter
         'mouse-face 'mode-line-highlight
         'help-echo (format "New version %s | Click with mouse-1 to update"
                            spacemacs-new-version)
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map
                        [mode-line down-mouse-1]
                        (lambda (event)
                          (interactive "@e")
                          (if (yes-or-no-p
                               (format (concat "Do you want to update to the newest "
                                               "version %s ?") spacemacs-new-version))
                              (progn
                                (spacemacs/switch-to-version spacemacs-new-version))
                            (message "Update aborted."))))
                      map)))

      (spaceline-define-segment new-version
        (spacemacs-powerline-new-version
         (spacemacs/get-new-version-lighter-face
          spacemacs-version spacemacs-new-version))
        :when spacemacs-new-version)

      (spaceline-spacemacs-theme '(new-version :when active))
      (spaceline-helm-mode t)
      (when (configuration-layer/package-usedp 'info+)
        (spaceline-info-mode t))

      (defun spacemacs//restore-powerline (buffer)
        "Restore the powerline in buffer"
        (with-current-buffer buffer
          (setq-local mode-line-format (default-value 'mode-line-format))
          (powerline-set-selected-window)
          (powerline-reset)))

      (defun spacemacs//prepare-diminish ()
        (when spaceline-minor-modes-p
          (let ((unicodep (dotspacemacs|symbol-value
                           dotspacemacs-mode-line-unicode-symbols)))
            (setq spaceline-minor-modes-separator
                  (if unicodep (if (display-graphic-p) "" " ") "|"))
            (dolist (mm spacemacs--diminished-minor-modes)
              (let ((mode (car mm)))
                (when (and (boundp mode) (symbol-value mode))
                  (let* ((unicode (cadr mm))
                         (ascii (caddr mm))
                         (dim (if unicodep
                                  unicode
                                (if ascii ascii unicode))))
                    (diminish mode dim))))))))
      (add-hook 'spaceline-pre-hook 'spacemacs//prepare-diminish))))

(defun spacemacs/init-vi-tilde-fringe ()
  (use-package vi-tilde-fringe
    :if window-system
    :init
    (progn
      (global-vi-tilde-fringe-mode)
      (spacemacs|add-toggle vi-tilde-fringe
        :status vi-tilde-fringe-mode
        :on (global-vi-tilde-fringe-mode)
        :off (global-vi-tilde-fringe-mode -1)
        :documentation
        "Globally display a ~ on empty lines in the fringe."
        :evil-leader "T~")
      ;; don't enable it on spacemacs home buffer
      (with-current-buffer  "*spacemacs*"
        (vi-tilde-fringe-mode -1))
      ;; after a major mode is loaded, check if the buffer is read only
      ;; if so, disable vi-tilde-fringe-mode
      (add-hook 'after-change-major-mode-hook (lambda ()
                                                (when buffer-read-only
                                                  (vi-tilde-fringe-mode -1)))))
    :config
    (spacemacs|hide-lighter vi-tilde-fringe-mode)))

(defun spacemacs/init-window-numbering ()
  (use-package window-numbering
    :config
    (progn
      (when (configuration-layer/package-usedp 'spaceline)
        (defun window-numbering-install-mode-line (&optional position)
          "Do nothing, the display is handled by the powerline."))
      (setq window-numbering-auto-assign-0-to-minibuffer nil)
      (spacemacs/set-leader-keys
        "0" 'select-window-0
        "1" 'select-window-1
        "2" 'select-window-2
        "3" 'select-window-3
        "4" 'select-window-4
        "5" 'select-window-5
        "6" 'select-window-6
        "7" 'select-window-7
        "8" 'select-window-8
        "9" 'select-window-9)
      (window-numbering-mode 1))

    (defun spacemacs//window-numbering-assign (windows)
      "Custom number assignment for special buffers."
      (mapc (lambda (w)
              (when (and (boundp 'neo-global--window)
                         (eq w neo-global--window))
                (window-numbering-assign w 0)))
            windows))
    (add-hook 'window-numbering-before-hook 'spacemacs//window-numbering-assign)
    (add-hook 'neo-after-create-hook '(lambda (w) (window-numbering-update)))))

(defun spacemacs/init-volatile-highlights ()
  (use-package volatile-highlights
    :config
    (progn
      (volatile-highlights-mode t)
      (spacemacs|hide-lighter volatile-highlights-mode))))

(defun spacemacs/init-zoom-frm ()
  (use-package zoom-frm
    :commands (zoom-frm-unzoom
               zoom-frm-out
               zoom-frm-in)
    :init
    (progn
      (spacemacs|define-micro-state zoom-frm
        :doc "[+/=] zoom frame in [-] zoom frame out [0] reset zoom [q]uit"
        :evil-leader "zf"
        :use-minibuffer t
        :bindings
        ("+" spacemacs/zoom-frm-in :post (spacemacs//zoom-frm-powerline-reset))
        ("=" spacemacs/zoom-frm-in :post (spacemacs//zoom-frm-powerline-reset))
        ("-" spacemacs/zoom-frm-out :post (spacemacs//zoom-frm-powerline-reset))
        ("0" spacemacs/zoom-frm-unzoom :post (spacemacs//zoom-frm-powerline-reset))
        ("q" nil :exit t))

      (defun spacemacs//zoom-frm-powerline-reset ()
        (when (fboundp 'powerline-reset)
          (setq-default powerline-height (spacemacs/compute-powerline-height))
          (powerline-reset)))

      (defun spacemacs//zoom-frm-do (arg)
        "Perform a zoom action depending on ARG value."
        (let ((zoom-action (cond ((eq arg 0) 'zoom-frm-unzoom)
                                 ((< arg 0) 'zoom-frm-out)
                                 ((> arg 0) 'zoom-frm-in)))
              (fm (cdr (assoc 'fullscreen (frame-parameters))))
              (fwp (* (frame-char-width) (frame-width)))
              (fhp (* (frame-char-height) (frame-height))))
          (when (equal fm 'maximized)
            (toggle-frame-maximized))
          (funcall zoom-action)
          (set-frame-size nil fwp fhp t)
          (when (equal fm 'maximized)
            (toggle-frame-maximized))))

      (defun spacemacs/zoom-frm-in ()
        "zoom in frame, but keep the same pixel size"
        (interactive)
        (spacemacs//zoom-frm-do 1))

      (defun spacemacs/zoom-frm-out ()
        "zoom out frame, but keep the same pixel size"
        (interactive)
        (spacemacs//zoom-frm-do -1))

      (defun spacemacs/zoom-frm-unzoom ()
        "Unzoom current frame, keeping the same pixel size"
        (interactive)
        (spacemacs//zoom-frm-do 0))

      ;; Font size, either with ctrl + mouse wheel
      (global-set-key (kbd "<C-wheel-up>") 'spacemacs/zoom-frm-in)
      (global-set-key (kbd "<C-wheel-down>") 'spacemacs/zoom-frm-out))))
