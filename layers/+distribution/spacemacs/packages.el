;;; packages.el --- Spacemacs Layer packages File
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

(setq spacemacs-packages
      '(
        ;; default
        ace-link
        ace-window
        adaptive-wrap
        aggressive-indent
        auto-dictionary
        auto-highlight-symbol
        avy
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
        (evil-indent-textobject :location (recipe :fetcher github :repo "TheBB/evil-indent-textobject"))
        evil-jumper
        evil-lisp-state
        evil-nerd-commenter
        evil-matchit
        evil-numbers
        evil-search-highlight-persist
        ;; Temporarily disabled, pending the resolution of
        ;; https://github.com/7696122/evil-terminal-cursor-changer/issues/8
        (evil-terminal-cursor-changer :excluded t)
        evil-tutor
        expand-region
        fancy-battery
        flx-ido
        golden-ratio
        google-translate
        helm-ag
        helm-make
        helm-mode-manager
        helm-swoop
        helm-themes
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
        move-text
        neotree
        pcre2el
        powerline
        rainbow-delimiters
        recentf
        smartparens
        smooth-scrolling
        (solarized-theme :location local)
        spray
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
      (define-key spacemacs-mode-map "o" 'spacemacs/ace-buffer-links)
      (eval-after-load "info"
        '(define-key Info-mode-map "o" 'ace-link-info))
      (eval-after-load "help-mode"
        '(define-key help-mode-map "o" 'ace-link-help))
      (eval-after-load "eww"
        '(progn
           (define-key eww-link-keymap "o" 'ace-link-eww)
           (define-key eww-mode-map "o" 'ace-link-eww))))
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
      (evil-leader/set-key
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

(defun spacemacs/init-auto-dictionary ()
  (use-package auto-dictionary
    :disabled t
    :defer t
    :init
    (progn
      (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1)))
      (evil-leader/set-key
        "Sd" 'adict-change-dictionary))))

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

      (eval-after-load 'evil
        '(progn
           (define-key evil-motion-state-map (kbd "*")
             'spacemacs/enter-ahs-forward)
           (define-key evil-motion-state-map (kbd "#")
             'spacemacs/enter-ahs-backward)))

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

      (evil-leader/set-key
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
                 (format "%s %s%s [n/N] move [e] edit [r] range [R] reset [d/D] definition [/] find in project [f] find in files [b] find in opened buffers [q] exit"
                         propplugin propx/y prophidden)))
        :on-exit  (spacemacs//ahs-ms-on-exit)
        :bindings
        ("d" ahs-forward-definition)
        ("D" ahs-backward-definition)
        ("e" nil
         :post (if (configuration-layer/package-usedp 'evil-iedit-state)
                   (evil-iedit-state/iedit-mode)
                 (ahs-edit-mode))
         :exit t)
        ("n" spacemacs/quick-ahs-forward)
        ("N" spacemacs/quick-ahs-backward)
        ("R" ahs-back-to-start)
        ("r" ahs-change-range)
        ("/" spacemacs/helm-project-smart-do-search-region-or-symbol :exit t)
        ("b" spacemacs/helm-buffers-smart-do-search-region-or-symbol :exit t)
        ("f" spacemacs/helm-files-smart-do-search-region-or-symbol :exit t)
        ("q" nil :exit t)))))

(defun spacemacs/init-avy ()
  (use-package avy
    :defer t
    :init
    (progn
      (setq avy-keys (number-sequence ?a ?z))
      (setq avy-all-windows 'all-frames)
      (setq avy-background t)
      (evil-leader/set-key
        "SPC" 'avy-goto-word-or-subword-1
        "l" 'avy-goto-line))
    :config
    (evil-leader/set-key "`" 'avy-pop-mark)))

(defun spacemacs/init-buffer-move ()
  (use-package buffer-move
    :defer t
    :init
    (evil-leader/set-key
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
    (evil-leader/set-key
      "xwd" 'define-word-at-point)))

(defun spacemacs/init-dired+ ()
  (use-package dired+
    :defer t))

(defun spacemacs/init-doc-view ()
  (use-package doc-view
    :defer t
    :init
    (evilify doc-view-mode doc-view-mode-map
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
      (when (configuration-layer/package-usedp 'powerline)
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
    :init (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
    :config
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map
      (kbd evil-leader/leader) evil-leader--default-map)))

(defun spacemacs/init-evil-indent-textobject ()
  (use-package evil-indent-textobject))

(defun spacemacs/init-evil-jumper ()
  (use-package evil-jumper
    :init
    (progn
      (setq evil-jumper-file (concat spacemacs-cache-directory "evil-jumps")
            evil-jumper-auto-save-interval 600)
      (evil-jumper-mode t))))

(defun spacemacs/init-evil-lisp-state ()
  (use-package evil-lisp-state
    :init
    (progn
      (setq evil-lisp-state-global t)
      (setq evil-lisp-state-leader-prefix "k"))))

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

      (evil-leader/set-key
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
      (evil-leader/set-key "n+" 'spacemacs/evil-numbers-increase)
      (evil-leader/set-key "n=" 'spacemacs/evil-numbers-increase)
      (evil-leader/set-key "n-" 'spacemacs/evil-numbers-decrease))))

(defun spacemacs/init-evil-search-highlight-persist ()
  (use-package evil-search-highlight-persist
    :init
    (progn
      (global-evil-search-highlight-persist)
      ;; (set-face-attribute )
      (evil-leader/set-key "sc" 'evil-search-highlight-persist-remove-all)
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
      (evil-leader/set-key "hT" 'evil-tutor-start))))

(defun spacemacs/init-expand-region ()
  (use-package expand-region
    :defer t
    :init (evil-leader/set-key "v" 'er/expand-region)
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

      (push 'fancy-battery-mode-line spacemacs--global-mode-line-excludes)

      (defun spacemacs/mode-line-battery-percentage ()
        "Return the load percentage or an empty string."
        (let ((p (cdr (assq ?p fancy-battery-last-status))))
          (if (and fancy-battery-show-percentage
                   p (not (string= "N/A" p))) (concat " " p "%%") "")))

      (defun spacemacs/mode-line-battery-time ()
        "Return the remaining time complete load or discharge."
        (let ((time (cdr (assq ?t fancy-battery-last-status))))
          (cond
           ((string= "0:00" time) "")
           ((string= "N/A" time) "")
           ((string-empty-p time) "")
           (t (concat " (" time ")")))))

      (setq-default fancy-battery-show-percentage t))
    :config
    (progn
      ;; redefine this function for Spacemacs,
      ;; basically remove all faces and properties.
      (defun fancy-battery-default-mode-line ()
        "Assemble a mode line string for Fancy Battery Mode."
        (when fancy-battery-last-status
          (let* ((type (cdr (assq ?L fancy-battery-last-status)))
                 (percentage (spacemacs/mode-line-battery-percentage))
                 (time (spacemacs/mode-line-battery-time)))
            (cond
             ((string= "on-line" type) " No Battery")
             ((string-empty-p type) " No Battery")
             (t (concat (if (string= "AC" type) " AC" "") percentage time))))))

      (defun fancy-battery-powerline-face ()
        "Return a face appropriate for powerline"
        (let ((type (cdr (assq ?L fancy-battery-last-status))))
          (if (and type (string= "AC" type))
              'fancy-battery-charging
            (pcase (cdr (assq ?b fancy-battery-last-status))
              ("!"  'fancy-battery-critical)
              ("+"  'fancy-battery-charging)
              ("-"  'fancy-battery-discharging)
              (_ 'fancy-battery-discharging))))))))

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
For instance pass En as source for english."
        (interactive
         "sEnter source language (ie. En): \nsEnter target language (ie. En): "
         source target)
        (message
         (format "Set google translate source language to %s and target to %s"
                 source target))
        (setq google-translate-default-source-language source)
        (setq google-translate-default-target-language target))
      (evil-leader/set-key
        "xgQ" 'google-translate-query-translate-reverse
        "xgq" 'google-translate-query-translate
        "xgT" 'google-translate-at-point-reverse
        "xgt" 'google-translate-at-point))
    :config
    (progn
      (require 'google-translate-default-ui)
      (setq google-translate-enable-ido-completion t)
      (setq google-translate-show-phonetic t)
      (setq google-translate-default-source-language "En")
      (setq google-translate-default-target-language "Fr"))))

(defun spacemacs/init-helm-ag ()
  (use-package helm-ag
    :defer t
    :init
    (progn
      (defun spacemacs//helm-do-ag-region-or-symbol (func &optional dir)
        "Search with `ag' with a default input."
        (require 'helm-ag)
        (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
                   ;; make thing-at-point choosing the active region first
                   ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
                   ((symbol-function 'thing-at-point)
                    (lambda (thing)
                      (let ((res (if (region-active-p)
                          (buffer-substring-no-properties
                           (region-beginning) (region-end))
                          (this-fn thing))))
                        (when res (rxt-quote-pcre res))))))
          (funcall func dir)))

      (defun spacemacs//helm-do-search-find-tool (base tools default-inputp)
        "Create a cond form given a TOOLS string list and evaluate it."
        (eval
         `(cond
           ,@(mapcar
              (lambda (x)
                `((executable-find ,x)
                  ',(let ((func
                           (intern
                            (format (if default-inputp
                                        "spacemacs/%s-%s-region-or-symbol"
                                      "spacemacs/%s-%s")
                                    base x))))
                      (if (fboundp func)
                          func
                        (intern (format "%s-%s"  base x))))))
                     tools)
           (t 'helm-do-grep))))

      ;; Search in current file ----------------------------------------------

      (defun spacemacs/helm-file-do-ag (&optional _)
        "Wrapper to execute `helm-ag-this-file.'"
        (interactive)
        (helm-ag-this-file))

      (defun spacemacs/helm-file-do-ag-region-or-symbol ()
        "Search in current file with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-file-do-ag))

      (defun spacemacs/helm-file-smart-do-search (&optional default-inputp)
        "Search in current file using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-file-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-file-smart-do-search-region-or-symbol ()
        "Search in current file using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-file-smart-do-search t))

      ;; Search in files -----------------------------------------------------

      (defun spacemacs/helm-files-do-ag (&optional dir)
        "Search in files with `ag' using a default input."
        (interactive)
        (helm-do-ag dir))

      (defun spacemacs/helm-files-do-ag-region-or-symbol ()
        "Search in files with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ag))

      (defun spacemacs/helm-files-do-ack (&optional dir)
        "Search in files with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-ack-region-or-symbol ()
        "Search in files with `ack' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ack))

      (defun spacemacs/helm-files-do-pt (&optional dir)
        "Search in files with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-pt-region-or-symbol ()
        "Search in files with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-pt))

      (defun spacemacs/helm-files-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-files-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-files-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools'.
with default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-files-smart-do-search t))

      ;; Search in buffers ---------------------------------------------------

      (defun spacemacs/helm-buffers-do-ag (&optional _)
        "Wrapper to execute `helm-ag-buffers.'"
        (interactive)
        (helm-do-ag-buffers))

      (defun spacemacs/helm-buffers-do-ag-region-or-symbol ()
        "Search in opened buffers with `ag' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ag))

      (defun spacemacs/helm-buffers-do-ack (&optional _)
        "Search in opened buffers with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-ack-region-or-symbol ()
        "Search in opened buffers with `ack' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ack))

      (defun spacemacs/helm-buffers-do-pt (&optional _)
        "Search in opened buffers with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-pt-region-or-symbol ()
        "Search in opened buffers with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-pt))

      (defun spacemacs/helm-buffers-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-buffers-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-buffers-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools' with
default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-buffers-smart-do-search t))

      ;; Search in project ---------------------------------------------------

      (defun spacemacs/helm-project-do-ag ()
        "Search in current project with `ag'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ag-region-or-symbol ()
        "Search in current project with `ag' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol 'helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack ()
        "Search in current project with `ack'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack-region-or-symbol ()
        "Search in current project with `ack' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt ()
        "Search in current project with `pt'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt-region-or-symbol ()
        "Search in current project with `pt' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-smart-do-search (&optional default-inputp)
        "Search in current project using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (let ((projectile-require-project-root nil))
         (call-interactively
          (spacemacs//helm-do-search-find-tool "helm-project-do"
                                               dotspacemacs-search-tools
                                               default-inputp))))

      (defun spacemacs/helm-project-smart-do-search-region-or-symbol ()
        "Search in current project using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-project-smart-do-search t))

      ;; evilify the helm-grep buffer
      (evilify helm-grep-mode helm-grep-mode-map
               (kbd "RET") 'helm-grep-mode-jump-other-window
               (kbd "q") 'quit-window)

      (evil-leader/set-key
        ;; opened buffers scope
        "sb"  'spacemacs/helm-buffers-smart-do-search
        "sB"  'spacemacs/helm-buffers-smart-do-search-region-or-symbol
        "sab" 'helm-do-ag-buffers
        "saB" 'spacemacs/helm-buffers-do-ag-region-or-symbol
        "skb" 'spacemacs/helm-buffers-do-ack
        "skB" 'spacemacs/helm-buffers-do-ack-region-or-symbol
        "stb" 'spacemacs/helm-buffers-do-pt
        "stB" 'spacemacs/helm-buffers-do-pt-region-or-symbol
        ;; current file scope
        "ss"  'spacemacs/helm-file-smart-do-search
        "sS"  'spacemacs/helm-file-smart-do-search-region-or-symbol
        "saa" 'helm-ag-this-file
        "saA" 'spacemacs/helm-file-do-ag-region-or-symbol
        ;; files scope
        "sf"  'spacemacs/helm-files-smart-do-search
        "sF"  'spacemacs/helm-files-smart-do-search-region-or-symbol
        "saf" 'helm-do-ag
        "saF" 'spacemacs/helm-files-do-ag-region-or-symbol
        "skf" 'spacemacs/helm-files-do-ack
        "skF" 'spacemacs/helm-files-do-ack-region-or-symbol
        "stf" 'spacemacs/helm-files-do-pt
        "stF" 'spacemacs/helm-files-do-pt-region-or-symbol
        ;; current project scope
        "/"   'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sp"  'spacemacs/helm-project-smart-do-search
        "sP"  'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sap" 'spacemacs/helm-project-do-ag
        "saP" 'spacemacs/helm-project-do-ag-region-or-symbol
        "skp" 'spacemacs/helm-project-do-ack
        "skP" 'spacemacs/helm-project-do-ack-region-or-symbol
        "stp" 'spacemacs/helm-project-do-pt
        "stP" 'spacemacs/helm-project-do-pt-region-or-symbol))
    :config
    (progn
      (evil-define-key 'normal helm-ag-map "SPC" evil-leader--default-map)
      (evilify helm-ag-mode helm-ag-mode-map
               (kbd "RET") 'helm-ag-mode-jump-other-window
               (kbd "q") 'quit-window))))

(defun spacemacs/init-helm-make ()
  (use-package helm-make
    :defer t
    :init
    (evil-leader/set-key
      "hk" 'helm-make
      "cc" 'helm-make-projectile)))

(defun spacemacs/init-helm-mode-manager ()
  (use-package helm-mode-manager
    :defer t
    :init
    (evil-leader/set-key
      "hM"    'helm-switch-major-mode
      ;; "hm"    'helm-disable-minor-mode
      "h C-m" 'helm-enable-minor-mode)))

(defun spacemacs/init-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :init
    (progn
      (setq helm-swoop-split-with-multiple-windows t
            helm-swoop-split-direction 'split-window-vertically
            helm-swoop-speed-or-color t
            helm-swoop-split-window-function 'helm-default-display-buffer
            helm-swoop-pre-input-function (lambda () ""))

      (defun spacemacs/helm-swoop-region-or-symbol ()
        "Call `helm-swoop' with default input."
        (interactive)
        (let ((helm-swoop-pre-input-function
               (lambda ()
                 (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (let ((thing (thing-at-point 'symbol t)))
                     (if thing thing ""))))))
          (call-interactively 'helm-swoop)))

      (evil-leader/set-key
        "ss"    'helm-swoop
        "sS"    'spacemacs/helm-swoop-region-or-symbol
        "s C-s" 'helm-multi-swoop-all)
      (defadvice helm-swoop (before add-evil-jump activate)
        (when (configuration-layer/package-usedp 'evil-jumper)
          (evil-set-jump))))))

(defun spacemacs/init-helm-themes ()
  (use-package helm-themes
    :defer t
    :init
    (evil-leader/set-key
      "Th" 'helm-themes)))

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
      (evil-leader/set-key "tCp" 'highlight-parentheses-mode)
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
      (evil-leader/set-key
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
        :evil-leader "t C-i"))
    :config
    (spacemacs|diminish indent-guide-mode " ⓘ" " i")))

(defun spacemacs/init-open-junk-file ()
  (use-package open-junk-file
    :defer t
    :commands (open-junk-file)
    :init
    (evil-leader/set-key "fJ" 'open-junk-file)
    (setq open-junk-file-directory (concat spacemacs-cache-directory "junk/"))))

(defun spacemacs/init-info+ ()
  (use-package info+
    :defer t
    :init
    (progn
      (eval-after-load 'info
        '(require 'info+))
      (setq Info-fontify-angle-bracketed-flag nil))))

(defun spacemacs/init-leuven-theme ()
  (use-package leuven-theme
    :defer t
    :init (setq org-fontify-whole-heading-line t)))

(defun spacemacs/init-linum-relative ()
  (use-package linum-relative
    :commands linum-relative-toggle
    :init
    (evil-leader/set-key "tr" 'linum-relative-toggle)
    :config
    (progn
      (setq linum-format 'linum-relative)
      (setq linum-relative-current-symbol "")
      (linum-relative-toggle))))

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
      (add-to-list 'evil-motion-state-modes 'neotree-mode)
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
            neo-vc-integration '(face))

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
        (define-key evil-motion-state-local-map (kbd "TAB") 'neotree-stretch-toggle)
        (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter)
        (define-key evil-motion-state-local-map (kbd "|")   'neotree-enter-vertical-split)
        (define-key evil-motion-state-local-map (kbd "-")   'neotree-enter-horizontal-split)
        (define-key evil-motion-state-local-map (kbd "?")   'evil-search-backward)
        (define-key evil-motion-state-local-map (kbd "c")   'neotree-create-node)
        (define-key evil-motion-state-local-map (kbd "d")   'neotree-delete-node)
        (define-key evil-motion-state-local-map (kbd "g")   'neotree-refresh)
        (define-key evil-motion-state-local-map (kbd "h")   'spacemacs/neotree-collapse-or-up)
        (define-key evil-motion-state-local-map (kbd "H")   'neotree-select-previous-sibling-node)
        (define-key evil-motion-state-local-map (kbd "J")   'neotree-select-down-node)
        (define-key evil-motion-state-local-map (kbd "K")   'neotree-select-up-node)
        (define-key evil-motion-state-local-map (kbd "l")   'spacemacs/neotree-expand-or-open)
        (define-key evil-motion-state-local-map (kbd "L")   'neotree-select-next-sibling-node)
        (define-key evil-motion-state-local-map (kbd "q")   'neotree-hide)
        (define-key evil-motion-state-local-map (kbd "r")   'neotree-rename-node)
        (define-key evil-motion-state-local-map (kbd "R")   'neotree-change-root)
        (define-key evil-motion-state-local-map (kbd "s")   'neotree-hidden-file-toggle))

      (evil-leader/set-key
        "ft" 'neotree-toggle
        "pt" 'neotree-find-project-root))

    :config
    (spacemacs/add-to-hook 'neotree-mode-hook '(spacemacs//neotree-key-bindings))))

(defun spacemacs/init-pcre2el ()
  (use-package pcre2el
    :defer t
    :commands rxt-fontify-regexp-at-point
    :init
    (progn
      (spacemacs/declare-prefix "R" "pcre2el")
      (evil-leader/set-key
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
        "Rt"  'rxt-toggle-elisp-rx
        "Rh"  'rxt-fontify-regexp-at-point))))

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

      (evilify paradox-menu-mode paradox-menu-mode-map
               "H" 'paradox-menu-quick-help
               "J" 'paradox-next-describe
               "K" 'paradox-previous-describe
               "L" 'paradox-menu-view-commit-list
               "o" 'paradox-menu-visit-homepage)
      (evil-leader/set-key
        "aP" 'spacemacs/paradox-list-packages))))

(defun spacemacs/init-powerline ()
  (use-package powerline
    :init
    (progn
      ;; Custom format of minor mode lighters, they are separated by a pipe.
      (defpowerline spacemacs-powerline-minor-modes
        (mapconcat (lambda (mm)
                     (propertize
                      mm
                      'mouse-face 'mode-line-highlight
                      'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map
                                     [mode-line down-mouse-1]
                                     (powerline-mouse 'minor 'menu mm))
                                   (define-key map
                                     [mode-line mouse-2]
                                     (powerline-mouse 'minor 'help mm))
                                   (define-key map
                                     [mode-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu mm))
                                   (define-key map
                                     [header-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu mm))
                                   map)))
                   (split-string (format-mode-line minor-mode-alist))
                   (concat (propertize
                            (if (dotspacemacs|symbol-value
                                 dotspacemacs-mode-line-unicode-symbols) " " "") 'face face)
                           (unless (dotspacemacs|symbol-value
                                    dotspacemacs-mode-line-unicode-symbols) "|"))))

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
                                (spacemacs/switch-to-version spacemacs-new-version)
                                (setq spacemacs-mode-line-new-version-lighterp nil))
                            (message "Update aborted."))))
                      map)))

      (defvar spacemacs-mode-line-minor-modesp t
        "If not nil, minor modes lighter are displayed in the mode-line.")
      (spacemacs|add-toggle mode-line-minor-modes
        :status spacemacs-mode-line-minor-modesp
        :on (setq spacemacs-mode-line-minor-modesp t)
        :off (setq spacemacs-mode-line-minor-modesp nil)
        :documentation "Show minor modes in mode-line."
        :evil-leader "tmm")

      (defvar spacemacs-mode-line-major-modep t
        "If not nil, major mode is displayed in the mode-line.")
      (spacemacs|add-toggle mode-line-major-mode
        :status spacemacs-mode-line-major-modep
        :on (setq spacemacs-mode-line-major-modep t)
        :off (setq spacemacs-mode-line-major-modep nil)
        :documentation "Show major mode in mode-line."
        :evil-leader "tmM")

      (defvar spacemacs-mode-line-version-controlp t
        "If not nil, version control info is displayed in the mode-line.")
      (spacemacs|add-toggle mode-line-version-control
        :status spacemacs-mode-line-version-controlp
        :on (setq spacemacs-mode-line-version-controlp t)
        :off (setq spacemacs-mode-line-version-controlp nil)
        :documentation "Show version control info in mode-line."
        :evil-leader "tmv")

      (defvar spacemacs-mode-line-new-version-lighterp t
        "If not nil, new version lighter is displayed in the mode-line.")
      (spacemacs|add-toggle mode-line-new-version
        :status spacemacs-mode-line-new-version-lighterp
        :on (setq spacemacs-mode-line-new-version-lighterp t)
        :off (setq spacemacs-mode-line-new-version-lighterp nil)
        :documentation "Show new version in the mode-line."
        :evil-leader "tmV")

      (defvar spacemacs-mode-line-display-point-p nil
        "If not nil, display point alongside row/column in the mode-line.")
      (spacemacs|add-toggle mode-line-display-point
        :status spacemacs-mode-line-display-point-p
        :on (setq spacemacs-mode-line-display-point-p t)
        :off (setq spacemacs-mode-line-display-point-p nil)
        :documentation "Show point in the mode-line."
        :evil-leader "tmp")

      (defvar spacemacs-mode-line-org-clock-current-taskp nil
        "If not nil, the currently clocked org-mode task will be
displayed in the mode-line.")
      (defvar spacemacs-mode-line-org-clock-format-function
        'org-clock-get-clock-string
        "Function used to render the currently clocked org-mode task.")
      (spacemacs|add-toggle mode-line-org-clock-current-task
        :status spacemacs-mode-line-org-clock-current-taskp
        :on (setq spacemacs-mode-line-org-clock-current-taskp t)
        :off (setq spacemacs-mode-line-org-clock-current-taskp nil)
        :documentation "Show org clock in mode-line."
        :evil-leader "tmc")

      (defvar spacemacs-mode-line-left
        '(((workspace-number window-number)
           :fallback state-tag
           :separator "|"
           :face state-face)
          anzu
          (buffer-modified buffer-size buffer-id remote-host)
          major-mode
          ((flycheck-errors flycheck-warnings flycheck-infos)
           :when active)
          ((minor-modes process)
           :when active)
          (erc-track :when active)
          (version-control :when active)
          (org-pomodoro :when active)
          (org-clock :when active)
          nyan-cat)
        "List of modeline segments to render on the left. Each element
must be a valid segment specification, see documentation for
`spacemacs//eval-mode-line-segment'.")

      (defvar spacemacs-mode-line-right
        '((battery :when active)
          selection-info
          ((buffer-encoding-abbrev
            point-position
            line-column)
           :separator " | ")
          ((global-mode new-version)
           :when active)
          buffer-position
          hud)
        "List of modeline segments to render on the right. Each element
must be a valid segment specification, see documentation for
`spacemacs//eval-mode-line-segment'.")

      (defun spacemacs//mode-line-file-encoding ()
        "Return the file encoding to be displayed in the mode-line."
        (let ((buf-coding (format "%s" buffer-file-coding-system)))
          (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
              (match-string 1 buf-coding)
            buf-coding)))

      (if (display-graphic-p)
          (setq-default powerline-default-separator 'wave)
        (setq-default powerline-default-separator 'utf-8))

      (defun spacemacs/customize-powerline-faces ()
        "Alter powerline face to make them work with more themes."
        (set-face-attribute 'powerline-inactive2 nil
                            :inherit 'font-lock-comment-face))
      (spacemacs/customize-powerline-faces)

      (defmacro spacemacs|define-mode-line-segment (name value &rest props)
        "Defines a modeline segment called `NAME' whose value is
computed by the form `VALUE'. The optional keyword argument `WHEN'
defines a condition required for the segment to be shown.

This macro defines a function `spacemacs//mode-line-NAME' which
returns a list of modeline objects (strings or images). If the
form `VALUE' does not result in a list, the return value will be
wrapped as a singleton list.

All properties are stored in a plist attached to the symbol, to be
inspected at evaluation time by `spacemacs//eval-mode-line-segment'."
        (declare (indent 1))
        (let* ((wrapper-func (intern (format "spacemacs//mode-line-%S" name)))
               (wrapper-func-available (intern (format "%S-available" wrapper-func)))
               (condition (if (plist-member props :when)
                              (plist-get props :when)
                             t)))
          `(progn
             (defun ,wrapper-func ()
               (when ,condition
                 (let ((value ,value))
                   (cond ((spacemacs//imagep value)
                          (list value))
                         ((listp value) value)
                         ((and (stringp value)
                               (= 0 (length value)))
                          nil)
                         (t (list value))))))
             (setplist ',wrapper-func ',props))))

      ;; An intermediate representation of the value of a modeline segment.
      (defstruct segment
        objects face-left face-right tight-left tight-right)

      (defun column-number-at-pos (pos)
        "Analog to line-number-at-pos."
        (save-excursion (goto-char pos) (current-column)))

      (defun selection-info ()
        "Info on the current selection for the mode-line.

It is a string holding:
- the number of columns in the selection if it covers only one line,
- the number of lines in the selection if if covers several full lines
- or rowsxcols if it's a block selection."
        (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
               (chars (- (1+ (region-end)) (region-beginning)))
               (cols (1+ (abs (- (column-number-at-pos (region-end))
                                 (column-number-at-pos (region-beginning)))))))
          (if (eq evil-visual-selection 'block)
              (format "%d×%d block" lines cols)
            (if (> lines 1) (format "%d lines" lines)
              (format "%d chars" chars)))))

      ;; BEGIN define modeline segments

      (spacemacs|define-mode-line-segment workspace-number
        (spacemacs/workspace-number)
        :when (and (bound-and-true-p eyebrowse-mode)
                   (spacemacs/workspace-number)))

      (spacemacs|define-mode-line-segment window-number
        (spacemacs/window-number)
        :when (and (bound-and-true-p window-numbering-mode)
                   (spacemacs/window-number)))

      (spacemacs|define-mode-line-segment state-tag
        (s-trim (evil-state-property evil-state :tag t)))

      (spacemacs|define-mode-line-segment anzu
        (anzu--update-mode-line)
        :when (and active (bound-and-true-p anzu--state)))

      (spacemacs|define-mode-line-segment buffer-modified "%*")
      (spacemacs|define-mode-line-segment buffer-size
        (powerline-buffer-size))
      (spacemacs|define-mode-line-segment buffer-id
        (powerline-buffer-id))
      (spacemacs|define-mode-line-segment remote-host
        (concat "@" (file-remote-p default-directory 'host))
        :when (file-remote-p default-directory 'host))

      (spacemacs|define-mode-line-segment major-mode
        (powerline-major-mode)
        :when spacemacs-mode-line-major-modep)
      (spacemacs|define-mode-line-segment minor-modes
        (spacemacs-powerline-minor-modes)
        :when spacemacs-mode-line-minor-modesp)
      (spacemacs|define-mode-line-segment process
        (powerline-raw mode-line-process)
        :when (spacemacs//mode-line-nonempty mode-line-process))

      (spacemacs|define-mode-line-segment erc-track
        (let* ((buffers (mapcar 'car erc-modified-channels-alist))
               (long-names (mapconcat (lambda (buf)
                                        (or (buffer-name buf) ""))
                                      buffers " ")))
          long-names)
        :when (bound-and-true-p erc-track-mode))

      (spacemacs|define-mode-line-segment version-control
        (s-trim (powerline-vc))
        :when (and (powerline-vc)
                   spacemacs-mode-line-version-controlp))

      (spacemacs|define-mode-line-segment selection-info
        (selection-info)
        :when (evil-visual-state-p))

      (spacemacs|define-mode-line-segment buffer-encoding
        (format "%s" buffer-file-coding-system))
      (spacemacs|define-mode-line-segment buffer-encoding-abbrev
        (spacemacs//mode-line-file-encoding))

      (spacemacs|define-mode-line-segment point-position
        (format "%d" (point))
        :when spacemacs-mode-line-display-point-p)
      (spacemacs|define-mode-line-segment line-column "%l:%2c")
      (spacemacs|define-mode-line-segment buffer-position "%p")

      (spacemacs|define-mode-line-segment hud
        (powerline-hud state-face default-face)
        :tight t
        :when (string-match "\%" (format-mode-line "%p")))

      (spacemacs|define-mode-line-segment nyan-cat
        (powerline-raw (nyan-create) default-face)
        :when (bound-and-true-p nyan-mode))

      (spacemacs|define-mode-line-segment global-mode
        (powerline-raw (-difference global-mode-string
                                    spacemacs--global-mode-line-excludes))
        :when (spacemacs//mode-line-nonempty global-mode-string))

      (spacemacs|define-mode-line-segment battery
        (powerline-raw (s-trim (fancy-battery-default-mode-line))
                       (fancy-battery-powerline-face))
        :when (bound-and-true-p fancy-battery-mode))

      (spacemacs|define-mode-line-segment new-version
        (spacemacs-powerline-new-version
         (spacemacs/get-new-version-lighter-face
          spacemacs-version spacemacs-new-version))
        :when (and spacemacs-new-version
                   spacemacs-mode-line-new-version-lighterp))

      ;; flycheck-errors, flycheck-warnings, flycheck-infos
      (dolist (type '(error warning info))
        (let ((segment-name (intern (format "flycheck-%ss" type)))
              (face (intern (format "spacemacs-mode-line-flycheck-%s-face" type))))
          (eval
           `(spacemacs|define-mode-line-segment ,segment-name
              (powerline-raw (s-trim (spacemacs|custom-flycheck-lighter ,type)) ',face)
              :when (and (bound-and-true-p flycheck-mode)
                         (or flycheck-current-errors
                             (eq 'running flycheck-last-status-change))
                         (spacemacs|custom-flycheck-lighter ,type))))))

      (spacemacs|define-mode-line-segment org-clock
        (substring-no-properties (funcall spacemacs-mode-line-org-clock-format-function))
        :when (and spacemacs-mode-line-org-clock-current-taskp
                   (fboundp 'org-clocking-p)
                   (org-clocking-p)))
      (push 'org-mode-line-string spacemacs--global-mode-line-excludes)

      (spacemacs|define-mode-line-segment org-pomodoro
        (nth 1 org-pomodoro-mode-line)
        :when (and (fboundp 'org-pomodoro-active-p)
                   (org-pomodoro-active-p)))
      (push 'org-pomodoro-mode-line spacemacs--global-mode-line-excludes)

      ;; END define modeline segments

      (defun spacemacs//eval-mode-line-segment (segment-spec &rest outer-props)
        "Evaluates a modeline segment given by `SEGMENT-SPEC' with
additional properties given by `OUTER-PROPS'.

`SEGMENT-SPEC' may be either:
- A literal value (number or string, for example)
- A symbol previously defined by `spacemacs|define-mode-line-segment'
- A list whose car is a segment-spec and whose cdr is a plist of properties
- A list of segment-specs

The properties applied are, in order of priority:
- Those given by `SEGMENT-SPEC', if applicable
- The properties attached to the segment symbol, if applicable
- `OUTER-PROPS'

Valid properties are:
- `:tight-left' => if true, the segment should be rendered with no padding
  or separator on its left side
- `:tight-right' => corresponding option for the right side
- `:tight' => shorthand option to set both `:tight-left' and `:tight-right'
- `:when' => condition that determines whether this segment is shown
- `:fallback' => segment to evaluate if this segment produces no output
- `:separator' => string with which to separate nested segments
- `:face' => the face with which to render the segment

When calling nested or fallback segments, the full property list is passed
as `OUTER-PROPS', with the exception of `:fallback'. This means that more
deeply specified properties, as a rule, override the higher level ones.
The exception is `:when', which must be true at all levels.

The return vaule is a `segment' struct. Its `OBJECTS' list may be nil."

        ;; We get a property list from `SEGMENT-SPEC' if it's a list
        ;; with more than one element whose second element is a symbol
        ;; starting with a colon
        (let* ((input (if (and (listp segment-spec)
                               (cdr segment-spec)
                               (keywordp (cadr segment-spec)))
                          segment-spec
                        (cons segment-spec nil)))
               (segment (car input))
               (segment-symbol (when (symbolp segment)
                                 (intern (format "spacemacs//mode-line-%S" segment))))

               ;; Assemble the properties in the correct order
               (props (append (cdr input)
                              (when (symbolp segment) (symbol-plist segment-symbol))
                              outer-props))

               ;; Property list to be passed to nested or fallback segments
               (nest-props (append '(:fallback nil) (cdr input) outer-props))

               ;; Parse property list
               (condition (if (plist-member props :when)
                              (eval (plist-get props :when))
                            t))
               (face (eval (or (plist-get props :face) 'default-face)))
               (separator (powerline-raw (or (plist-get props :separator) " ") face))
               (tight-left (or (plist-get props :tight)
                               (plist-get props :tight-left)))
               (tight-right (or (plist-get props :tight)
                                (plist-get props :tight-right)))

               ;; Final output
               (result (make-segment :objects nil
                                     :face-left face
                                     :face-right face
                                     :tight-left tight-left
                                     :tight-right tight-right)))

          ;; Evaluate the segment based on its type
          (when condition
            (cond
             ;; A list of segments
             ((listp segment)
              (let ((results (remove-if-not
                              'segment-objects
                              (mapcar (lambda (s)
                                        (apply 'spacemacs//eval-mode-line-segment
                                               s nest-props))
                                      segment))))
                (when results
                  (setf (segment-objects result)
                        (apply 'append (spacemacs//intersperse
                                        (mapcar 'segment-objects results)
                                        (list separator))))
                  (setf (segment-face-left result)
                        (segment-face-left (car results)))
                  (setf (segment-face-right result)
                        (segment-face-right (car (last results))))
                  (setf (segment-tight-left result)
                        (segment-tight-left (car results)))
                  (setf (segment-tight-right result)
                        (segment-tight-right (car (last results)))))))
             ;; A single symbol
             ((symbolp segment)
              (setf (segment-objects result)
                    (mapcar (lambda (s)
                              (if (spacemacs//imagep s) s (powerline-raw s face)))
                            (funcall segment-symbol))))
             ;; A literal value
             (t (setf (segment-objects result)
                      (list (powerline-raw (format "%s" segment) face))))))

          (cond
           ;; This segment produced output, so return it
           ((segment-objects result) result)
           ;; Return the fallback segment, if any
           ((plist-get props :fallback)
            (apply 'spacemacs//eval-mode-line-segment
                   (plist-get props :fallback) nest-props))
           ;; No output (objects = nil)
           (t result))))

      (defun spacemacs//mode-line-prepare-any (spec side)
        "Prepares one side of the modeline. `SPEC' is a list of segment
specifications (see `spacemacs//eval-mode-line-segment'), and `SIDE' is
one of `l' or `r'."
        (let* ((active (powerline-selected-window-active))
               (line-face (if active 'powerline-active2 'powerline-inactive2))
               (default-face (if active 'powerline-active1 'powerline-inactive1))
               (other-face (if active 'mode-line 'mode-line-inactive))
               (state-face (if active (spacemacs/current-state-face) line-face))

               ;; Loop through the segments and collect the results
               (segments (loop with result
                               for s in spec
                               do (setq result (spacemacs//eval-mode-line-segment s))
                               if (segment-objects result)
                                 collect result
                                 and do (rotatef default-face other-face)))

               (dummy (make-segment :face-left line-face :face-right line-face))
               (separator-style (format "powerline-%S" powerline-default-separator))
               (default-separator (intern (format "%s-%S" separator-style
                                                  (car powerline-default-separator-dir))))
               (other-separator (intern (format "%s-%S" separator-style
                                                (cdr powerline-default-separator-dir)))))

          ;; Collect all segment values and add separators
          (apply 'append
                 (mapcar
                  (lambda (pair)
                    (let* ((lhs (car pair))
                           (rhs (cdr pair))
                           (objs (if (eq 'l side) lhs rhs))
                           (add-sep (not (or (segment-tight-right lhs)
                                             (segment-tight-left rhs)))))
                      (rotatef default-separator other-separator)
                      (append
                       (when (and (eq 'r side) add-sep)
                         (list (funcall default-separator
                                        (segment-face-right lhs)
                                        (segment-face-left rhs))))
                       (unless (segment-tight-left objs)
                         (list (powerline-raw " " (segment-face-left objs))))
                       (segment-objects objs)
                       (unless (segment-tight-right objs)
                         (list (powerline-raw " " (segment-face-right objs))))
                       (when (and (eq 'l side) add-sep)
                         (list (funcall default-separator
                                        (segment-face-right lhs)
                                        (segment-face-left rhs)))))))
                  (-zip (if (eq 'l side) segments (cons dummy segments))
                        (if (eq 'l side) (append (cdr segments) (list dummy)) segments))))))

      (defun spacemacs//mode-line-prepare-left ()
        (spacemacs//mode-line-prepare-any spacemacs-mode-line-left 'l))

      (defun spacemacs//mode-line-prepare-right ()
        (spacemacs//mode-line-prepare-any spacemacs-mode-line-right 'r))

      (defun spacemacs//mode-line-prepare ()
        ;; diminish the lighters
        (when spacemacs-mode-line-minor-modesp
          (let ((unicodep (dotspacemacs|symbol-value
                           dotspacemacs-mode-line-unicode-symbols)))
            (dolist (mm spacemacs--diminished-minor-modes)
              (let ((mode (car mm)))
                (when (and (boundp mode) (symbol-value mode))
                  (let* ((unicode (cadr mm))
                         (ascii (caddr mm))
                         (dim (if unicodep
                                  unicode
                                (if ascii ascii unicode))))
                    (diminish mode dim)))))))
        (let* ((active (powerline-selected-window-active))
               (lhs (spacemacs//mode-line-prepare-left))
               (rhs (spacemacs//mode-line-prepare-right))
               (line-face (if active 'powerline-active2 'powerline-inactive2)))
          ;; create the line
          (concat (powerline-render lhs)
                  (powerline-fill line-face (powerline-width rhs))
                  (powerline-render rhs))))

      (setq-default mode-line-format
                    '("%e" (:eval (spacemacs//mode-line-prepare))))

      (defun spacemacs//restore-powerline (buffer)
        "Restore the powerline in buffer"
        (with-current-buffer buffer
              (setq-local mode-line-format
                          '("%e" (:eval (spacemacs//mode-line-prepare))))
              (powerline-set-selected-window)
              (powerline-reset)))

      (defun spacemacs//set-powerline-for-startup-buffers ()
        "Set the powerline for buffers created when Emacs starts."
        (unless configuration-layer-error-count
          (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
            (when (and (get-buffer buffer)
                       (configuration-layer/package-usedp 'powerline))
              (spacemacs//restore-powerline buffer)))))
      (add-hook 'emacs-startup-hook
                'spacemacs//set-powerline-for-startup-buffers))))

(defun spacemacs/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (evil-leader/set-key "tCd" 'rainbow-delimiters-mode)
      (when (member dotspacemacs-highlight-delimiters '(any all))
        (spacemacs/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook))))))

(defun spacemacs/init-smartparens ()
  (use-package smartparens
    :defer t
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
            sp-cancel-autoskip-on-backward-movement nil))
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
  (if dotspacemacs-smooth-scrolling
      (use-package smooth-scrolling
        :init
        (setq smooth-scroll-margin 5
              scroll-conservatively 101
              scroll-preserve-screen-position t
              auto-window-vscroll nil)
        :config
        (setq scroll-margin 5))

    ;; deactivate the defadvice's
    (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
    (ad-activate 'previous-line)
    (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
    (ad-activate 'next-line)
    (ad-disable-advice 'isearch-repeat 'after 'isearch-smooth-scroll)
    (ad-activate 'isearch-repeat)))

(defun spacemacs/init-solarized-theme ()
  (use-package solarized
    :init
    (progn
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (deftheme solarized-light "The light variant of the Solarized colour theme"))))

(defun spacemacs/init-spray ()
  (use-package spray
    :commands spray-mode
    :init
    (progn
      (defun spacemacs/start-spray ()
        "Start spray speed reading on current buffer at current point."
        (interactive)
        (evil-insert-state)
        (spray-mode t)
        (internal-show-cursor (selected-window) nil))
      (evil-leader/set-key "asr" 'spacemacs/start-spray)

      (defadvice spray-quit (after spacemacs//quit-spray activate)
        "Correctly quit spray."
        (internal-show-cursor (selected-window) t)
        (evil-normal-state)))
    :config
    (progn
      (define-key spray-mode-map (kbd "h") 'spray-backward-word)
      (define-key spray-mode-map (kbd "l") 'spray-forward-word)
      (define-key spray-mode-map (kbd "q") 'spray-quit))))

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
        :evil-leader "t~")
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
      (when (configuration-layer/package-usedp 'powerline)
        (defun window-numbering-install-mode-line (&optional position)
          "Do nothing, the display is handled by the powerline."))
      (setq window-numbering-auto-assign-0-to-minibuffer nil)
      (evil-leader/set-key
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

    (defun spacemacs/window-number ()
      "Return the number of the window."
      (let* ((num (window-numbering-get-number))
             (str (if num (int-to-string num))))
        (cond
         ((not (dotspacemacs|symbol-value
                dotspacemacs-mode-line-unicode-symbols)) str)
         ((equal str "1")  "➊")
         ((equal str "2")  "➋")
         ((equal str "3")  "➌")
         ((equal str "4")  "➍")
         ((equal str "5")  "➎")
         ((equal str "6")  "❻")
         ((equal str "7")  "➐")
         ((equal str "8")  "➑")
         ((equal str "9")  "➒")
         ((equal str "0")  "➓"))))

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
        :doc "[+] zoom frame in [-] zoom frame out [=] reset zoom"
        :evil-leader "zf"
        :use-minibuffer t
        :bindings
        ("+" spacemacs/zoom-frm-in :post (spacemacs//zoom-frm-powerline-reset))
        ("-" spacemacs/zoom-frm-out :post (spacemacs//zoom-frm-powerline-reset))
        ("=" spacemacs/zoom-frm-unzoom :post (spacemacs//zoom-frm-powerline-reset)))

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
