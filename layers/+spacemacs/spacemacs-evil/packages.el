;;; packages.el --- Spacemacs Evil Layer packages File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-evil-packages
      '(evil-anzu
        evil-args
        evil-ediff
        evil-exchange
        evil-iedit-state
        evil-indent-plus
        evil-lisp-state
        ;; for testing purpose, contribute by reporting bugs and sending PRs
        ;; to https://github.com/gabesoft/evil-mc
        ;; To enable it add `(global-evil-mc-mode)' to user-config function
        evil-mc
        evil-nerd-commenter
        evil-matchit
        evil-numbers
        evil-search-highlight-persist
        evil-surround
        ;; Temporarily disabled, pending the resolution of
        ;; https://github.com/7696122/evil-terminal-cursor-changer/issues/8
        ;; evil-terminal-cursor-changer
        evil-tutor
        (evil-unimpaired :location (recipe :fetcher local))
        evil-visual-mark-mode
        (hs-minor-mode :location built-in)
        linum-relative
        vi-tilde-fringe
        ))

(defun spacemacs-evil/init-evil-anzu ()
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

(defun spacemacs-evil/init-evil-args ()
  (use-package evil-args
    :init
    (progn
      ;; bind evil-args text objects
      (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
      (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))))

(defun spacemacs-evil/init-evil-ediff ()
  (use-package evil-ediff
    :after (ediff)
    :if (memq dotspacemacs-editing-style '(hybrid vim))))

(defun spacemacs-evil/init-evil-exchange ()
  (use-package evil-exchange
    :init (evil-exchange-install)))

(defun spacemacs-evil/init-evil-iedit-state ()
  (use-package evil-iedit-state
    :commands (evil-iedit-state evil-iedit-state/iedit-mode)
    :init
    (progn
      (setq iedit-current-symbol-default t
            iedit-only-at-symbol-boundaries t
            iedit-toggle-key-default nil)
      (spacemacs/set-leader-keys "se" 'evil-iedit-state/iedit-mode))
    :config
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)
    (spacemacs//iedit-insert-state-hybrid dotspacemacs-editing-style)
    (add-hook 'spacemacs-editing-style-hook
              #'spacemacs//iedit-insert-state-hybrid)))

(defun spacemacs-evil/init-evil-indent-plus ()
  (use-package evil-indent-plus
    :init (evil-indent-plus-default-bindings)))

(defun spacemacs-evil/init-evil-lisp-state ()
  (use-package evil-lisp-state
    :init (setq evil-lisp-state-global t)
    :config (spacemacs/set-leader-keys "k" evil-lisp-state-map)))

(defun spacemacs-evil/init-evil-mc ()
  (use-package evil-mc
    :defer t
    :init
    ;; remove emc prefix when there is not multiple cursors
    (setq evil-mc-mode-line
          `(:eval (when (> (evil-mc-get-cursor-count) 1)
                    (format ,(propertize " %s:%d" 'face 'cursor)
                            evil-mc-mode-line-prefix
                            (evil-mc-get-cursor-count)))))))

;; other commenting functions in funcs.el with keybinds in keybindings.el
(defun spacemacs-evil/init-evil-nerd-commenter ()
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

(defun spacemacs-evil/init-evil-matchit ()
  (use-package evil-matchit
    :defer t))

(defun spacemacs-evil/init-evil-numbers ()
  (use-package evil-numbers
    :config
    (progn
      (spacemacs|define-transient-state evil-numbers
        :title "Evil Numbers Transient State"
        :doc "\n[_+_/_=_] increase number  [_-_] decrease  [0..9] prefix  [_q_] quit"
        :bindings
        ("+" evil-numbers/inc-at-pt)
        ("=" evil-numbers/inc-at-pt)
        ("-" evil-numbers/dec-at-pt)
        ("q" nil :exit t))
      (spacemacs/set-leader-keys
        "n+" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
        "n=" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
        "n-" 'spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt))))

(defun spacemacs-evil/init-evil-search-highlight-persist ()
  (use-package evil-search-highlight-persist
    :init
    (progn
      (global-evil-search-highlight-persist)
      ;; (set-face-attribute )
      (spacemacs/set-leader-keys "sc" 'spacemacs/evil-search-clear-highlight)
      (define-key evil-search-highlight-persist-map (kbd "C-x SPC") 'rectangle-mark-mode)
      (evil-ex-define-cmd "nohlsearch"
                          'evil-search-highlight-persist-remove-all)
      (spacemacs//adaptive-evil-highlight-persist-face)
      (add-hook 'spacemacs-post-theme-change-hook 'spacemacs//adaptive-evil-highlight-persist-face))))

(defun spacemacs-evil/init-evil-surround ()
  (use-package evil-surround
    :init
    (progn
      (global-evil-surround-mode 1)
      ;; `s' for surround instead of `substitute'
      ;; see motivation for this change in the documentation
      (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))))

(defun spacemacs-evil/init-evil-terminal-cursor-changer ()
  (use-package evil-terminal-cursor-changer
    :if (not (display-graphic-p))
    :init (setq evil-visual-state-cursor 'box
                evil-insert-state-cursor 'bar
                evil-emacs-state-cursor 'hbar)))

(defun spacemacs-evil/init-evil-tutor ()
  (use-package evil-tutor
    :commands (evil-tutor-start
               evil-tutor-resume)
    :init
    (progn
      (setq evil-tutor-working-directory
            (concat spacemacs-cache-directory ".tutor/"))
      (spacemacs/set-leader-keys "hT" 'evil-tutor-start))))

(defun spacemacs-evil/init-evil-unimpaired ()
  ;; No laziness here, unimpaired bindings should be available right away.
  (use-package evil-unimpaired))

(defun spacemacs-evil/init-evil-visual-mark-mode ()
  (use-package evil-visual-mark-mode
    :defer t
    :init
    (spacemacs|add-toggle evil-visual-mark-mode
      :mode evil-visual-mark-mode
      :documentation "Enable evil visual marks mode."
      :evil-leader "t`")))

(defun spacemacs-evil/init-hs-minor-mode ()
  (add-hook 'prog-mode-hook 'spacemacs//enable-hs-minor-mode))

(defun spacemacs-evil/init-linum-relative ()
  (use-package linum-relative
    :commands (linum-relative-toggle linum-relative-on)
    :init
    (progn
      (when (or (eq dotspacemacs-line-numbers 'relative)
                (and (listp dotspacemacs-line-numbers)
                     (car (spacemacs/mplist-get dotspacemacs-line-numbers
                                                :relative))))
        (add-hook 'spacemacs-post-user-config-hook 'linum-relative-on))
      (spacemacs/set-leader-keys "tr" 'spacemacs/linum-relative-toggle))
    :config
    (setq linum-relative-current-symbol "")))

(defun spacemacs-evil/init-vi-tilde-fringe ()
  (spacemacs|do-after-display-system-init
   (use-package vi-tilde-fringe
     :init
     (progn
       (global-vi-tilde-fringe-mode)
       (spacemacs|add-toggle vi-tilde-fringe
         :mode global-vi-tilde-fringe-mode
         :documentation
         "Globally display a ~ on empty lines in the fringe."
         :evil-leader "T~")
       ;; don't enable it on some special buffers
       (with-current-buffer spacemacs-buffer-name (spacemacs/disable-vi-tilde-fringe))
       (add-hook 'which-key-init-buffer-hook 'spacemacs/disable-vi-tilde-fringe)
       ;; after a major mode is loaded, check if the buffer is read only
       ;; if so, disable vi-tilde-fringe-mode
       (add-hook 'after-change-major-mode-hook
                 'spacemacs/disable-vi-tilde-fringe-read-only)
       ;; TODO move this hook if/when we have a layer for eww
       (spacemacs/add-to-hooks 'spacemacs/disable-vi-tilde-fringe
                               '(eww-mode-hook)))
     :config
     (spacemacs|hide-lighter vi-tilde-fringe-mode))))
