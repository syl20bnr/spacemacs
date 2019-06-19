;;; packages.el --- Spacemacs Evil Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-evil-packages
      '(
        evil-anzu
        evil-args
        evil-cleverparens
        evil-ediff
        evil-escape
        evil-exchange
        evil-goggles
        evil-iedit-state
        evil-indent-plus
        evil-lion
        evil-lisp-state
        evil-nerd-commenter
        evil-matchit
        evil-numbers
        evil-surround
        ;; Temporarily disabled, pending the resolution of
        ;; https://github.com/7696122/evil-terminal-cursor-changer/issues/8
        ;; evil-terminal-cursor-changer
        evil-textobj-line
        evil-tutor
        (evil-unimpaired :location (recipe :fetcher local))
        evil-visual-mark-mode
        evil-visualstar
        (hs-minor-mode :location built-in)
        (linum-relative :toggle (version< emacs-version "26"))
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
      (when (configuration-layer/package-used-p 'spaceline)
        (defun spacemacs/anzu-update-mode-line (here total)
          "Custom update function which does not propertize the status."
          (when anzu--state
            (let ((status
                   (cl-case anzu--state
                     (search (format "(%s/%d%s)"
                                     (anzu--format-here-position here total)
                                     total (if anzu--overflow-p "+" "")))
                     (replace-query (format "(%d replace)" total))
                     (replace (format "(%d/%d)" here total)))))
              status)))
        (setq anzu-mode-line-update-function
              'spacemacs/anzu-update-mode-line)))))

(defun spacemacs-evil/init-evil-args ()
  (use-package evil-args
    :defer t
    :init
    (progn
      ;; bind evil-args text objects
      (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
      (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))))

(defun spacemacs-evil/init-evil-cleverparens ()
  (use-package evil-cleverparens
    :defer t
    :init
    (progn
      (setq evil-cleverparens-use-regular-insert t)
      (eval `(spacemacs|add-toggle evil-safe-lisp-structural-editing
               :if (memq dotspacemacs-editing-style '(vim hybrid))
               :mode evil-cleverparens-mode
               :documentation "Enable evil-cleverparens."
               :evil-leader-for-mode
               ,@(mapcar (lambda (x) (cons x "Ts"))
                         evil-lisp-safe-structural-editing-modes)))
      (spacemacs|diminish evil-cleverparens-mode " 🆂" " [s]"))))

(defun spacemacs-evil/init-evil-ediff ()
  (use-package evil-ediff
    :after (ediff)
    :if (memq dotspacemacs-editing-style '(hybrid vim))))

(defun spacemacs-evil/init-evil-escape ()
  (use-package evil-escape
    :defer t
    :init
    (add-hook 'emacs-startup-hook
              (lambda ()
                (spacemacs|add-transient-hook evil-normal-state-exit-hook
                  (lambda () (require 'evil-escape))
                  lazy-load-evil-escape-1)
                (spacemacs|add-transient-hook window-configuration-change-hook
                  (lambda () (require 'evil-escape))
                  lazy-load-evil-escape-2)))
    :config
    (progn
      (add-hook 'spacemacs-editing-style-hook #'spacemacs//evil-escape-deactivate-in-holy-mode)
      ;; apply once when emacs starts
      (spacemacs//evil-escape-deactivate-in-holy-mode dotspacemacs-editing-style)
      (spacemacs|hide-lighter evil-escape-mode))))

(defun spacemacs-evil/init-evil-exchange ()
  (use-package evil-exchange
    :defer t
    :init
    (progn
      (let ((evil-exchange-key (kbd "gx"))
            (evil-exchange-cancel-key (kbd "gX")))
        (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
        (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
        (define-key evil-normal-state-map evil-exchange-cancel-key
          'evil-exchange-cancel)
        (define-key evil-visual-state-map evil-exchange-cancel-key
          'evil-exchange-cancel)))))

(defun spacemacs-evil/init-evil-goggles ()
  (use-package evil-goggles
    :defer t
    :init
    (progn
      ;; disable pulses as it is more distracting than useful and
      ;; less readable.
      (setq evil-goggles-pulse nil
            evil-goggles-async-duration 0.1
            evil-goggles-blocking-duration 0.05)
      (when (or vim-style-visual-feedback
              hybrid-style-visual-feedback)
        (spacemacs|add-transient-hook evil-operator-state-entry-hook
          (lambda () (require 'evil-goggles))
          lazy-load-evil-googles)))
    :config
    (progn
      (if (or vim-style-visual-feedback
              hybrid-style-visual-feedback)
          (evil-goggles-mode)
        (evil-goggles-mode -1))
      (spacemacs|hide-lighter evil-goggles-mode))))

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
    (progn
      ;; set TAB action
      (add-hook 'spacemacs-editing-style-hook
                #'spacemacs//iedit-state-TAB-key-bindings)
      (spacemacs//iedit-state-TAB-key-bindings dotspacemacs-editing-style)
      ;; activate leader in iedit and iedit-insert states
      (define-key evil-iedit-state-map
        (kbd dotspacemacs-leader-key) spacemacs-default-map)
      (spacemacs//iedit-insert-state-hybrid dotspacemacs-editing-style)
      (add-hook 'spacemacs-editing-style-hook
                #'spacemacs//iedit-insert-state-hybrid))))

(defun spacemacs-evil/init-evil-indent-plus ()
  (use-package evil-indent-plus
    :defer t
    :init
    (progn
      (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
      (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
      (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
      (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
      (define-key evil-inner-text-objects-map "J"
        'evil-indent-plus-i-indent-up-down)
      (define-key evil-outer-text-objects-map "J"
        'evil-indent-plus-a-indent-up-down))))

(defun spacemacs-evil/init-evil-lion ()
  (use-package evil-lion
    :defer t
    :init
    (progn
      ;; Override the default keys, as they collide (with what ? :-))
      (setq evil-lion-left-align-key nil
            evil-lion-right-align-key nil)
      (spacemacs/set-leader-keys
        "xal" 'evil-lion-left
        "xaL" 'evil-lion-right))
    :config (evil-lion-mode)))

(defun spacemacs-evil/init-evil-lisp-state ()
  (use-package evil-lisp-state
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'spacemacs//load-evil-lisp-state)
      (setq evil-lisp-state-global t))
    :config (spacemacs/set-leader-keys "k" evil-lisp-state-map)))

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
    :defer t
    :init
    (progn
      (spacemacs|define-transient-state evil-numbers
        :title "Evil Numbers Transient State"
        :doc
        "\n[_+_/_=_] increase number  [_-_] decrease  [0..9] prefix  [_q_] quit"
        :bindings
        ("+" evil-numbers/inc-at-pt)
        ("=" evil-numbers/inc-at-pt)
        ("-" evil-numbers/dec-at-pt)
        ("q" nil :exit t))
      (spacemacs/set-leader-keys
        "n+" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
        "n=" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
        "n-" 'spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt))))

(defun spacemacs-evil/init-evil-surround ()
  (use-package evil-surround
    :defer t
    :init
    (progn
      ;; `s' for surround instead of `substitute'
      ;; see motivation here:
      ;; https://github.com/syl20bnr/spacemacs/blob/develop/doc/DOCUMENTATION.org#the-vim-surround-case
      (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
      (spacemacs|add-transient-hook evil-visual-state-entry-hook
        (lambda () (require 'evil-surround))
        lazy-load-evil-surround)
      (spacemacs|add-transient-hook evil-operator-state-entry-hook
        (lambda () (require 'evil-surround))
        lazy-load-evil-surround-2))
    :config
    (progn
      (global-evil-surround-mode 1))))

(defun spacemacs-evil/init-evil-terminal-cursor-changer ()
  (use-package evil-terminal-cursor-changer
    :if (not (display-graphic-p))
    :init (setq evil-visual-state-cursor 'box
                evil-insert-state-cursor 'bar
                evil-emacs-state-cursor 'hbar)))

(defun spacemacs-evil/init-evil-textobj-line ()
  ;; No laziness here, the line text object should be available right away.
  (use-package evil-textobj-line))

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

(defun spacemacs-evil/init-evil-visualstar ()
  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search-forward
               evil-visualstar/begin-search-backward)
    :init
    (progn
      (define-key evil-visual-state-map (kbd "*")
        'evil-visualstar/begin-search-forward)
      (define-key evil-visual-state-map (kbd "#")
        'evil-visualstar/begin-search-backward))))

(defun spacemacs-evil/init-hs-minor-mode ()
  (add-hook 'prog-mode-hook 'spacemacs//enable-hs-minor-mode))

(defun spacemacs-evil/init-linum-relative ()
  (use-package linum-relative
    :commands (linum-relative-toggle linum-relative-on)
    :init
    (progn
      (when (spacemacs/relative-line-numbers-p)
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
       (with-current-buffer spacemacs-buffer-name
         (spacemacs/disable-vi-tilde-fringe))
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
