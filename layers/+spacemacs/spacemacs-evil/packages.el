;;; packages.el --- Spacemacs Evil Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
        ;; Temporarily disabled, pending the resolution of
        ;; https://github.com/7696122/evil-terminal-cursor-changer/issues/8
        (evil-terminal-cursor-changer :excluded t)
        evil-tutor
        (evil-unimpaired :skip-install t)))

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

(defun spacemacs-evil/init-evil-exchange ()
  (use-package evil-exchange
    :init (evil-exchange-install)))

(defun spacemacs-evil/init-evil-iedit-state ()
  (use-package evil-iedit-state
    :commands (evil-iedit-state evil-iedit-state/iedit-mode)
    :init (spacemacs/set-leader-keys "se" (if (eq dotspacemacs-editing-style 'emacs)
                                              'iedit-mode
                                            'evil-iedit-state/iedit-mode))
    :config
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)))

(defun spacemacs-evil/init-evil-indent-plus ()
  (use-package evil-indent-plus
    :init (evil-indent-plus-default-bindings)))

(defun spacemacs-evil/init-evil-lisp-state ()
  (use-package evil-lisp-state
    :init (setq evil-lisp-state-global t)
    :config (spacemacs/set-leader-keys "k" evil-lisp-state-map)))

(defun spacemacs-evil/init-evil-mc ()
  (use-package evil-mc
    :defer t))

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
        :doc "\n[_+_/_=_] increase number [_-_] decrease"
        :bindings
        ("+" evil-numbers/inc-at-pt)
        ("=" evil-numbers/inc-at-pt)
        ("-" evil-numbers/dec-at-pt))
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
    (save-excursion
      (evil-insert-newline-above)))

  (defun evil-unimpaired/insert-space-below ()
    (interactive)
    (save-excursion
      (evil-insert-newline-below)))

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
  (define-key evil-normal-state-map (kbd "] q") 'spacemacs/next-error)
  (define-key evil-normal-state-map (kbd "[ q") 'spacemacs/previous-error)
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

