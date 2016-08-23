;;; packages.el --- shell packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq shell-packages
      '(
        company
        helm
        multi-term
        (comint :location built-in)
        xterm-color
        shell
        shell-pop
        term
        eshell
        eshell-prompt-extras
        esh-help
        magit
        ))

(defun shell/pre-init-company ()
  ;; support in eshell
  (spacemacs|use-package-add-hook eshell
    :post-init
    (progn
      (push 'company-capf company-backends-eshell-mode)
      (spacemacs|add-company-hook eshell-mode))
    :post-config
    (progn
      (defun spacemacs//toggle-shell-auto-completion-based-on-path ()
        "Deactivates automatic completion on remote paths.
Retrieving completions for Eshell blocks Emacs. Over remote
connections the delay is often annoying, so it's better to let
the user activate the completion manually."
        (if (file-remote-p default-directory)
            (setq-local company-idle-delay nil)
          (setq-local company-idle-delay 0.2)))
      (add-hook 'eshell-directory-change-hook
                'spacemacs//toggle-shell-auto-completion-based-on-path)
      ;; The default frontend screws everything up in short windows like
      ;; terminal often are
      (defun spacemacs//eshell-switch-company-frontend ()
        "Sets the company frontend to `company-preview-frontend' in e-shell mode."
        (setq-local company-frontends '(company-preview-frontend)))
      (add-hook 'eshell-mode-hook
                'spacemacs//eshell-switch-company-frontend))))

(defun shell/init-eshell ()
  (use-package eshell
    :defer t
    :init
    (progn
      (setq eshell-cmpl-cycle-completions nil
            ;; auto truncate after 20k lines
            eshell-buffer-maximum-lines 20000
            ;; history size
            eshell-history-size 350
            ;; no duplicates in history
            eshell-hist-ignoredups t
            ;; buffer shorthand -> echo foo > #'buffer
            eshell-buffer-shorthand t
            ;; my prompt is easy enough to see
            eshell-highlight-prompt nil
            ;; treat 'echo' like shell echo
            eshell-plain-echo-behavior t
            ;; cache directory
            eshell-directory-name (concat spacemacs-cache-directory "eshell/"))

      (defun spacemacs//eshell-auto-end ()
        "Move point to end of current prompt when switching to insert state."
        (when (and (eq major-mode 'eshell-mode)
                   ;; Not on last line, we might want to edit within it.
                   (not (eq (line-end-position) (point-max))))
          (end-of-buffer)))

      (when shell-protect-eshell-prompt
        (defun spacemacs//protect-eshell-prompt ()
          "Protect Eshell's prompt like Comint's prompts.

E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
          (let ((inhibit-field-text-motion t))
            (add-text-properties
             (point-at-bol)
             (point)
             '(rear-nonsticky t
               inhibit-line-move-field-capture t
               field output
               read-only t
               front-sticky (field inhibit-line-move-field-capture)))))
        (add-hook 'eshell-after-prompt-hook 'spacemacs//protect-eshell-prompt))

      (defun spacemacs//init-eshell ()
        "Stuff to do when enabling eshell."
        (setq pcomplete-cycle-completions nil)
        (if (bound-and-true-p linum-mode) (linum-mode -1))
        (unless shell-enable-smart-eshell
          ;; we don't want auto-jump to prompt when smart eshell is enabled.
          ;; Idea: maybe we could make auto-jump smarter and jump only if the
          ;; point is not on a prompt line
          (add-hook 'evil-insert-state-entry-hook
                    'spacemacs//eshell-auto-end nil t))
        (when (configuration-layer/package-usedp 'semantic)
          (semantic-mode -1)))

      ;; Defining a function like this makes it possible to type 'clear' in eshell and have it work
      (defun eshell/clear ()
        (interactive)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (eshell-send-input))

      ;; Caution! this will erase buffer's content at C-l
      (add-hook 'eshell-mode-hook
         #'(lambda ()
             (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)
             (define-key eshell-mode-map (kbd "C-d") 'eshell-life-is-too-much)))
      (add-hook 'eshell-mode-hook 'spacemacs//init-eshell))
    :config
    (progn
      (require 'esh-opt)

      ;; quick commands
      (defalias 'eshell/e 'find-file-other-window)
      (defalias 'eshell/d 'dired)
      (setenv "PAGER" "cat")

      ;; support `em-smart'
      (when shell-enable-smart-eshell
        (require 'em-smart)
        (setq eshell-where-to-jump 'begin
              eshell-review-quick-commands nil
              eshell-smart-space-goes-to-end t)
        (add-hook 'eshell-mode-hook 'eshell-smart-initialize))

      ;; Visual commands
      (require 'em-term)
      (mapc (lambda (x) (push x eshell-visual-commands))
            '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

      ;; automatically truncate buffer after output
      (when (boundp 'eshell-output-filter-functions)
        (push 'eshell-truncate-buffer eshell-output-filter-functions))

      ;; These don't work well in normal state
      ;; due to evil/emacs cursor incompatibility
      (evil-define-key 'insert eshell-mode-map
        (kbd "C-k") 'eshell-previous-matching-input-from-input
        (kbd "C-j") 'eshell-next-matching-input-from-input))))

(defun shell/init-esh-help ()
  (use-package esh-help
    :defer t
    :init (add-hook 'eshell-mode-hook 'eldoc-mode)
    :config (setup-esh-help-eldoc)))

(defun shell/init-eshell-prompt-extras ()
  (use-package eshell-prompt-extras
    :commands epe-theme-lambda
    :init
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(defun shell/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-init
    (progn
      ;; eshell
      (defun spacemacs/helm-eshell-history ()
        "Correctly revert to insert state after selection."
        (interactive)
        (helm-eshell-history)
        (evil-insert-state))
      (defun spacemacs/helm-shell-history ()
        "Correctly revert to insert state after selection."
        (interactive)
        (helm-comint-input-ring)
        (evil-insert-state))
      (defun spacemacs/init-helm-eshell ()
        "Initialize helm-eshell."
        ;; this is buggy for now
        ;; (define-key eshell-mode-map (kbd "<tab>") 'helm-esh-pcomplete)
        (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
          "H" 'spacemacs/helm-eshell-history)
        (define-key eshell-mode-map
          (kbd "M-l") 'spacemacs/helm-eshell-history))
      (add-hook 'eshell-mode-hook 'spacemacs/init-helm-eshell)
      ;;shell
      (spacemacs/set-leader-keys-for-major-mode 'shell-mode
        "H" 'spacemacs/helm-shell-history))))

(defun shell/init-multi-term ()
  (use-package multi-term
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "ast" 'shell-pop-multi-term)
      (defun multiterm (_)
        "Wrapper to be able to call multi-term from shell-pop"
        (interactive)
        (multi-term)))
    :config
    (progn
      (defun term-send-tab ()
        "Send tab in term mode."
        (interactive)
        (term-send-raw-string "\t"))
      (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
      ;; multi-term commands to create terminals and move through them.
      (spacemacs/set-leader-keys-for-major-mode 'term-mode "c" 'multi-term)
      (spacemacs/set-leader-keys-for-major-mode 'term-mode "p" 'multi-term-prev)
      (spacemacs/set-leader-keys-for-major-mode 'term-mode "n" 'multi-term-next)

      (when (configuration-layer/package-usedp 'projectile)
        (defun projectile-multi-term-in-root ()
          "Invoke `multi-term' in the project's root."
          (interactive)
          (projectile-with-default-dir (projectile-project-root) (multi-term)))
        (spacemacs/set-leader-keys "p$t" 'projectile-multi-term-in-root)))))

(defun shell/init-comint ()
  (setq comint-prompt-read-only t))

(defun shell/init-xterm-color ()
  (use-package xterm-color
    :init
    (progn
      ;; Comint and Shell
      (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
      (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
      (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)
      (with-eval-after-load 'esh-mode
        (add-hook 'eshell-mode-hook (lambda () (setq xterm-color-preserve-properties t)))
        (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
        (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))))))

(defun shell/init-shell ()
  (defun shell-comint-input-sender-hook ()
    "Check certain shell commands.
 Executes the appropriate behavior for certain commands."
    (setq comint-input-sender
          (lambda (proc command)
            (cond
             ;; Check for clear command and execute it.
             ((string-match "^[ \t]*clear[ \t]*$" command)
              (comint-send-string proc "\n")
              (erase-buffer))
             ;; Check for man command and execute it.
             ((string-match "^[ \t]*man[ \t]*" command)
              (comint-send-string proc "\n")
              (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
              (setq command (replace-regexp-in-string "[ \t]+$" "" command))
              (funcall 'man command))
             ;; Send other commands to the default handler.
             (t (comint-simple-send proc command))))))
  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook))

(defun shell/init-shell-pop ()
  (use-package shell-pop
    :defer t
    :init
    (progn
      (setq shell-pop-window-position shell-default-position
            shell-pop-window-size     shell-default-height
            shell-pop-term-shell      shell-default-term-shell
            shell-pop-full-span t)
      (defmacro make-shell-pop-command (type &optional shell)
        (let* ((name (symbol-name type)))
          `(defun ,(intern (concat "shell-pop-" name)) (index)
             (interactive "P")
             (require 'shell-pop)
             (shell-pop--set-shell-type
              'shell-pop-shell-type
              (backquote (,name
                          ,(concat "*" name "*")
                          (lambda nil (funcall ',type ,shell)))))
             (shell-pop index))))
      (make-shell-pop-command eshell)
      (make-shell-pop-command shell)
      (make-shell-pop-command term shell-pop-term-shell)
      (make-shell-pop-command multiterm)
      (make-shell-pop-command ansi-term shell-pop-term-shell)

      (defun ansi-term-handle-close ()
        "Close current term buffer when `exit' from term buffer."
        (when (ignore-errors (get-buffer-process (current-buffer)))
          (set-process-sentinel (get-buffer-process (current-buffer))
                                (lambda (proc change)
                                  (when (string-match "\\(finished\\|exited\\)" change)
                                    (kill-buffer (process-buffer proc))
                                    (when (> (count-windows) 1)
                                      (delete-window)))))))
      (add-hook 'term-mode-hook 'ansi-term-handle-close)
      (add-hook 'term-mode-hook (lambda () (linum-mode -1)))

      (defun spacemacs/default-pop-shell ()
        "Open the default shell in a popup."
        (interactive)
        (let ((shell (if (eq 'multi-term shell-default-shell)
                         'multiterm
                       shell-default-shell)))
          (call-interactively (intern (format "shell-pop-%S" shell)))))
      (spacemacs/set-leader-keys
        "'"   'spacemacs/default-pop-shell
        "ase" 'shell-pop-eshell
        "asi" 'shell-pop-shell
        "asm" 'shell-pop-multiterm
        "ast" 'shell-pop-ansi-term
        "asT" 'shell-pop-term))))

(defun shell/init-term ()
  (defun term-send-tab ()
    "Send tab in term mode."
    (interactive)
    (term-send-raw-string "\t"))
  ;; hack to fix pasting issue, the paste micro-state won't
  ;; work in term
  (evil-define-key 'normal term-raw-map "p" 'term-paste)
  (evil-define-key 'insert term-raw-map (kbd "C-c C-d") 'term-send-eof)
  (evil-define-key 'insert term-raw-map (kbd "C-c C-z") 'term-stop-subjob)
  (evil-define-key 'insert term-raw-map (kbd "<tab>") 'term-send-tab)

  (evil-define-key 'insert term-raw-map
    (kbd "C-k") 'term-send-up
    (kbd "C-j") 'term-send-down)
  (evil-define-key 'normal term-raw-map
    (kbd "C-k") 'term-send-up
    (kbd "C-j") 'term-send-down))

(defun shell/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-init
    (defalias 's 'magit-status)))
