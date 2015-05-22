;;; packages.el --- shell packages File for Spacemacs
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

(setq shell-packages
      '(
        helm
        multi-term
        shell
        shell-pop
        term
        eshell
        eshell-prompt-extras
        esh-help
        ))

(defun shell/post-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    (progn
      (defun spacemacs//shell-helm-post-:config ()
        "Configuration to append to helm package `:config' block."
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
          (evil-leader/set-key-for-mode 'eshell-mode
            "mH" 'spacemacs/helm-eshell-history))
        (add-hook 'eshell-mode-hook 'spacemacs/init-helm-eshell)
        ;;shell
        (evil-leader/set-key-for-mode 'shell-mode
          "mH" 'spacemacs/helm-shell-history)))))

(defun shell/init-multi-term ()
  (use-package multi-term
    :defer t
    :init
    (progn
      (evil-leader/set-key "ast" 'shell-pop-multi-term)
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

      (when (configuration-layer/package-usedp 'projectile)
        (defun projectile-multi-term-in-root ()
          "Invoke `multi-term' in the project's root."
          (interactive)
          (projectile-with-default-dir (projectile-project-root) (multi-term)))
        (evil-leader/set-key "p$t" 'projectile-multi-term-in-root)))))

(defun spacemacs/init-shell ()
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
  (defun eshell/clear ()
    "Clear contents in eshell."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq pcomplete-cycle-completions nil))))

(defun shell/init-shell-pop ()
  (use-package shell-pop
    :defer t
    :init
    (progn
      (setq shell-pop-window-position shell-default-position
            shell-pop-window-height   shell-default-height
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

      (defun spacemacs//term-kill-buffer-hook ()
        "Function that hook `kill-buffer-hook'."
        (when (eq major-mode 'term-mode)
          (when (term-check-proc (current-buffer))
            (term-quit-subjob))))
      (add-hook 'kill-buffer-hook 'spacemacs//term-kill-buffer-hook)

      (defun ansi-term-handle-close ()
        "Close current term buffer when `exit' from term buffer."
        (when (ignore-errors (get-buffer-process (current-buffer)))
          (set-process-sentinel (get-buffer-process (current-buffer))
                                (lambda (proc change)
                                  (when (string-match "\\(finished\\|exited\\)" change)
                                    (kill-buffer (process-buffer proc))
                                    (delete-window))))))
      (add-hook 'term-mode-hook 'ansi-term-handle-close)

      (defun spacemacs/default-pop-shell ()
        "Open the default shell in a popup."
        (interactive)
        (let ((shell (if (eq 'multi-term shell-default-shell)
                         'multiterm
                       shell-default-shell)))
          (call-interactively (intern (format "shell-pop-%S" shell)))))
      (evil-leader/set-key
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
  (evil-define-key 'insert term-raw-map (kbd "<tab>") 'term-send-tab))

(defun shell/init-eshell ()
  (use-package eshell
    :config
    (progn
      (defalias 'e 'find-file-other-window)
      (defalias 's 'eshell/magit)
      (defalias 'd 'dired)
      (setenv "PAGER" "cat")

      (require 'em-smart)
      (setq eshell-where-to-jump 'begin
            eshell-review-quick-commands nil
            eshell-smart-space-goes-to-end t)
      (add-hook 'eshell-mode-hook 'eshell-smart-initialize)

      (use-package esh-opt
        :config
        (progn
          (use-package em-term)

          (setq eshell-cmpl-cycle-completions nil
                ;; auto truncate after 20k lines
                eshell-buffer-maximum-lines 20000
                ;; history size
                eshell-history-size 350
                ;; buffer shorthand -> echo foo > #'buffer
                eshell-buffer-shorthand t
                ;; my prompt is easy enough to see
                eshell-highlight-prompt nil
                ;; treat 'echo' like shell echo
                eshell-plain-echo-behavior t)

          ;; Visual commands
          (setq eshell-visual-commands
                (append '("ssh" "less" "top" "htop" "el" "elinks" "tmux")
                        eshell-visual-commands))

          ;; automatically truncate buffer after output
          (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)))

      (add-hook 'eshell-mode-hook
                (lambda ()
                  ;; turn off semantic-mode in eshell buffers
                  (semantic-mode -1)
                  (define-key eshell-mode-map (kbd "M-l")
                    'helm-eshell-history)))

      (defun eshell/magit ()
        "Function to open magit-status for the current directory"
        (interactive)
        (magit-status default-directory)
        nil))))

(defun shell/init-eshell-prompt-extras ()
  (use-package eshell
    :config
    (use-package esh-opt
      :config
      (use-package eshell-prompt-extras
        :init
        (setq eshell-highlight-prompt nil
              eshell-prompt-function 'epe-theme-lambda)))))

(defun shell/init-esh-help ()
  (use-package eshell
    :config
    (use-package esh-help
      :init
      (progn
        (setup-esh-help-eldoc)
        (add-hook 'eshell-mode-hook 'eldoc-mode)))))
