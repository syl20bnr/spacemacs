;;; funcs.el --- Shell Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/projectile-shell-pop ()
  "Open a term buffer at projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'spacemacs/default-pop-shell)))

(defun spacemacs/disable-hl-line-mode ()
  "Locally disable global-hl-line-mode"
  (interactive)
  (setq-local global-hl-line-mode nil))

(defun spacemacs/init-eshell-xterm-color ()
  "Initialize xterm coloring for eshell"
  (setq-local xterm-color-preserve-properties t)
  (make-local-variable 'eshell-preoutput-filter-functions)
  (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq-local eshell-output-filter-functions
              (remove 'eshell-handle-ansi-color
                      eshell-output-filter-functions)))

(defun ansi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)"
                                                change)
                              (kill-buffer (process-buffer proc))
                              (when (and close-window-with-terminal
                                         (> (count-windows) 1))
                                (delete-window)))))))

(defun spacemacs/default-pop-shell ()
  "Open the default shell in a popup."
  (interactive)
  (let ((shell (cl-case shell-default-shell
                 ('multi-term 'multiterm)
                 ('shell 'inferior-shell)
                 (t shell-default-shell))))
    (call-interactively (intern (format "spacemacs/shell-pop-%S" shell)))))

(defun spacemacs/resize-shell-to-desired-width ()
  (when (and (string= (buffer-name) shell-pop-last-shell-buffer-name)
             (memq shell-pop-window-position '(left right)))
    (enlarge-window-horizontally (- (/ (* (frame-width) shell-default-width)
                                       100)
                                    (window-width)))))

(defmacro make-shell-pop-command (name func &optional shell)
  "Create a function to open a shell via the function FUNC.
SHELL is the SHELL function to use (i.e. when FUNC represents a terminal)."
  `(defun ,(intern (concat "spacemacs/shell-pop-" name)) (index)
     ,(format (concat "Toggle a popup window with `%S'.\n"
                      "Multiple shells can be opened with a numerical prefix "
                      "argument. Using the universal prefix argument will "
                      "open the shell in the current buffer instead of a "
                      "popup buffer.")
              func)
     (interactive "P")
     (require 'shell-pop)
     (if (equal '(4) index)
         ;; no popup
         (,func ,shell)
       (shell-pop--set-shell-type
        'shell-pop-shell-type
        (list ,name
              ,(if (bound-and-true-p layouts-enable-local-variables)
                   `(concat "*" (spacemacs//current-layout-name) "-"
                            (if (file-remote-p default-directory)
                                "remote-"
                              "")
                            ,name "*")
                 (concat "*" name "*"))
              (lambda nil (,func ,shell))))
       (shell-pop index)
       (spacemacs/resize-shell-to-desired-width))))

(defun projectile-multi-term-in-root ()
  "Invoke `multi-term' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root) (multi-term)))

(defun spacemacs//toggle-shell-auto-completion-based-on-path ()
  "Deactivates automatic completion on remote paths.
Retrieving completions for Eshell blocks Emacs. Over remote
connections the delay is often annoying, so it's better to let
the user activate the completion manually."
  (if (file-remote-p default-directory)
      (setq-local company-idle-delay nil)
    (setq-local company-idle-delay auto-completion-idle-delay)))

(defun spacemacs//eshell-switch-company-frontend ()
  "Sets the company frontend to `company-preview-frontend' in e-shell mode."
  (require 'company)
  (setq-local company-frontends '(company-preview-frontend)))

(defun spacemacs//eshell-auto-end ()
  "Move point to end of current prompt when switching to insert state."
  (when (and (eq major-mode 'eshell-mode)
             ;; Not on last line, we might want to edit within it.
             (not (>= (point) eshell-last-output-end))
             ;; Not on the last sent command if we use smart-eshell so we can
             ;; edit it.
             (not (and shell-enable-smart-eshell
                       (>= (point) eshell-last-input-start)
                       (< (point) eshell-last-input-end))))
    (end-of-buffer)))

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

(defun spacemacs//init-eshell ()
  "Stuff to do when enabling eshell."
  (setq pcomplete-cycle-completions nil)
  (if (bound-and-true-p linum-mode) (linum-mode -1))
  ;; autojump to prompt line if not on one already
  (add-hook 'evil-insert-state-entry-hook
            'spacemacs//eshell-auto-end nil t)
  (add-hook 'evil-hybrid-state-entry-hook
            'spacemacs//eshell-auto-end nil t)
  (when (configuration-layer/package-used-p 'semantic)
    (semantic-mode -1))
  ;; This is an eshell alias
  (defun eshell/clear ()
    (let ((inhibit-read-only t))
      (erase-buffer)))
  ;; This is a key-command
  (defun spacemacs/eshell-clear-keystroke ()
    "Allow for keystrokes to invoke eshell/clear"
    (interactive)
    (eshell/clear)
    (eshell-send-input))
  ;; Caution! this will erase buffer's content at C-l
  (define-key eshell-mode-map (kbd "C-l") 'spacemacs/eshell-clear-keystroke)
  (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof)

  ;; These don't work well in normal state
  ;; due to evil/emacs cursor incompatibility
  (evil-define-key 'insert eshell-mode-map
    (kbd "C-k") 'eshell-previous-matching-input-from-input
    (kbd "C-j") 'eshell-next-matching-input-from-input))

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
  (define-key eshell-mode-map (kbd "<tab>") 'helm-esh-pcomplete)
  (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
    "H" 'spacemacs/helm-eshell-history)
  (define-key eshell-mode-map
    (kbd "M-l") 'spacemacs/helm-eshell-history))

(defun spacemacs/ivy-eshell-history ()
  (interactive)
  (counsel-esh-history)
  (evil-insert-state))

(defun spacemacs/pcomplete-std-complete ()
  (interactive)
  (pcomplete-std-complete)
  (evil-insert-state))

(defun spacemacs/init-ivy-eshell ()
  "Initialize ivy-eshell."
  (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
    "H" #'spacemacs/ivy-eshell-history)
  (define-key eshell-mode-map (kbd "M-l") #'spacemacs/ivy-eshell-history)
  (define-key eshell-mode-map (kbd "<tab>") #'spacemacs/pcomplete-std-complete))

(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

;; Wrappers for non-standard shell commands
(defun multiterm (&optional ARG)
  "Wrapper to be able to call multi-term from shell-pop"
  (interactive)
  (multi-term))

(defun inferior-shell (&optional ARG)
  "Wrapper to open shell in current window"
  (interactive)
  (switch-to-buffer "*shell*")
  (shell "*shell*"))

;; https://stackoverflow.com/questions/6837511/automatically-disable-a-global-minor-mode-for-a-specific-major-mode
(defun spacemacs//inhibit-global-centered-cursor-mode ()
  "Counter-act `global-centered-cursor-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (centered-cursor-mode 0))
            :append
            :local))

(defun spacemacs//shell-pop-restore-window ()
  "Fixes an issue during `shell-pop-out' where it
tries to restore a dead buffer or window."
  (unless (buffer-live-p shell-pop-last-buffer)
    (setq shell-pop-last-buffer (window-buffer (get-mru-window nil t t))))
  (unless (window-live-p shell-pop-last-window)
    (setq shell-pop-last-window (get-buffer-window shell-pop-last-buffer))))

(defun spacemacs//vterm-make-history-candidates ()
  (with-temp-buffer
    (insert-file-contents spacemacs-vterm-history-file-location)
    (reverse
     (delete-dups
      (split-string (buffer-string) "\n")))))

(defun spacemacs/helm-vterm-search-history ()
  "Narrow down bash history with helm."
  (interactive)
  (assert (string-equal mode-name "VTerm") nil "Not in VTerm mode")
  (helm :sources (helm-build-sync-source "Bash history"
                                         :candidates (spacemacs//vterm-make-history-candidates)
                                         :action #'vterm-send-string)
        :buffer "*helm-bash-history*"
        :candidate-number-limit 10000))

(defun spacemacs/counsel-vterm-search-history ()
  "Narrow down bash history with ivy."
  (interactive)
  (assert (string-equal mode-name "VTerm") nil "Not in VTerm mode")
  (ivy-read "Bash history: "
            (spacemacs//vterm-make-history-candidates)
            :keymap counsel-describe-map
            :preselect (ivy-thing-at-point)
            :history 'spacemacs/counsel-shell-search-history-history
            :require-match t
            :action #'vterm-send-string
            :caller 'spacemacs/counsel-vterm-search-history))

(defun spacemacs//vterm-bind-m-r (mode-map)
  (cond
   ((configuration-layer/layer-used-p 'helm)
    (define-key mode-map (kbd "M-r") 'spacemacs/helm-vterm-search-history))
   ((configuration-layer/layer-used-p 'ivy)
    (define-key mode-map (kbd "M-r") 'spacemacs/counsel-vterm-search-history))))
