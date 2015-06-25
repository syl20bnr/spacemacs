;;; extensions.el --- Git Layer Extensions File for Spacemacs
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

(setq git-post-extensions '())

(when git-use-magit-next
  (setq git-post-extensions '(magit-next)))

(defun git/init-magit-next ()
  (use-package magit
    :if git-use-magit-next
    :commands (magit-status
               magit-blame-mode
               magit-log
               magit-commit)
    :init
    (progn
      (add-to-list 'load-path (format "%smagit-next/lisp/"
                                      (configuration-layer/get-layer-property
                                       'git :ext-dir)))
      (setq magit-last-seen-setup-instructions "1.4.0"
            magit-completing-read-function 'magit-ido-completing-read)
      (add-hook 'git-commit-mode-hook 'fci-mode)
      ;; must enable auto-fill-mode again because somehow fci-mode disable it
      (add-hook 'git-commit-mode-hook 'auto-fill-mode)
      ;; On Windows, we must use Git GUI to enter username and password
      ;; See: https://github.com/magit/magit/wiki/FAQ#windows-cannot-push-via-https
      (when (eq window-system 'w32)
        (setenv "GIT_ASKPASS" "git-gui--askpass"))

      (defun spacemacs/magit-diff-head ()
        "Execute `magit-diff' against current HEAD."
        (interactive)
        (magit-diff "HEAD"))

      (evil-leader/set-key
        "gb" 'magit-blame-mode
        "gl" 'magit-log
        "gs" 'magit-status
        "gd" 'spacemacs/magit-diff-head
        "gC" 'magit-commit)
      (evilify magit-commit-mode magit-commit-mode-map
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-log-mode magit-log-mode-map
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-process-mode magit-process-mode-map
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-branch-manager-mode magit-branch-manager-mode-map
               "K" 'magit-discard-item
               "L" 'magit-key-mode-popup-logging
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-status-mode magit-status-mode-map
               "K" 'magit-discard-item
               "L" 'magit-key-mode-popup-logging
               "H" 'magit-key-mode-popup-diff-options
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item)
      (evilify magit-diff-mode magit-diff-mode-map
               "K" 'magit-discard-item
               "L" 'magit-key-mode-popup-logging
               "H" 'magit-key-mode-popup-diff-options
               (kbd "C-j") 'magit-goto-next-section
               (kbd "C-k") 'magit-goto-previous-section
               (kbd "C-n") 'magit-goto-next-section
               (kbd "C-p") 'magit-goto-previous-section
               (kbd "C-v") 'magit-revert-item))
    :config
    (progn
      ;; (spacemacs|hide-lighter magit-auto-revert-mode)
      (defun spacemacs//evilify-magit-section-map (symbol-map)
        "Evilify the magit section map."
        (let ((current-map symbol-map)
              (evilified-keys (mapcar 'car (cdr evil-evilified-state-map))))
          (map-keymap 'spacemacs//remap-magit-section-event
                      (symbol-value symbol-map))))

      (defun spacemacs//remap-magit-section-event (event value)
        "Remap event if it corresponds to an event of evilified state."
        (when (and (characterp event)
                   (member event evilified-keys)
                   (not (string-match "spacemacs" (symbol-name value))))
          ;; keep this debug message around for now
          (message "preparing to evilify: %s|%s (%s)"
                   (char-to-string event) value current-map)
          (eval `(define-key ,current-map [remap ,value]
                   ',(spacemacs//wrap-magit-command event value)))
          ;; wrapper is necessary because we remap value so we need
          ;; a new function name (may work with aliases ?)
          (let ((wrapper `(lambda ()
                            (interactive)
                            (call-interactively ',value))))
            (cond
             ((char-equal ?k event)
              (define-key (symbol-value current-map) "K" wrapper))
             ((char-equal ?v event)
              (define-key (symbol-value current-map) (kbd "C-v") wrapper))))))

      (defun spacemacs//wrap-magit-command (event value)
        "Wrap VALUE in a function which takes care of the evilified state."
        (let ((wrapper-func (intern (format "spacemacs/evilified-%s" value))))
          (eval
           `(defun ,wrapper-func ()
              ,(format (concat "Wrap command %s to support evilified state.\n"
                               "If the current state is `evilified' then "
                               "execute the evilified command, otherwise "
                               "execute the original command.") value)
              (interactive)
              (if (eq 'evilified evil-state)
                  (progn
                    (call-interactively ',(lookup-key evil-evilified-state-map
                                                      (char-to-string event))))
                (call-interactively ',value))))
          wrapper-func))

      ;; evilify all the magit section maps
      (spacemacs//evilify-magit-section-map 'magit-tag-section-map)
      (spacemacs//evilify-magit-section-map 'magit-untracked-section-map)
      (spacemacs//evilify-magit-section-map 'magit-branch-section-map)
      (spacemacs//evilify-magit-section-map 'magit-remote-section-map)
      (spacemacs//evilify-magit-section-map 'magit-file-section-map)
      (spacemacs//evilify-magit-section-map 'magit-hunk-section-map)
      (spacemacs//evilify-magit-section-map 'magit-unstaged-section-map)
      (spacemacs//evilify-magit-section-map 'magit-staged-section-map)
      (spacemacs//evilify-magit-section-map 'magit-commit-section-map)
      (spacemacs//evilify-magit-section-map 'magit-module-commit-section-map)
      (spacemacs//evilify-magit-section-map 'magit-unpulled-section-map)
      (spacemacs//evilify-magit-section-map 'magit-unpushed-section-map)
      (spacemacs//evilify-magit-section-map 'magit-stashes-section-map)
      (spacemacs//evilify-magit-section-map 'magit-stash-section-map)

      ;; the following comments are remaing maps to evilify

;; (defvar magit-status-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map magit-mode-map)
;;     (define-key map "jz" 'magit-jump-to-stashes)
;;     (define-key map "jt" 'magit-jump-to-tracked)
;;     (define-key map "jn" 'magit-jump-to-untracked)
;;     (define-key map "ju" 'magit-jump-to-unstaged)
;;     (define-key map "js" 'magit-jump-to-staged)
;;     (define-key map "jf" 'magit-jump-to-unpulled)
;;     (define-key map "jp" 'magit-jump-to-unpushed)
;;     map)
;;   "Keymap for `magit-status-mode'.")

;; (defvar magit-refs-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map magit-mode-map)
;;     (define-key map "\C-y" 'magit-refs-set-show-commit-count)
;;     map)
;;   "Keymap for `magit-refs-mode'.")

;; (defvar git-rebase-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map special-mode-map)
;;     (define-key map [remap undo] 'git-rebase-undo)
;;     (define-key map (kbd "RET") 'git-rebase-show-commit)
;;     (define-key map (kbd "x")   'git-rebase-exec)
;;     (define-key map (kbd "c")   'git-rebase-pick)
;;     (define-key map (kbd "r")   'git-rebase-reword)
;;     (define-key map (kbd "e")   'git-rebase-edit)
;;     (define-key map (kbd "s")   'git-rebase-squash)
;;     (define-key map (kbd "f")   'git-rebase-fixup)
;;     (define-key map (kbd "y")   'git-rebase-insert)
;;     (define-key map (kbd "k")   'git-rebase-kill-line)
;;     (define-key map (kbd "C-k") 'git-rebase-kill-line)
;;     (define-key map (kbd "p")   'git-rebase-backward-line)
;;     (define-key map (kbd "n")   'forward-line)
;;     (define-key map (kbd "M-p")      'git-rebase-move-line-up)
;;     (define-key map (kbd "M-n")      'git-rebase-move-line-down)
;;     (define-key map (kbd "M-<up>")   'git-rebase-move-line-up)
;;     (define-key map (kbd "M-<down>") 'git-rebase-move-line-down)
;;     (define-key map (kbd "C-x C-t")  'git-rebase-move-line-up)
;;     map)
;;   "Keymap for Git-Rebase mode.")

;; (defvar git-commit-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-c C-s") 'git-commit-signoff)
;;     (define-key map (kbd "C-c C-a") 'git-commit-ack)
;;     (define-key map (kbd "C-c C-t") 'git-commit-test)
;;     (define-key map (kbd "C-c C-r") 'git-commit-review)
;;     (define-key map (kbd "C-c C-o") 'git-commit-cc)
;;     (define-key map (kbd "C-c C-p") 'git-commit-reported)
;;     (define-key map (kbd "C-c C-i") 'git-commit-suggested)
;;     (define-key map (kbd "C-c M-s") 'git-commit-save-message)
;;     (define-key map (kbd "M-p")     'git-commit-prev-message)
;;     (define-key map (kbd "M-n")     'git-commit-next-message)
;;     ;; Old bindings to avoid confusion
;;     (define-key map (kbd "C-c C-x s") 'git-commit-signoff)
;;     (define-key map (kbd "C-c C-x a") 'git-commit-ack)
;;     (define-key map (kbd "C-c C-x t") 'git-commit-test)
;;     (define-key map (kbd "C-c C-x r") 'git-commit-review)
;;     (define-key map (kbd "C-c C-x o") 'git-commit-cc)
;;     (define-key map (kbd "C-c C-x p") 'git-commit-reported)
;;     map)
;;   "Key map used by `git-commit-mode'.")

;; (defvar magit-blame-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\r" 'magit-show-commit)
;;     (define-key map "\s" 'magit-diff-show-or-scroll-up)
;;     (define-key map "\d" 'magit-diff-show-or-scroll-down)
;;     (define-key map "b"  'magit-blame-popup)
;;     (define-key map "n"  'magit-blame-next-chunk)
;;     (define-key map "N"  'magit-blame-next-chunk-same-commit)
;;     (define-key map "p"  'magit-blame-previous-chunk)
;;     (define-key map "P"  'magit-blame-previous-chunk-same-commit)
;;     (define-key map "q"  'magit-blame-quit)
;;     (define-key map "t"  'magit-blame-toggle-headings)
;;     map)
;;   "Keymap for `magit-blame-mode'.")

;; (defvar magit-diff-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map magit-mode-map)
;;     (define-key map "\C-c\C-d" 'magit-diff-while-committing)
;;     (define-key map "\C-c\C-b" 'magit-go-backward)
;;     (define-key map "\C-c\C-f" 'magit-go-forward)
;;     (define-key map "\s" 'scroll-up)
;;     (define-key map "\d" 'scroll-down)
;;     (define-key map "j" 'magit-jump-to-diffstat-or-diff)
;;     map)
;;   "Keymap for `magit-diff-mode'.")

;; (defvar magit-log-read-revs-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map crm-local-completion-map)
;;     (define-key map "\s" 'self-insert-command)
;;     map))

;; (defvar magit-log-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map magit-mode-map)
;;     (define-key map "\C-c\C-b" 'magit-go-backward)
;;     (define-key map "\C-c\C-f" 'magit-go-forward)
;;     (define-key map "+" 'magit-log-show-more-commits)
;;     (define-key map "q" 'magit-log-bury-buffer)
;;     map)
;;   "Keymap for `magit-log-mode'.")

;; (defvar magit-log-select-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map magit-log-mode-map)
;;     (define-key map "\C-c\C-b" 'undefined)
;;     (define-key map "\C-c\C-f" 'undefined)
;;     (define-key map "."        'magit-log-select-pick)
;;     (define-key map "e"        'magit-log-select-pick)
;;     (define-key map "\C-c\C-c" 'magit-log-select-pick)
;;     (define-key map "q"        'magit-log-select-quit)
;;     (define-key map "\C-c\C-k" 'magit-log-select-quit)
;;     map)
;;   "Keymap for `magit-log-select-mode'.")

;; (defvar magit-cherry-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map magit-mode-map)
;;     map)
;;   "Keymap for `magit-cherry-mode'.")

;; (defvar magit-reflog-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map magit-log-mode-map)
;;     map)
;;   "Keymap for `magit-reflog-mode'.")

;; (defvar magit-popup-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [remap self-insert-command] 'magit-invoke-popup-action)
;;     (define-key map [?- t]        'magit-invoke-popup-switch)
;;     (define-key map [?= t]        'magit-invoke-popup-option)
;;     (define-key map [?\C-c ?\C-c] 'magit-popup-set-default-arguments)
;;     (define-key map [?\C-x ?\C-s] 'magit-popup-save-default-arguments)
;;     (define-key map [?\C-g]       'magit-popup-quit)
;;     (define-key map [??]          'magit-popup-help)
;;     (define-key map [?\C-h ?i]    'magit-popup-info)
;;     (define-key map [?\C-t]       'magit-popup-toggle-show-common-commands)
;;     (define-key map [?\d]         'backward-button)
;;     (define-key map [?\C-p]       'backward-button)
;;     (define-key map [?\t]         'forward-button)
;;     (define-key map [?\C-n]       'forward-button)
;;     (define-key map [?\r]         'push-button)
;;     map)
;;   "Keymap for `magit-popup-mode'.

;; \\<magit-popup-mode-map>\
;; This keymap contains bindings common to all popups.  A section
;; listing these commands can be shown or hidden using \
;; \\[magit-popup-toggle-show-common-commands].

;; The prefix used to toggle any switch can be changed by binding
;; another key to `magit-invoke-popup-switch'.  Likewise binding
;; another key to `magit-invoke-popup-option' changes the prefixed
;; used to set any option.  The two prefixes have to be different.
;; If you change these bindings you should also change the `prefix'
;; property of the button types `magit-popup-switch-button' and
;; `magit-popup-option-button'.

;; If you change any other binding, then you might have to also edit
;; `magit-popup-common-commands' for things to align correctly in
;; the section listing these commands.

;; Never bind an alphabetic character in this keymap or you might
;; make it impossible to invoke certain actions.")

;; (defvar magit-process-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map magit-mode-map)
;;     map)
;;   "Keymap for `magit-process-mode'.")

;; (defvar with-editor-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\C-c\C-c"                   'with-editor-finish)
;;     (define-key map [remap server-edit]          'with-editor-finish)
;;     (define-key map "\C-c\C-k"                   'with-editor-cancel)
;;     (define-key map [remap kill-buffer]          'with-editor-cancel)
;;     (define-key map [remap ido-kill-buffer]      'with-editor-cancel)
;;     (define-key map [remap iswitchb-kill-buffer] 'with-editor-cancel)
;;     map))

      ;; full screen magit-status
      (when git-magit-status-fullscreen
        (defadvice magit-status (around magit-fullscreen activate)
          (window-configuration-to-register :magit-fullscreen)
          ad-do-it
          (delete-other-windows))

        (defun magit-quit-session ()
          "Restores the previous window configuration and kills the magit buffer"
          (interactive)
          (kill-buffer)
          (jump-to-register :magit-fullscreen))
        (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

      (defun magit-toggle-whitespace ()
        (interactive)
        (if (member "-w" magit-diff-options)
            (magit-dont-ignore-whitespace)
          (magit-ignore-whitespace)))

      (defun magit-ignore-whitespace ()
        (interactive)
        (add-to-list 'magit-diff-options "-w")
        (magit-refresh))

      (defun magit-dont-ignore-whitespace ()
        (interactive)
        (setq magit-diff-options (remove "-w" magit-diff-options))
        (magit-refresh))
      (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))))
