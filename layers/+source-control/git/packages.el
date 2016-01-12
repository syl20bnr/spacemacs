;;; packages.el --- Git Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq git-packages
      '(
        evil-magit
        gitattributes-mode
        gitconfig-mode
        gitignore-mode
        git-commit
        git-messenger
        git-timemachine
        helm-gitignore
        magit
        magit-gitflow
        ;; not compatible with magit 2.1 at the time of release
        ;; magit-svn
        orgit
        smeargle
        ))

(defun git/init-evil-magit ()
  (unless (eq dotspacemacs-editing-style 'emacs)
    (with-eval-after-load 'magit
      (require 'evil-magit)
      (evil-define-key 'motion magit-mode-map
        (kbd dotspacemacs-leader-key) spacemacs-default-map))))

(defun git/init-helm-gitignore ()
  (use-package helm-gitignore
    :defer t
    :init (spacemacs/set-leader-keys "gI" 'helm-gitignore)))

(defun git/init-git-commit ()
  (use-package git-commit
    :defer t))

(defun git/init-git-messenger ()
  (use-package git-messenger
    :defer t
    :init
     (spacemacs/set-leader-keys
      "gm" 'git-messenger:popup-message)
    :config
    (define-key git-messenger-map [escape] 'git-messenger:popup-close)
    ))

(defun git/init-git-timemachine ()
  (use-package git-timemachine
    :defer t
    :commands spacemacs/time-machine-micro-state
    :init
    (spacemacs/set-leader-keys
      "gt" 'spacemacs/time-machine-micro-state)

    :config
    (progn

      (spacemacs|define-micro-state time-machine
        :doc "[p] [N] previous [n] next [c] current [Y] copy hash [q] quit"
        :on-enter (let (golden-ratio-mode)
                    (unless (bound-and-true-p git-timemachine-mode)
                      (call-interactively 'git-timemachine)))
        :on-exit (when (bound-and-true-p git-timemachine-mode)
                   (git-timemachine-quit))
        :persistent t
        :bindings
        ("c" git-timemachine-show-current-revision)
        ("p" git-timemachine-show-previous-revision)
        ("n" git-timemachine-show-next-revision)
        ("N" git-timemachine-show-previous-revision)
        ("Y" git-timemachine-kill-revision)
        ("q" nil :exit t)))))

(defun git/init-gitattributes-mode ()
  (use-package gitattributes-mode
    :defer t))

(defun git/init-gitconfig-mode ()
  (use-package gitconfig-mode
    :defer t))

(defun git/init-gitignore-mode ()
  (use-package gitignore-mode
    :defer t))

(defun git/init-magit ()
  (use-package magit
    :commands (magit-blame-mode
               magit-commit-popup
               magit-diff-popup
               magit-fetch-popup
               magit-log-popup
               magit-pull-popup
               magit-push-popup
               magit-status)
    :init
    (progn
      (setq magit-completing-read-function 'magit-builtin-completing-read
            magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
      (add-hook 'git-commit-mode-hook 'fci-mode)
      ;; On Windows, we must use Git GUI to enter username and password
      ;; See: https://github.com/magit/magit/wiki/FAQ#windows-cannot-push-via-https
      (when (eq window-system 'w32)
        (setenv "GIT_ASKPASS" "git-gui--askpass"))

      (defun spacemacs/magit-diff-head ()
        "Execute `magit-diff' against current HEAD."
        (interactive)
        (magit-diff "HEAD"))

      (spacemacs/declare-prefix "gd" "diff")
      (spacemacs/set-leader-keys
        "gb" 'spacemacs/git-blame-micro-state
        "gc" 'magit-commit-popup
        "gC" 'magit-checkout
        "gd" 'magit-diff-popup
        "gD" 'spacemacs/magit-diff-head
        "ge" 'magit-ediff-compare
        "gE" 'magit-ediff-show-working-tree
        "gf" 'magit-fetch-popup
        "gF" 'magit-pull-popup
        "gi" 'magit-init
        "gl" 'magit-log-popup
        "gL" 'magit-log-buffer-file
        "gP" 'magit-push-popup
        "gs" 'magit-status
        "gS" 'magit-stage-file
        "gU" 'magit-unstage-file)

      (spacemacs|define-micro-state git-blame
        :doc (concat "Press [b] again to blame further in the history, "
                     "[q] to go up or quit.")
        :on-enter (let (golden-ratio-mode)
                    (unless (bound-and-true-p magit-blame-mode)
                      (call-interactively 'magit-blame)))
        :persistent t
        :bindings
        ("b" magit-blame)
        ;; here we use the :exit keyword because we should exit the
        ;; micro-state only if the magit-blame-quit effectively disable
        ;; the magit-blame mode.
        ("q" nil :exit (progn (when (bound-and-true-p magit-blame-mode)
                                (magit-blame-quit))
                              (not (bound-and-true-p magit-blame-mode))))))
    :config
    (progn
      ;; seems to be necessary at the time of release
      (require 'git-rebase)
      ;; bind function keys
      ;; (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)
      (unless (configuration-layer/package-usedp 'evil-magit)
        ;; use auto evilification if `evil-magit' is not used
        (evilified-state-evilify-map magit-mode-map
          :bindings
          "gr" 'magit-refresh
          "gR" 'magit-refresh-all)
        (evilified-state-evilify-map magit-status-mode-map
          :mode magit-status-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-refs-mode-map
          :mode magit-refs-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-blame-mode-map
          :mode magit-blame-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-hunk-section-map
          :mode magit-status-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-diff-mode-map
          :mode magit-diff-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-log-read-revs-map
          :mode magit-log-read-revs
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-log-mode-map
          :mode magit-log-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-log-select-mode-map
          :mode magit-log-select-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-cherry-mode-map
          :mode magit-cherry-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-reflog-mode-map
          :mode magit-reflog-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-process-mode-map
          :mode magit-process-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map magit-stash-mode-map
          :mode magit-stash-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward)
        (evilified-state-evilify-map git-rebase-mode-map
          :mode git-rebase-mode
          :bindings
          (kbd "C-S-j") 'magit-section-forward
          (kbd "C-S-k") 'magit-section-backward
          (kbd "C-n") 'magit-section-forward
          (kbd "C-p") 'magit-section-backward
          "J" 'git-rebase-move-line-down
          "K" 'git-rebase-move-line-up
          "u" 'git-rebase-undo
          "y" 'git-rebase-insert)
        ;; default state for additional modes
        (dolist (mode '(magit-popup-mode
                        magit-popup-sequence-mode))
          (evil-set-initial-state mode 'emacs))
        (let ((refresh-key "gr")
              (refresh-all-key "gR")
              (delete-key (nth 0 (where-is-internal 'magit-delete-thing
                                                    magit-mode-map))))
          (evilified-state--configure-default-state 'magit-revision-mode)
          ;; section maps
          (eval `(evilified-state-evilify-map magit-tag-section-map
                   :pre-bindings
                   ,delete-key 'magit-tag-delete
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-untracked-section-map
                   :pre-bindings
                   ,delete-key 'magit-discard
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-branch-section-map
                   :pre-bindings
                   ,delete-key 'magit-branch-delete
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-remote-section-map
                   :pre-bindings
                   ,delete-key 'magit-remote-remove
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-file-section-map
                   :pre-bindings
                   ,delete-key 'magit-discard
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-hunk-section-map
                   :pre-bindings
                   ,delete-key 'magit-discard
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-unstaged-section-map
                   :pre-bindings
                   ,delete-key 'magit-discard
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-staged-section-map
                   :pre-bindings
                   ,delete-key 'magit-discard
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-commit-section-map
                   :pre-bindings
                   ,delete-key 'magit-discard
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-stashes-section-map
                   :pre-bindings
                   ,delete-key 'magit-stash-clear
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-stash-section-map
                   :pre-bindings
                   ,delete-key 'magit-stash-drop
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-module-commit-section-map
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-unpulled-section-map
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))
          (eval `(evilified-state-evilify-map magit-unpushed-section-map
                   :bindings
                   ,refresh-key 'magit-refresh
                   ,refresh-all-key 'magit-refresh-all))))

      ;; full screen magit-status
      (when git-magit-status-fullscreen
        (setq magit-display-buffer-function
              (lambda (buffer)
                (if (or
                     ;; the original should stay alive, so we can't go fullscreen
                     magit-display-buffer-noselect
                     ;; don't go fullscreen for certain magit buffers if current
                     ;; buffer is a magit buffer (we're conforming to
                     ;; `magit-display-buffer-traditional')
                     (and (derived-mode-p 'magit-mode)
                          (not (memq (with-current-buffer buffer major-mode)
                                     '(magit-process-mode
                                       magit-revision-mode
                                       magit-diff-mode
                                       magit-stash-mode
                                       magit-status-mode)))))
                    ;; open buffer according to original magit rules
                    (magit-display-buffer-traditional buffer)
                  ;; open buffer in fullscreen
                  (delete-other-windows)
                  ;; make sure the window isn't dedicated, otherwise
                  ;; `set-window-buffer' throws an error
                  (set-window-dedicated-p nil nil)
                  (set-window-buffer nil buffer)
                  ;; return buffer's window
                  (get-buffer-window buffer)))))

      (when dotspacemacs-major-mode-leader-key
        (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
        (let ((mm-key dotspacemacs-major-mode-leader-key))
          (dolist (state '(normal motion))
            (evil-define-key state with-editor-mode-map
              (concat mm-key mm-key) 'with-editor-finish
              (concat mm-key "a")    'with-editor-cancel
              (concat mm-key "c")    'with-editor-finish
              (concat mm-key "k")    'with-editor-cancel))))

      ;; whitespace
      (defun magit-toggle-whitespace ()
        (interactive)
        (if (member "-w" (if (derived-mode-p 'magit-diff-mode)
			     magit-refresh-args
			   magit-diff-section-arguments))
            (magit-dont-ignore-whitespace)
          (magit-ignore-whitespace)))
      (defun magit-ignore-whitespace ()
        (interactive)
        (add-to-list (if (derived-mode-p 'magit-diff-mode)
			 'magit-refresh-args 'magit-diff-section-arguments) "-w")
        (magit-refresh))
      (defun magit-dont-ignore-whitespace ()
        (interactive)
        (setq magit-diff-options
              (remove "-w"
                      (if (derived-mode-p 'magit-diff-mode)
                          magit-refresh-args
                        magit-diff-section-arguments))) (magit-refresh))
      (define-key magit-status-mode-map (kbd "C-S-w")
        'magit-toggle-whitespace))))

(defun git/init-magit-gitflow ()
  (use-package magit-gitflow
    :commands turn-on-magit-gitflow
    :init (progn
            (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
            (with-eval-after-load 'magit
              (define-key magit-mode-map "%" 'magit-gitflow-popup)))
    :config (spacemacs|diminish magit-gitflow-mode "Flow")))

(defun git/init-magit-svn ()
  (use-package magit-svn
    :if git-enable-magit-svn-plugin
    :commands turn-on-magit-svn
    :init (add-hook 'magit-mode-hook 'turn-on-magit-svn)
    :config
    (progn
      (evil-define-key 'emacs magit-status-mode-map
        "N" 'magit-key-mode-popup-svn))))

(defun git/init-orgit ())

(defun git/init-smeargle ()
  (use-package smeargle
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "gH" "highlight")
      (when (configuration-layer/package-usedp 'which-key)
        ;; TODO abstract this to a function
        (let ((descr
               '(("smeargle" . "highlight by last update time")
                 ("smeargle-commits" . "highlight by age of changes")
                 ("smeargle-clear" . "clear"))))
          (dolist (nd descr)
            ;; ensure the target matches the whole string
            (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
                  which-key-description-replacement-alist))))
      (spacemacs/set-leader-keys
        "gHc" 'smeargle-clear
        "gHh" 'smeargle-commits
        "gHt" 'smeargle))))
