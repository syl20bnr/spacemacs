;;; packages.el --- Git Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
        fill-column-indicator
        gitattributes-mode
        gitconfig-mode
        gitignore-mode
        gitignore-templates
        git-commit
        git-link
        git-messenger
        git-timemachine
        golden-ratio
        (helm-git-grep :requires helm)
        (helm-gitignore :requires helm)
        magit
        magit-gitflow
        magit-svn
        org
        (orgit :requires org)
        smeargle
        transient
        ))

(defun git/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-names " *transient*")))

(defun git/pre-init-evil-magit ()
  (spacemacs|use-package-add-hook magit
    :post-config
    (when (spacemacs//support-evilified-buffer-p dotspacemacs-editing-style)
      (evil-magit-init))
    (evil-define-key 'motion magit-mode-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)))

(defun git/init-evil-magit ()
  (use-package evil-magit
    :defer t
    :init (add-hook 'spacemacs-editing-style-hook
                    'spacemacs//magit-evil-magit-bindings)))

(defun git/post-init-fill-column-indicator ()
  (add-hook 'git-commit-mode-hook 'fci-mode))

(defun git/init-helm-git-grep ()
  (use-package helm-git-grep
    :defer t
    :init (spacemacs/set-leader-keys
            "g/" 'helm-git-grep
            "g*" 'helm-git-grep-at-point)))

(defun git/init-helm-gitignore ()
  (use-package helm-gitignore
    :defer t
    :init (spacemacs/set-leader-keys "gI" 'helm-gitignore)))

(defun git/init-git-commit ()
  (use-package git-commit
    :defer t))

(defun git/init-git-link ()
  (use-package git-link
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "gl" "links")
      (spacemacs/set-leader-keys
        "glc" 'git-link-commit
        "glC" 'spacemacs/git-link-commit-copy-url-only
        "gll" 'git-link
        "glL" 'spacemacs/git-link-copy-url-only
        "glp" 'spacemacs/git-permalink
        "glP" 'spacemacs/git-permalink-copy-url-only)

      ;; default is to open the generated link
      (setq git-link-open-in-browser t))))

(defun git/init-git-messenger ()
  (use-package git-messenger
    :defer t
    :init (spacemacs/set-leader-keys "gM" 'git-messenger:popup-message)
    :config (define-key git-messenger-map [escape] 'git-messenger:popup-close)))

(defun git/init-git-timemachine ()
  (use-package git-timemachine
    :defer t
    :commands spacemacs/time-machine-transient-state/body
    :init
    (spacemacs/set-leader-keys
      "gt" 'spacemacs/time-machine-transient-state/body)
    :config
    (progn
      (spacemacs|define-transient-state time-machine
        :title "Git Timemachine Transient State"
        :doc "
[_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit"
        :on-enter (let (golden-ratio-mode)
                    (unless (bound-and-true-p git-timemachine-mode)
                      (call-interactively 'git-timemachine)))
        :on-exit (when (bound-and-true-p git-timemachine-mode)
                   (git-timemachine-quit))
        :foreign-keys run
        :bindings
        ("c" git-timemachine-show-current-revision)
        ("g" git-timemachine-show-nth-revision)
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

(defun git/init-gitignore-templates ()
  (use-package gitignore-templates
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'gitignore-mode
      "i" 'gitignore-templates-insert)
    (spacemacs/set-leader-keys
      "gfi" 'gitignore-templates-new-file)))

(defun git/init-magit ()
  (use-package magit
    :defer (spacemacs/defer)
    :init
    (progn
      (push "magit: .*" spacemacs-useless-buffers-regexp)
      (push "magit-.*: .*"  spacemacs-useless-buffers-regexp)
      (spacemacs|require 'magit)
      (setq magit-completing-read-function
            (if (configuration-layer/layer-used-p 'ivy)
                'ivy-completing-read
              'magit-builtin-completing-read))
      (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
      ;; On Windows, we must use Git GUI to enter username and password
      ;; See: https://github.com/magit/magit/wiki/FAQ#windows-cannot-push-via-https
      (when (eq window-system 'w32)
        (setenv "GIT_ASKPASS" "git-gui--askpass"))
      ;; key bindings
      (spacemacs/declare-prefix "gf" "file")
      (spacemacs/set-leader-keys
        "gb"  'spacemacs/git-blame-micro-state
        "gc"  'magit-clone
        "gfF" 'magit-find-file
        "gfl" 'magit-log-buffer-file
        "gfd" 'magit-diff
        "gi"  'magit-init
        "gL"  'magit-list-repositories
        "gm"  'magit-dispatch
        "gs"  'magit-status
        "gS"  'magit-stage-file
        "gU"  'magit-unstage-file)
      ;; transient state
      ;; TODO use transient state instead of old micro-state, IIRC we continue
      ;; to use micro-state because of the re-entry keyword :on-enter which is
      ;; not available in transient state
      (spacemacs|define-micro-state git-blame
        :title "Git Blame Transient State"
        :doc "
Press [_b_] again to blame further in the history, [_q_] to go up or quit."
        :on-enter (let (golden-ratio-mode)
                    (unless (bound-and-true-p magit-blame-mode)
                      (call-interactively 'magit-blame-addition)))
        :foreign-keys run
        :bindings
        ("b" magit-blame-addition)
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
      (evilified-state-evilify-map magit-repolist-mode-map
          :mode magit-repolist-mode
          :bindings
          (kbd "gr") 'magit-list-repositories
          (kbd "RET") 'magit-repolist-status)
      ;; confirm/abort
      (when dotspacemacs-major-mode-leader-key
        (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
        (let ((mm-key dotspacemacs-major-mode-leader-key))
          (dolist (state '(normal motion))
            (evil-define-key state with-editor-mode-map
              (concat mm-key mm-key) 'with-editor-finish
              (concat mm-key "a")    'with-editor-cancel
              (concat mm-key "c")    'with-editor-finish
              (concat mm-key "k")    'with-editor-cancel)
            (evil-define-key state magit-log-select-mode-map
              (concat mm-key mm-key) 'magit-log-select-pick
              (concat mm-key "a")    'magit-log-select-quit
              (concat mm-key "c")    'magit-log-select-pick
              (concat mm-key "k")    'magit-log-select-quit))))
      ;; whitespace
      (define-key magit-status-mode-map (kbd "C-S-w")
        'spacemacs/magit-toggle-whitespace)
      ;; full screen magit-status
      (when git-magit-status-fullscreen
        (setq magit-display-buffer-function
              'magit-display-buffer-fullframe-status-v1))
      (spacemacs|hide-lighter with-editor-mode)
      ;; Workaround for #12747 - org-mode
      (evil-define-key 'normal magit-blame-read-only-mode-map (kbd "RET") 'magit-show-commit))))

(defun git/init-magit-gitflow ()
  (use-package magit-gitflow
    :defer t
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
    :config
    (progn
      (spacemacs|diminish magit-gitflow-mode "Flow")
      (define-key magit-mode-map "%" 'magit-gitflow-popup))))

(defun git/init-magit-svn ()
  (use-package magit-svn
    :if git-enable-magit-svn-plugin
    :commands turn-on-magit-svn
    :init (add-hook 'magit-mode-hook 'turn-on-magit-svn)
    :config (progn
              (spacemacs|diminish magit-svn-mode "SVN")
              (define-key magit-mode-map "~" 'magit-svn))))

(defun git/init-orgit ()
  (use-package orgit
    :defer t))

(defun git/post-init-org ()
  ;; unfold the org headings for a target line
  (advice-add 'magit-blame-addition :after #'spacemacs/org-reveal-advice)
  (advice-add 'magit-diff-visit-file :after #'spacemacs/org-reveal-advice)
  (advice-add 'magit-diff-visit-worktree-file
              :after #'spacemacs/org-reveal-advice))

(defun git/init-smeargle ()
  (use-package smeargle
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "gH" "highlight")
      (when (configuration-layer/package-used-p 'which-key)
        ;; TODO abstract this to a function
        (let ((descr
               '(("smeargle" . "highlight by last update time")
                 ("smeargle-commits" . "highlight by age of changes")
                 ("smeargle-clear" . "clear"))))
          (dolist (nd descr)
            ;; ensure the target matches the whole string
            (push (cons (cons nil (concat "\\`" (car nd) "\\'"))
                        (cons nil (cdr nd)))
                  which-key-replacement-alist))))
      (spacemacs/set-leader-keys
        "gHc" 'smeargle-clear
        "gHh" 'smeargle-commits
        "gHt" 'smeargle))))

(defun git/init-transient ()
  (use-package transient
    :defer t
    :init
    (setq
     transient-levels-file
     (expand-file-name "transient/levels.el" spacemacs-cache-directory)
     transient-values-file
     (expand-file-name "transient/values.el" spacemacs-cache-directory)
     transient-history-file
     (expand-file-name "transient/history.el" spacemacs-cache-directory))))
