;;; packages.el --- Git Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(setq git-packages
      '(
        evil-collection
        fill-column-indicator
        ;; forge requires a C compiler on Windows so we disable
        ;; it by default on Windows.
        (forge :toggle (not (spacemacs/system-is-mswindows)))
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
        (magit-delta :toggle git-enable-magit-delta-plugin)
        (magit-gitflow :toggle git-enable-magit-gitflow-plugin)
        magit-section
        (magit-svn :toggle git-enable-magit-svn-plugin)
        (magit-todos :toggle git-enable-magit-todos-plugin)
        org
        (orgit :requires org)
        (orgit-forge :requires (org forge))
        smeargle
        transient))


(defun git/pre-init-golden-ratio ()
  (spacemacs|use-package-add-hook golden-ratio
    :post-config
    (add-to-list 'golden-ratio-exclude-buffer-names " *transient*")))

(defun git/pre-init-evil-collection ()
  (when (spacemacs//support-evilified-buffer-p)
    (add-to-list 'spacemacs-evil-collection-allowed-list 'magit)))

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
      (spacemacs|require-when-dumping 'magit)
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
        "gb"  'spacemacs/git-blame-transient-state/body
        "gc"  'magit-clone
        "gfF" 'magit-find-file
        "gfl" 'magit-log-buffer-file
        "gfd" 'magit-diff
        "gfm" 'magit-file-dispatch
        "gi"  'magit-init
        "gL"  'magit-list-repositories
        "gm"  'magit-dispatch
        "gs"  'magit-status
        "gS"  'magit-stage-file
        "gU"  'magit-unstage-file)
      (spacemacs|define-transient-state git-blame
        :title "Git Blame Transient State"
        :hint-is-doc t
        :dynamic-hint (spacemacs//git-blame-ts-hint)
        :on-enter (let (golden-ratio-mode)
                    (unless (bound-and-true-p magit-blame-mode)
                      (call-interactively 'magit-blame-addition)))
        :bindings
        ("?" spacemacs//git-blame-ts-toggle-hint)
        ;; chunks
        ("p" magit-blame-previous-chunk)
        ("P" magit-blame-previous-chunk-same-commit)
        ("n" magit-blame-next-chunk)
        ("N" magit-blame-next-chunk-same-commit)
        ("RET" magit-show-commit)
        ;; commits
        ("b" magit-blame-addition)
        ("r" magit-blame-removal)
        ("f" magit-blame-reverse)
        ("e" magit-blame-echo)
        ;; q closes any open blame buffers, one at a time,
        ;; closing the last blame buffer disables magit-blame-mode,
        ;; pressing q in this state closes the git blame TS
        ("q" magit-blame-quit :exit (not (bound-and-true-p magit-blame-mode)))
        ;; other
        ("c" magit-blame-cycle-style)
        ("Y" magit-blame-copy-hash)
        ("B" magit-blame :exit t)
        ("Q" nil :exit t)))
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
              (concat (kbd mm-key) (kbd mm-key)) 'with-editor-finish
              (concat (kbd mm-key) "a")    'with-editor-cancel
              (concat (kbd mm-key) "c")    'with-editor-finish
              (concat (kbd mm-key) "k")    'with-editor-cancel)
            (evil-define-key state magit-log-select-mode-map
              (concat (kbd mm-key) (kbd mm-key)) 'magit-log-select-pick
              (concat (kbd mm-key) "a")    'magit-log-select-quit
              (concat (kbd mm-key) "c")    'magit-log-select-pick
              (concat (kbd mm-key) "k")    'magit-log-select-quit))))
      ;; whitespace
      (define-key magit-status-mode-map (kbd "C-S-w")
        'spacemacs/magit-toggle-whitespace)
      ;; Add missing which-key prefixes using the new keymap api
      (when (spacemacs//support-evilified-buffer-p)
        (which-key-add-keymap-based-replacements magit-status-mode-map
          "gf"  "jump-to-unpulled"
          "gp"  "jump-to-unpushed"))
      ;; https://magit.vc/manual/magit/MacOS-Performance.html
      ;; But modified according Tommi Komulainen's advice: "...going through
      ;; shell raises an eyebrow, and in the odd edge case of not having git
      ;; setting the executable to empty string(?) feels slightly wrong."
      (when-let ((git (executable-find "git")))
        (setq magit-git-executable git))
      ;; full screen magit-status
      (when git-magit-status-fullscreen
        (setq magit-display-buffer-function
              'magit-display-buffer-fullframe-status-v1))
      (spacemacs|hide-lighter with-editor-mode)
      ;; Workaround for #12747 - org-mode
      (evil-define-key 'normal magit-blame-read-only-mode-map (kbd "RET") 'magit-show-commit)
      ;; Make sure that M-m still switch windows in all magit buffers
      (evil-define-key 'normal magit-section-mode-map (kbd "M-1") 'spacemacs/winum-select-window-1)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-2") 'spacemacs/winum-select-window-2)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-3") 'spacemacs/winum-select-window-3)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-4") 'spacemacs/winum-select-window-4)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-5") 'spacemacs/winum-select-window-5)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-6") 'spacemacs/winum-select-window-6)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-7") 'spacemacs/winum-select-window-7)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-8") 'spacemacs/winum-select-window-8)
      (evil-define-key 'normal magit-section-mode-map (kbd "M-9") 'spacemacs/winum-select-window-9))))

(defun git/init-magit-delta ()
  (use-package magit-delta
    :hook (magit-mode . magit-delta-mode)))

(defun git/init-magit-gitflow ()
  (use-package magit-gitflow
    :hook (magit-mode . magit-gitflow-mode)
    :init (setq magit-gitflow-popup-key "%")
    :config (progn
              (spacemacs|diminish magit-gitflow-mode "Flow")
              (define-key magit-mode-map "%" 'magit-gitflow-popup))))

(defun git/init-magit-section ()
  (use-package magit-section
    :defer t))

(defun git/init-magit-svn ()
  (use-package magit-svn
    :hook (magit-mode . magit-svn-mode)
    :config (progn
              (spacemacs|diminish magit-svn-mode "SVN")
              (define-key magit-mode-map "~" 'magit-svn))))

(defun git/pre-init-magit-todos ()
  (when (configuration-layer/layer-used-p 'spacemacs-evil)
    (add-to-list 'spacemacs-evil-collection-allowed-list 'magit-todos)))

(defun git/init-magit-todos ()
  (use-package magit-todos
    :hook (magit-mode . magit-todos-mode)
    :config (spacemacs|diminish magit-todos-mode "TODOS")))

(defun git/init-orgit ()
  (use-package orgit
    :defer t))

(defun git/init-orgit-forge ()
  (use-package orgit-forge
    :after forge
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

(defun git/pre-init-transient ()
  (setq transient-history-file (expand-file-name "transient/history.el"
                                                 spacemacs-cache-directory))
  (setq transient-levels-file (expand-file-name "transient/levels.el"
                                                spacemacs-cache-directory))
  (setq transient-values-file (expand-file-name "transient/values.el"
                                                spacemacs-cache-directory)))

(defun git/init-transient ()
  (use-package transient
    :defer t))

(defun git/init-forge ()
  (use-package forge
    :after magit
    :init
    (progn
      (setq forge-database-file (concat spacemacs-cache-directory
                                        "forge-database.sqlite"))
      (spacemacs/set-leader-keys-for-major-mode 'forge-topic-mode
        "c" 'forge-create-post
        "e" 'forge-edit-post)
      (spacemacs/set-leader-keys-for-major-mode 'forge-post-mode
        dotspacemacs-major-mode-leader-key 'forge-post-submit
        "c" 'forge-post-submit
        "k" 'forge-post-cancel
        "a" 'forge-post-cancel))))
