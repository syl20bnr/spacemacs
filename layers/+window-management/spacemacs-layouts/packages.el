;;; packages.el --- Spacemacs Layouts Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq spacemacs-layouts-packages
      '(;; temporary switch on a fork to fix
        ;; https://github.com/syl20bnr/spacemacs/issues/4120
        (persp-mode :location (recipe :fetcher github
                                      :repo "syl20bnr/persp-mode.el"
                                      :branch "fix-emacsclient-crash"))
        spaceline
        eyebrowse
        helm
        swiper))

(defun spacemacs-layouts/init-persp-mode ()
  (use-package persp-mode
    :diminish persp-mode
    :init
    (progn
      (setq persp-auto-resume-time (if (or dotspacemacs-auto-resume-layouts
                                           spacemacs-force-resume-layouts)
                                       1 -1)
            persp-nil-name dotspacemacs-default-layout-name
            persp-reset-windows-on-nil-window-conf nil
            persp-set-last-persp-for-new-frames nil
            persp-save-dir spacemacs-layouts-directory)

      ;; always activate persp-mode
      (persp-mode)

      (spacemacs/set-leader-keys "l" 'spacemacs/layouts-transient-state/body)

      (spacemacs|define-custom-layout "@Spacemacs"
        :binding "e"
        :body
        (spacemacs/find-dotfile)))
    :config
    (progn
      (defadvice persp-activate (before spacemacs//save-toggle-layout activate)
        (setq spacemacs--last-selected-layout persp-last-persp-name))
      (add-hook 'persp-mode-hook 'spacemacs//layout-autosave)

      (spacemacs/declare-prefix "b" "persp-buffers")
      (spacemacs/declare-prefix "B" "global-buffers")

      ;; Override SPC TAB to only change buffers in perspective
      (spacemacs/set-leader-keys
        "TAB"  'spacemacs/alternate-buffer-in-persp
        "ba"   'persp-add-buffer
        "br"   'persp-remove-buffer
        "Bb"   'spacemacs-layouts/non-restricted-buffer-list))))

(defun spacemacs-layouts/post-init-spaceline ()
  (setq spaceline-display-default-perspective
        dotspacemacs-display-default-layout))

(defun spacemacs-layouts/post-init-eyebrowse ()
  (add-hook 'persp-before-switch-functions #'spacemacs/update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook #'spacemacs/save-eyebrowse-for-perspective)
  (add-hook 'persp-activated-hook #'spacemacs/load-eyebrowse-for-perspective))

(defun spacemacs-layouts/post-init-helm ()
  (spacemacs/set-leader-keys
    "pl" 'spacemacs/helm-persp-switch-project))

(defun spacemacs-layouts/post-init-swiper ()
  (defun spacemacs/ivy-persp-switch-project (arg)
    (interactive "P")
    (ivy-read "Switch to Project Perspective: "
              (if (projectile-project-p)
                  (cons (abbreviate-file-name (projectile-project-root))
                        (projectile-relevant-known-projects))
                projectile-known-projects)
              :action (lambda (project)
                        (let ((persp-reset-windows-on-nil-window-conf t))
                          (persp-switch project)
                          (let ((projectile-completion-system 'ivy))
                            (projectile-switch-project-by-name project))))))

  (spacemacs/set-leader-keys
    "pl" 'spacemacs/ivy-persp-switch-project))
