;;; funcs.el --- Colors Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; magit

(defun spacemacs/magit-toggle-whitespace ()
  "Toggle whitespace in `magit-diff-mode'."
  (interactive)
  (if (member "-w" (if (derived-mode-p 'magit-diff-mode)
                       magit-refresh-args
                     magit-diff-section-arguments))
      (spacemacs//magit-dont-ignore-whitespace)
    (spacemacs//magit-ignore-whitespace)))

(defun spacemacs//magit-ignore-whitespace ()
  "Ignore whitespace in `magit-diff-mode'"
  (add-to-list (if (derived-mode-p 'magit-diff-mode)
                   'magit-refresh-args 'magit-diff-section-arguments) "-w")
  (magit-refresh))

(defun spacemacs//magit-dont-ignore-whitespace ()
  "Don't ignore whitespace in `magit-diff-mode'"
  (setq magit-diff-options
        (remove "-w"
                (if (derived-mode-p 'magit-diff-mode)
                    magit-refresh-args
                  magit-diff-section-arguments))) (magit-refresh))

(defun spacemacs/git-link-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'spacemacs/git-link)))

(defun spacemacs/git-link-commit-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'spacemacs/git-link-commit)))

(defun spacemacs/git-link ()
  "Allow the user to run git-link in a git-timemachine buffer."
  (interactive)
  (require 'git-link)
  (if (and (boundp 'git-timemachine-revision)
           git-timemachine-revision)
      (cl-letf (((symbol-function 'git-link--branch)
                 (lambda ()
                   (car git-timemachine-revision))))
        (call-interactively 'git-link))
    (call-interactively 'git-link)))

(defun spacemacs/git-link-commit ()
  "Allow the user to run git-link-commit in a git-timemachine buffer."
  (interactive)
  (require 'git-link)
  (if (and (boundp 'git-timemachine-revision)
           git-timemachine-revision)
      (cl-letf (((symbol-function 'word-at-point)
                 (lambda ()
                   (car git-timemachine-revision))))
        (call-interactively 'git-link-commit))
    (call-interactively 'git-link-commit)))


(defun spacemacs//support-evilified-buffer-p (style)
  "Return non-nil if evil navigation should be enabled for STYLE."
  (or (eq style 'vim)
      (and (eq style 'hybrid)
           hybrid-style-enable-evilified-state)))

(defun spacemacs//magit-evil-magit-bindings (style)
  "Set `evil-magit' bindings for the given editing STYLE."
  (cond
   ((spacemacs//support-evilified-buffer-p style)
    (evil-magit-init))
   (t
    (when (featurep 'evil-magit)
      (evil-magit-revert)))))
