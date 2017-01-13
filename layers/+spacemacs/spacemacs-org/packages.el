;;; packages.el --- spacemacs-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <d12frosted@d12frosted.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst spacemacs-org-packages
  '(
    flyspell
    ;; default-org package does not exist, we invent this package name
    ;; to allow the `org' layer to own the `org' package instead of this
    ;; layer. So it is easier for users to steal the ownership of the
    ;; `org' package.
    (default-org-config :location built-in)
    (org-plus-contrib :step pre)
    org-bullets
    (space-doc :location local)
    toc-org
    ))

(defun spacemacs-org/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'org-mode-hook))

;; dummy init function to force installation of `org-plus-contrib'
(defun spacemacs-org/init-org-plus-contrib ())

(defun spacemacs-org/init-default-org-config ()
  (use-package org
    :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
    :defer t
    :init
    (progn
      ;; FIXME: This check has been disabled pending a resolution of
      ;; https://github.com/syl20bnr/spacemacs/issues/3933
      ;; (when (featurep 'org)
      ;;   (configuration-layer//set-error)
      ;;   (spacemacs-buffer/append
      ;;    (concat
      ;;     "Org features were loaded before the `org' layer initialized.\n"
      ;;     "Try removing org code from user initialization and private layers.") t))
      (setq org-startup-with-inline-images t
            org-src-fontify-natively t
            ;; this is consistent with the value of
            ;; `helm-org-headings-max-depth'.
            org-imenu-depth 8)
    :config
    (progn
      (font-lock-add-keywords
       'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                    (1 font-lock-comment-face prepend)
                    (2 font-lock-function-name-face)
                    (3 font-lock-comment-face prepend))))
      ;; Open links and files with RET in normal state
      (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)))))

(defun spacemacs-org/init-org-bullets ()
  (use-package org-bullets
    :defer t
    :init (add-hook 'org-mode-hook 'org-bullets-mode)))

(defun spacemacs-org/init-toc-org ()
  (use-package toc-org
    :defer t
    :init
    (progn
      (setq toc-org-max-depth 10)
      (add-hook 'org-mode-hook 'toc-org-enable))))

(defun spacemacs-org/init-space-doc ())

;;; packages.el ends here
