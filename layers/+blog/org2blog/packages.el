;;; packages.el --- org2blog layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Christian E. Hopps <chopps@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; Org2blog layer.

;;; Code:

(defconst org2blog-packages
  '((org2blog :location (recipe :fetcher github :repo "punchagan/org2blog"))
     org
     metaweblog
     xml-rpc))

(defun org2blog/init-xml-rpc ()
  (use-package xml-rpc))

(defun org2blog/init-metaweblog ()
  (use-package metaweblog))

(defun org2blog/init-org2blog ()
  (use-package org2blog
    :commands (org2blog/wp-mode org2blog/wp-new-entry org2blog/wp-login)
    :init
      (spacemacs/set-leader-keys "ob" 'org2blog/wp-new-entry)
    :config
    (progn
      ;; Map emacs languages to HTML recognized ones.
      (setq org2blog/wp-shortcode-langs-map '(("emacs-lisp" . "lisp") ("sh" . "bash")))

      (if org2blog-name
        (setq org2blog/wp-blog-alist
          `((,org2blog-name
              :url ,(concat "http://" org2blog-name "/xmlrpc.php")
              :username ,(car (auth-source-user-and-password org2blog-name))
              :password ,(cadr (auth-source-user-and-password org2blog-name)))))))))

(defun org2blog/post-init-org ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "Bx" 'org2blog/wp-delete-entry
    "BX" 'org2blog/wp-delete-page
    "Bl" 'org2blog/wp-login
    ;; preview ('n' unixy dry-run)
    "Bn" 'org2blog/wp-preview-buffer-post
    "BN" 'org2blog/wp-preview-subtree-post
    ;; post, capitalize for -and-publish
    "Bb" 'org2blog/wp-post-buffer
    "Bp" 'org2blog/wp-post-buffer-as-page
    "Bs" 'org2blog/wp-post-subtree

    "BB" 'org2blog/wp-post-buffer-and-publish ;; need prefix key
    "BP" 'org2blog/wp-post-buffer-as-page-and-publish ;; need prefix key
    "BS" 'org2blog/wp-post-subtree-as-page-and-publish
    ;; "bpS" 'org2blog/wp-post-subtree-as-page
    )
)
;;; packages.el ends here
