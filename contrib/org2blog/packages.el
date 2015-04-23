;;; packages.el --- Org Layer packages File for Spacemacs
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

(defvar org2blog-packages
  '(
    org2blog
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun org2blog/init-org2blog()
  (use-package org2blog
    :init
    (progn
      (evil-leader/set-key
        "aoi"   'org2blog/wp-login
        "aob"   'org2blog/wp-post-buffer
        "aop"   'org2blog/wp-post-buffer-as-page
        "aoo"   'org2blog/wp-logout
        ))
    :config
    (progn
      (require 'org2blog-autoloads)
      (setq org2blog/wp-blog-alist
            '(("wordpress"
               :url "http://hujianxin.com/xmlrpc.php"
               :username "hujianxin"
               :default-title "Emacs Post"
               :default-categories ("Tecnology")
               :tags-as-categories nil)
              ("localhost"
               :url "http://localhost/wordpress/xmlrpc.php")))
      (setq org2blog/wp-use-sourcecode-shortcode 't)
      ;; removed light="true"
      (setq org2blog/wp-sourcecode-default-params nil)
      ;; target language needs to be in here
      (setq org2blog/wp-sourcecode-langs
            '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
              "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
              "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
              "vb" "xml" "sh" "emacs-lisp" "lisp" "lua"))
      ;; this will use emacs syntax higlighting in your #+BEGIN_SRC
      ;; <language> <your-code> #+END_SRC code blocks.
    (setq org-src-fontify-natively t))
    )
)
