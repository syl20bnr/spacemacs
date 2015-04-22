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

(defvar org-packages
  '(
    evil-org
    org
    org-bullets
    org-pomodoro
    org-repo-todo
    ox-gfm
    org2blog
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar org-excluded-packages
  '(
    ;; seems to be problematic, to investigate
    ox-gfm
    )
  "List of packages to exclude.")

(defun org/init-evil-org ()
  (use-package evil-org
    :commands evil-org-mode
    :init
    (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'org-mode
           "a" nil "ma" 'org-agenda
           "c" nil "mA" 'org-archive-subtree
           "o" nil "mC" 'evil-org-recompute-clocks
           "l" nil "ml" 'evil-org-open-links
           "t" nil "mt" 'org-show-todo-tree)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

(defun org/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :defer t
    :init
    (progn
      (setq org-log-done t)

      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))
      (add-hook 'org-mode-hook 'org-indent-mode)

      (evil-leader/set-key-for-mode 'org-mode
        "mc" 'org-capture
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mf" 'org-set-effort
        "mi" 'org-clock-in
        "mo" 'org-clock-out
        "mm" 'org-ctrl-c-ctrl-c
        "mq" 'org-clock-cancel
        "mr" 'org-refile
        "ms" 'org-schedule)

      (eval-after-load "org-agenda"
        '(progn
           (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
           (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
           (define-key org-agenda-mode-map
             (kbd "SPC") evil-leader--default-map))))
    :config
    (progn
      (require 'org-indent)
      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda))))

(defun org/init-org-bullets ()
  (use-package org-bullets
    :defer t
    :init (add-hook 'org-mode-hook 'org-bullets-mode)))

(defun org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (evil-leader/set-key-for-mode 'org-mode
        "mp" 'org-pomodoro))))

(defun org/init-org-repo-todo ()
  (use-package org-repo-todo
    :commands (ort/capture-todo
               ort/capture-todo-check
               ort/goto-todos)
    :init
    (progn
      (evil-leader/set-key
        "Ct"  'ort/capture-todo
        "CT"  'ort/capture-todo-check)
      (evil-leader/set-key-for-mode 'org-mode
        "mgt" 'ort/goto-todos))))

(defun org/init-ox-gfm ()
  (use-package ox-gfm
    :defer t))

(defun org/init-org2blog()
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
