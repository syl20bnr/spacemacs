#!/usr/bin/emacs --script
;;; install.el --- Dash layer dependencies installation script
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(load "/usr/local/spacemacs/lib/deps-install-helpers.el" nil t)

(defconst docsets '(
                    (clojure . ("http://newyork.kapeli.com/feeds/Clojure.tgz"))
                    (go . ("http://london.kapeli.com/feeds/Go.tgz"))
                    (html . ("http://sanfrancisco.kapeli.com/feeds/HTML.tgz"
                             "http://sanfrancisco.kapeli.com/feeds/Sass.tgz"
                             "http://london.kapeli.com/feeds/CSS.tgz"))
;;;;;;;;;;;;;;;;;;; FIXME: https://github.com/jinzhu/zeal-at-point/issues/28
                    (emacs-lisp . ("http://sanfrancisco.kapeli.com/feeds/Emacs_Lisp.tgz"))
                    (javascript . ("http://sanfrancisco.kapeli.com/feeds/JavaScript.tgz"))
                    (docker . ("http://frankfurt.kapeli.com/feeds/Docker.tgz"))
                    (markdown . ("http://sanfrancisco.kapeli.com/feeds/Markdown.tgz"))
                    ;; FIXME: ADD MORE!
                    )
  "Zeal doc-set alist of shape (layer . (URL URL URL..)) Source: https://zealdocs.org/")

;;NOTE: Yes it's Zeal/Zeal for some reason(probably a bug).
(defconst docset-dir (format "%s.local/share/Zeal/Zeal/docsets/"
                             (dir (get-glob-env "UHOME"))))

(checkpoint
  (message (concat "Spacemacs'es docset list provides docs for: "
                   (mapconcat (lambda (docset) (symbol-name (car docset))) docsets " ")
                   " layers.\n Open a PR/issue if you need more. Docsets available at:"
                   " https://zealdocs.org/"))
  (with-installed (curl tar gzip software-properties-common)
    ($ "add-apt-repository ppa:zeal-developers/ppa"
       "apt-get update")
    (mkdirp docset-dir)
    (install zeal)
    (when (or (dotfile-has-symbol-p 'helm-dash-docset-newpath)
              (dotfile-has-symbol-p 'helm-dash-docsets-path))
      (message "Setting helm-dash docset path to \"%s\" (for Zeal compatibility)"
               docset-dir))
    (append-to-user-config "helm-dash docsets path (default for Zeal app)."
                           (format "(setq helm-dash-docset-newpath \"%s\")"
                                   docset-dir))
    (unless (dotfile-has-symbol-p 'helm-dash-browser-func)
      (append-to-user-config "Open helm-dash in eww."
                             "(setq helm-dash-browser-func 'eww)"))
    (dolist (docset docsets)
      (when (and (layer-used-p (car docset))
                 (layer-installer-not-excluded-p (car docset)))
        (dolist (url (cdr docset))
          ($ `("curl %s | tar xz -C %s" ,url ,docset-dir)))
        ($ `("chown ${UID}:${GID} -R %s" ,docset-dir)))))
  (install sqlite3))
