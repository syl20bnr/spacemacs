;;; packages.el --- eww Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq eww-packages
    '(
      eww
      eww-lnum
      ))

;; List of packages to exclude.
(setq eww-excluded-packages '())

;; For each package, define a function eww/init-<package-name>
;;
;; (defun eww/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun eww/init-eww ()
  (use-package eww
    :defer t
    :init
    (custom-set-variables
     `(eww-search-prefix ,(cdr (assoc default-search-engine eww--search-engines))))
    (evil-leader/set-key "aw" 'eww)
    :config
    ;; needed to populate eww-bookmarks
    (when eww-bookmarks
      (eww-bookmark-prepare))

    (defun get-title (bookmark)
      (plist-get bookmark :title))

    (defun has-title (title)
      (lambda (el)
        (equal (plist-get el :title) title)))

    (defun get-url (title bookmarks)
      (plist-get
       (car (remove-if-not (has-title title) bookmarks))
       :url))

    (defun open-from-bookmarks ()
      (lambda (title)
        (eww-browse-url (get-url title eww-bookmarks))))

    (defun delete-from-bookmarks ()
      (lambda (title)
        (setq eww-bookmarks (remove-if (has-title title) eww-bookmarks))
        (eww-write-bookmarks)
        (message "bookmark deleted")))

    (defun helm-source-eww-bookmarks ()
      `((name . "Bookmarks")
        (candidates . ,(mapcar #'get-title eww-bookmarks))
        (action . (("open url" . ,(open-from-bookmarks))
                   ("delete bookmark" . ,(delete-from-bookmarks))))))

    (defun helm-eww-bookmarks ()
      (interactive)
      (helm :sources (helm-source-eww-bookmarks)
            :buffer "*helm-eww-bookmarks*"))

    (evilified-state-evilify-map eww-mode-map
      :mode eww-mode
      :bindings
      "l" 'evil-forward-char
      "i" 'evil-insert
      "H" 'eww-back-url
      "L" 'eww-forward-url
      "f" 'eww-lnum-follow ;;ace-link-eww
      "F" 'eww-lnum-universal
      "o" 'eww
      "Y" 'eww-copy-page-url
      ;; "p" TODO: open url on clipboard
      "r" 'eww-reload
      "b" 'helm-eww-bookmarks
      "a" 'eww-add-bookmark)))

(defun eww/init-eww-lnum ()
  (use-package eww-lnum
    :defer t))
