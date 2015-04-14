;;; packages.el --- gnus Layer packages File for Spacemacs
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

(setq gnus-packages '(gnus))

(defun gnus/init-gnus ()
  "Initialize my package"
  (use-package gnus
    :defer t
    :commands gnus
    :init
    (evil-leader/set-key "ag" 'gnus)
    :config
    (progn
    ;; No primary server:
    (setq gnus-select-method '(nnnil ""))

    ; Use topics per default:
    (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

    (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")

    ;;; Show the article headers in this order.
    (setq gnus-sorted-header-list
      '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
        "^Subject:" "^Date:" "^Gnus"))

    (setq-default
        gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
        gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
        gnus-group-line-format "%M%S%p%P%5y:%B %G\n";;"%B%(%g%)"
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
        gnus-sum-thread-tree-false-root ""
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-leaf-with-other "├► "
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-single-leaf "╰► "
        gnus-sum-thread-tree-vertical "│"
        gnus-article-browse-delete-temp t
        gnus-fetch-old-headers t
        gnus-treat-strip-trailing-blank-lines 'last
        gnus-keep-backlog 'nil
        gnus-summary-display-arrow nil ; Don't show that annoying arrow:
        gnus-mime-display-multipart-related-as-mixed t ; Show more MIME-stuff:
        gnus-auto-select-first nil ; Don't get the first article automatically:
        smiley-style 'medium
        gnus-keep-backlog '0)

    (require 'browse-url)
    (require 'nnrss)
    (defun spacemacs/browse-nnrss-url (arg)
      "Open RSS Article directy in the browser"
    (interactive "p")
    (let ((url (assq nnrss-url-field
                        (mail-header-extra
                        (gnus-data-header
                        (assq (gnus-summary-article-number)
                                gnus-newsgroup-data))))))
        (if url
            (progn
            (browse-url (cdr url))
            (gnus-summary-mark-as-read-forward 1))
        (gnus-summary-scroll-up arg))))
    (add-to-list 'nnmail-extra-headers nnrss-url-field)

    (evilify gnus-group-mode gnus-group-mode-map)
    (evilify gnus-server-mode gnus-server-mode-map)
    (evilify gnus-browse-mode gnus-browse-mode-map)
    (evilify gnus-article-mode gnus-article-mode-map)
    (evilify gnus-summary-mode gnus-summary-mode-map
      (kbd "J") 'gnus-summary-next-article
      (kbd "K") 'gnus-summary-prev-article
      (kbd "<RET>") 'spacemacs/browse-nnrss-url)))

  ;; org-mime is initialized here because otherwise spacemacs
  ;; complains that the org-mime package does not exist
  (use-package org-mime
    :defer t
    :commands (org-mime-htmlize
               org-mime-org-buffer-htmlize)
    :init
    (progn
      ;; setup org-mime
      (evil-leader/set-key-for-mode 'message-mode
        "mo" 'org-mime-htmlize)
      (evil-leader/set-key-for-mode 'org-mode
        "mH" 'org-mime-org-buffer-htmlize))))
