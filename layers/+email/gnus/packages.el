;;; packages.el --- gnus Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
    (spacemacs/set-leader-keys "ag" 'gnus)
    :config
    (progn
      ;; No primary server
      (setq gnus-select-method '(nnnil ""))

      ;; Use topics per default
      (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

      (setq gnus-visible-headers
            "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")

      ;; Show the article headers in this order.
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

      ;; All the functions of `SPC' are bind to `quote'
      (evilified-state-evilify gnus-group-mode gnus-group-mode-map
        (kbd "f") 'gnus-group-get-new-news
        (kbd "'") 'gnus-topic-read-group)
      (evilified-state-evilify gnus-server-mode gnus-server-mode-map
        (kbd "'") 'gnus-server-read-server-in-server-buffer)
      (evilified-state-evilify gnus-browse-mode gnus-browse-mode-map                      
        (kbd "'") 'gnus-browse-read-group)
      (evilified-state-evilify gnus-article-mode gnus-article-mode-map
        (kbd "'") 'gnus-article-goto-next-page)
      (evilified-state-evilify gnus-summary-mode gnus-summary-mode-map
        (kbd "J") 'gnus-summary-next-article
        (kbd "K") 'gnus-summary-prev-article
        (kbd "'") 'gnus-summary-next-page
        (kbd "<RET>") 'spacemacs/browse-nnrss-url)

      ;; Bind Shawdowed bindings to Major mode prefix
      (dolist (prefix '(("mP" . "group/sort-select")
                        ("mS" . "group/sort")))
        (spacemacs/declare-prefix-for-mode
         'gnus-group-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'gnus-group-mode
        ;; Group
        "D" 'gnus-group-enter-directory
        "E" 'gnus-group-edit-group
        "G" 'gnus-group-make-nnir-group
        "M" 'gnus-group-read-ephemeral-group
        "R" 'gnus-group-make-rss-group
        "V" 'gnus-group-make-empty-virtual
        "c" 'gnus-group-customize
        "d" 'gnus-group-make-directory-group
        "e" 'gnus-group-edit-group-method
        "f" 'gnus-group-make-doc-group
        "h" 'gnus-group-make-help-group
        "l" 'gnus-group-nnimap-edit-acl
        "m" 'gnus-group-make-group
        "p" 'gnus-group-edit-group-parameters
        "r" 'gnus-group-rename-group
        "u" 'gnus-group-make-useful-group
        "v" 'gnus-group-add-to-virtual
        "w" 'gnus-group-make-web-group
        "x" 'gnus-group-expunge-group
        "z" 'gnus-group-compact-group
        "DEL" 'gnus-group-delete-group
        ;; Sort Selected
        "Pa" 'gnus-group-sort-selected-groups-by-alphabet
        "Pl" 'gnus-group-sort-selected-groups-by-level
        "Pm" 'gnus-group-sort-selected-groups-by-method
        "Pn" 'gnus-group-sort-selected-groups-by-real-name
        "Pr" 'gnus-group-sort-selected-groups-by-rank
        "Ps" 'gnus-group-sort-selected-groups
        "Pu" 'gnus-group-sort-selected-groups-by-unread
        "Pv" 'gnus-group-sort-selected-groups-by-score
        ;; Sort
        "Sa" 'gnus-group-sort-groups-by-alphabet
        "Sl" 'gnus-group-sort-groups-by-level
        "Sm" 'gnus-group-sort-groups-by-method
        "Sn" 'gnus-group-sort-groups-by-real-name
        "Sr" 'gnus-group-sort-groups-by-rank
        "Ss" 'gnus-group-sort-groups
        "Su" 'gnus-group-sort-groups-by-unread
        "Sv" 'gnus-group-sort-groups-by-score))))
