;;; packages.el --- gnus Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst gnus-packages
  '(
    gnus
    window-purpose
    persp-mode))

(defun gnus/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      (spacemacs|define-custom-layout gnus-spacemacs-layout-name
        :binding gnus-spacemacs-layout-binding
        :body
        (call-interactively 'gnus)))))

(defun gnus/init-gnus ()
  "Initialize my package"
  (use-package gnus
    :defer t
    :commands gnus
    :init
    (spacemacs/declare-prefix "aeg" "gnus")
    (spacemacs/set-leader-keys
      "aegg" 'gnus
      "aegs" 'gnus-slave
      "aegu" 'gnus-unplugged
      "aego" 'gnus-slave-unplugged)
    (spacemacs/declare-prefix-for-mode 'message-mode "mi" "insert")
    (spacemacs/set-leader-keys-for-major-mode 'message-mode
      ;; RFC 1855
      "miF" 'flame-on)
    ;; NOTE: If any of the following variables are modified,
    ;; also update their values in: `gnus/README.org'
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
    :config
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

    (require 'browse-url)
    (require 'nnrss)

    (defun spacemacs/gnus-flame-on ()
      "Most important email function, for RFC1855 compliance."
      ;; https://tools.ietf.org/html/rfc1855
      (interactive)
      (insert "FLAME ON:\n")
      (insert "FLAME OFF\n")
      (forward-line -2)
      (end-of-line))

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

    ;; Use the more modern macro to enable shadowing
    ;; Make sure this is only run after the keymaps
    ;; have been loaded.
    (with-eval-after-load 'gnus-group
      (evilified-state-evilify-map gnus-group-mode-map
        :mode gnus-group-mode
        :bindings
        (kbd "g r") 'gnus-group-get-new-news
        (kbd "O") 'gnus-group-group-map))
    (with-eval-after-load 'gnus-srvr
      (evilified-state-evilify-map gnus-server-mode-map
        :mode gnus-server-mode)
      (evilified-state-evilify-map gnus-browse-mode-map
        :mode gnus-browse-mode))
    (with-eval-after-load 'gnus-art
      (evilified-state-evilify-map gnus-article-mode-map
        :mode gnus-article-mode))
    (with-eval-after-load 'gnus-sum
      (evilified-state-evilify-map gnus-summary-mode-map
        :mode gnus-summary-mode
        :bindings
        (kbd "J") 'gnus-summary-next-article
        (kbd "K") 'gnus-summary-prev-article
        (kbd "<RET>") 'spacemacs/browse-nnrss-url))))

(defun gnus/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :gnus-layer
   (purpose-conf :mode-purposes '((gnus-group-mode . mail)
                                  (gnus-server-mode . mail)
                                  (gnus-browse-mode . mail)
                                  (gnus-article-mode . mail)
                                  (gnus-summary-mode . mail)))))
