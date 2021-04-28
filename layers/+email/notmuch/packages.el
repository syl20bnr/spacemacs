;;; packages.el --- Notmuch Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


(defconst notmuch-packages
  '(
    (counsel-notmuch :requires ivy)
    (helm-notmuch :requires helm)
    notmuch
    org
    persp-mode
    window-purpose))


(defun notmuch/init-counsel-notmuch ()
  (use-package counsel-notmuch
    :defer t
    :init (spacemacs/set-leader-keys "aenn" 'counsel-notmuch)))

(defun notmuch/init-helm-notmuch ()
  (use-package helm-notmuch
    :defer t
    :init (spacemacs/set-leader-keys "aenn" 'helm-notmuch)))

(defun notmuch/init-notmuch ()
  (use-package notmuch
    :defer t
    :commands notmuch
    :init
    (progn
      (spacemacs/declare-prefix "aen" "notmuch")
      (spacemacs/set-leader-keys
        "aenN" 'notmuch
        "aeni" 'spacemacs/notmuch-inbox
        "aenj" 'notmuch-jump-search
        "aens" 'notmuch-search))
    :config
    (progn
      (dolist (prefix '(("ms" . "stash")
                        ("mp" . "part")
                        ("mP" . "patch")))
        (spacemacs/declare-prefix-for-mode 'notmuch-show-mode
          (car prefix) (cdr prefix)))
      ;; key bindings
      (evil-define-key 'visual notmuch-search-mode-map
        "*" 'notmuch-search-tag-all
        "a" 'notmuch-search-archive-thread
        "-" 'notmuch-search-remove-tag
        "+" 'notmuch-search-add-tag)
      (spacemacs/set-leader-keys-for-major-mode 'notmuch-show-mode
        "a" 'notmuch-show-save-attachments
        ;; part
        "pm" 'notmuch-show-choose-mime-of-part
        "pp" 'spacemacs/notmuch-show-as-patch
        "p|" 'notmuch-show-pipe-part
        "po" 'notmuch-show-interactively-view-part
        "pv" 'notmuch-show-view-part
        "ps" 'notmuch-show-save-part
        ;; stash
        "sG" 'notmuch-show-stash-git-send-email
        "sL" 'notmuch-show-stash-mlarchive-link-and-go
        "sl" 'notmuch-show-stash-mlarchive-link
        "st" 'notmuch-show-stash-to
        "sT" 'notmuch-show-stash-tags
        "ss" 'notmuch-show-stash-subject
        "sI" 'notmuch-show-stash-message-id-stripped
        "si" 'notmuch-show-stash-message-id
        "sf" 'notmuch-show-stash-from
        "sF" 'notmuch-show-stash-filename
        "sd" 'notmuch-show-stash-date
        "sc" 'notmuch-show-stash-cc
        ;; patch
        "Po" 'spacemacs/notmuch-show-open-github-patch
        "Pa" 'spacemacs/notmuch-git-apply-patch
        "PA" 'spacemacs/notmuch-git-apply-patch-part)
      ;; Evilify notmuch modes
      ;; Use normal mode map to allow proper editing capabilities
      ;; for the embedded search field in `notmuch-hello-mode`
      (evil-set-initial-state 'notmuch-hello-mode 'normal)
      (evil-define-key 'normal notmuch-hello-mode-map
        "C-tab" #'widget-backward
        "S-tab" #'widget-backward
        "=" #'notmuch-refresh-this-buffer
        "?" #'notmuch-help
        "G" #'notmuch-poll-and-refresh-this-buffer
        "g" #'notmuch-refresh-this-buffer
        "J" #'notmuch-jump-search
        "m" #'notmuch-mua-new-mail
        "q" #'notmuch-bury-or-kill-this-buffer
        "s" #'notmuch-search
        "v" #'notmuch-hello-versions
        "z" #'notmuch-tree
        "M-=" #'notmuch-refresh-all-buffers)
      ;; Make notmuch message mode closable via q
      (evil-define-key 'normal notmuch-message-mode-map
        "q" #'message-kill-buffer)
      (evilified-state-evilify-map notmuch-show-mode-map
        :mode notmuch-show-mode
        :bindings
        (kbd "N")   'notmuch-show-next-message
        (kbd "P")   'notmuch-show-previous-message
        (kbd "p")   'notmuch-show-previous-open-message
        (kbd "n")   'notmuch-show-next-open-message
        (kbd "o")   'notmuch-show-open-or-close-all
        (kbd "O")   'spacemacs/notmuch-show-close-all)
      (evilified-state-evilify-map notmuch-tree-mode-map
        :mode notmuch-tree-mode
        :bindings
        (kbd "N") 'notmuch-tree-next-message
        (kbd "P") 'notmuch-tree-prev-message
        (kbd "d") 'spacemacs/notmuch-tree-message-delete-down
        (kbd "D") 'spacemacs/notmuch-tree-message-delete-up
        (kbd "n") 'notmuch-tree-next-matching-message
        (kbd "p") 'notmuch-tree-prev-matching-message
        (kbd "M") 'compose-mail-other-frame)
      (evilified-state-evilify-map notmuch-search-mode-map
        :mode notmuch-search-mode
        :bindings
        (kbd "a") 'spacemacs/notmuch-search-archive-thread-down
        (kbd "A") 'spacemacs/notmuch-search-archive-thread-up
        (kbd "d") 'spacemacs/notmuch-search-message-delete-down
        (kbd "D") 'spacemacs/notmuch-search-message-delete-up
        (kbd "J") 'notmuch-jump-search
        (kbd "L") 'notmuch-search-filter
        (kbd "gg") 'notmuch-search-first-thread
        (kbd "gr") 'notmuch-refresh-this-buffer
        (kbd "gR") 'notmuch-refresh-all-buffers
        (kbd "G") 'notmuch-search-last-thread
        (kbd "M") 'compose-mail-other-frame))))

(defun notmuch/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (require 'ol-notmuch)))

(defun notmuch/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      (add-to-list 'persp-filter-save-buffers-functions
                   'spacemacs//notmuch-persp-filter-save-buffers-function)
      (spacemacs|define-custom-layout notmuch-spacemacs-layout-name
        :binding notmuch-spacemacs-layout-binding
        :body
        (progn
          (dolist (mode notmuch-modes)
            (let ((hook (intern (concat (symbol-name mode) "-hook"))))
              (add-hook hook #'spacemacs//notmuch-buffer-to-persp)))
          (call-interactively 'notmuch))))))

(defun notmuch/post-init-window-purpose ()
  (let ((modes))
    (dolist (mode notmuch-modes)
      (add-to-list 'modes (cons mode 'mail)))
    (purpose-set-extension-configuration
     :notmuch-layer
     (purpose-conf :mode-purposes modes))))
