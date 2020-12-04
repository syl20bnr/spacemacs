;;; packages.el --- Notmuch Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

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
    :init (space-macs/set-leader-keys "aenn" 'counsel-notmuch)))

(defun notmuch/init-helm-notmuch ()
  (use-package helm-notmuch
    :defer t
    :init (space-macs/set-leader-keys "aenn" 'helm-notmuch)))

(defun notmuch/init-notmuch ()
  (use-package notmuch
    :defer t
    :commands notmuch
    :init
    (progn
      (space-macs/declare-prefix "aen" "notmuch")
      (space-macs/set-leader-keys
        "aenN" 'notmuch
        "aeni" 'space-macs/notmuch-inbox
        "aenj" 'notmuch-jump-search
        "aens" 'notmuch-search))
    :config
    (progn
      (dolist (prefix '(("ms" . "stash")
                        ("mp" . "part")
                        ("mP" . "patch")))
        (space-macs/declare-prefix-for-mode 'notmuch-show-mode
          (car prefix) (cdr prefix)))
      ;; key bindings
      (evil-define-key 'visual notmuch-search-mode-map
        "*" 'notmuch-search-tag-all
        "a" 'notmuch-search-archive-thread
        "-" 'notmuch-search-remove-tag
        "+" 'notmuch-search-add-tag)
      (space-macs/set-leader-keys-for-major-mode 'notmuch-show-mode
        "a" 'notmuch-show-save-attachments
        ;; part
        "pm" 'notmuch-show-choose-mime-of-part
        "pp" 'space-macs/notmuch-show-as-patch
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
        "Po" 'space-macs/notmuch-show-open-github-patch
        "Pa" 'space-macs/notmuch-git-apply-patch
        "PA" 'space-macs/notmuch-git-apply-patch-part)
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
        (kbd "O")   'space-macs/notmuch-show-close-all)
      (evilified-state-evilify-map notmuch-tree-mode-map
        :mode notmuch-tree-mode
        :bindings
        (kbd "N") 'notmuch-tree-next-message
        (kbd "P") 'notmuch-tree-prev-message
        (kbd "d") 'space-macs/notmuch-message-delete-down
        (kbd "D") 'space-macs/notmuch-message-delete-up
        (kbd "n") 'notmuch-tree-next-matching-message
        (kbd "p") 'notmuch-tree-prev-matching-message
        (kbd "M") 'compose-mail-other-frame)
      (evilified-state-evilify-map notmuch-search-mode-map
        :mode notmuch-search-mode
        :bindings
        (kbd "a") 'space-macs/notmuch-search-archive-thread-down
        (kbd "A") 'space-macs/notmuch-search-archive-thread-up
        (kbd "d") 'space-macs/notmuch-message-delete-down
        (kbd "D") 'space-macs/notmuch-message-delete-up
        (kbd "J") 'notmuch-jump-search
        (kbd "L") 'notmuch-search-filter
        (kbd "gg") 'notmuch-search-first-thread
        (kbd "gr") 'notmuch-refresh-this-buffer
        (kbd "gR") 'notmuch-refresh-all-buffers
        (kbd "G") 'notmuch-search-last-thread
        (kbd "M") 'compose-mail-other-frame))))

(defun notmuch/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config (require 'ol-notmuch)))

(defun notmuch/pre-init-persp-mode ()
  (space-macs|use-package-add-hook persp-mode
    :post-config
    (progn
      (add-to-list 'persp-filter-save-buffers-functions
                   'space-macs//notmuch-persp-filter-save-buffers-function)
      (space-macs|define-custom-layout notmuch-space-macs-layout-name
        :binding notmuch-space-macs-layout-binding
        :body
        (progn
          (dolist (mode notmuch-modes)
            (let ((hook (intern (concat (symbol-name mode) "-hook"))))
              (add-hook hook #'space-macs//notmuch-buffer-to-persp)))
          (call-interactively 'notmuch))))))

(defun notmuch/post-init-window-purpose ()
  (let ((modes))
    (dolist (mode notmuch-modes)
      (add-to-list 'modes (cons mode 'mail)))
    (purpose-set-extension-configuration
     :notmuch-layer
     (purpose-conf :mode-purposes modes))))


