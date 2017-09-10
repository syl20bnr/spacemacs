;;; packages.el --- mu4e Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq notmuch-packages '(notmuch helm-notmuch))

(defun notmuch/init-notmuch ()
  (use-package notmuch
    :defer t
    :commands notmuch
    :init
    (progn
      (spacemacs/declare-prefix "aN" "notmuch")
      (spacemacs/set-leader-keys "aNN" 'notmuch)
      (spacemacs/set-leader-keys "aNn" 'helm-notmuch)
      (load-library "org-notmuch"))
    :config
    (progn
      (dolist (prefix '(("ms" . "stash")
                        ("mp" . "part")
                        ("mP" . "patch")))
        (spacemacs/declare-prefix-for-mode 'notmuch-show-mode
          (car prefix)
          (cdr prefix)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; notmuch-hello-mode-map ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (evilified-state-evilify-map notmuch-hello-mode-map
        :mode notmuch-hello-mode)

      ;;;;;;;;;;;;;;;;;;;;;;;
      ;; notmuch-show mode ;;
      ;;;;;;;;;;;;;;;;;;;;;;;
      (spacemacs/set-leader-keys-for-major-mode 'notmuch-show-mode
        "a" 'notmuch-show-save-attachments
        ;; part
        "pm" 'notmuch-show-choose-mime-of-part
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
        "Pa" 'spacemacs/notmuch-git-apply-patch
        "PA" 'spacemacs/notmuch-git-apply-patch-part
        )

      (evilified-state-evilify-map notmuch-show-mode-map
        :mode notmuch-show-mode
        :bindings
        (kbd "C-n") 'notmuch-show-next-thread-show
        (kbd "C-p") 'notmuch-show-previous-thread-show
        (kbd "N")   'notmuch-show-next-message
        (kbd "n")   'notmuch-show-next-open-message
        (kbd "p")   'notmuch-show-previous-open-message
        (kbd "P")   'notmuch-show-previous-message
        (kbd "o")   'notmuch-show-open-or-close-all
        (kbd "O")   'spacemacs/notmuch-show-close-all
        )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; notmuch-tree-mode-map ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (evilified-state-evilify-map notmuch-tree-mode-map
        :mode notmuch-tree-mode
        :bindings
        (kbd "d") 'spacemacs/notmuch-message-delete-down
        (kbd "D") 'spacemacs/notmuch-message-delete-up
        (kbd "M") 'compose-mail-other-frame)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; notmuch-search-mode-map ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (evilified-state-evilify-map notmuch-search-mode-map
        :mode notmuch-search-mode
        :bindings
        (kbd "a") 'notmuch-search-archive-thread
        (kbd "d") 'spacemacs/notmuch-message-delete-down
        (kbd "D") 'spacemacs/notmuch-message-delete-up
        (kbd "J") 'notmuch-jump-search
        (kbd "L") 'notmuch-search-filter
        (kbd "gg") 'notmuch-search-first-thread
        (kbd "gr") 'notmuch-refresh-this-buffer
        (kbd "gR") 'notmuch-refresh-all-buffers
        (kbd "G") 'notmuch-search-last-thread
        (kbd "T") 'spacemacs/notmuch-trash
        (kbd "M") 'compose-mail-other-frame)

      (evil-define-key 'visual notmuch-search-mode-map
        "*" 'notmuch-search-tag-all
        "a" 'notmuch-search-archive-thread
        "-" 'notmuch-search-remove-tag
        "+" 'notmuch-search-add-tag)
      ))

  ;; fixes: killing a notmuch buffer does not show the previous buffer
  (push "\\*notmuch.+\\*" spacemacs-useful-buffers-regexp)
  )

(defun notmuch/init-helm-notmuch ()
  (use-package helm-notmuch
    :defer t
    :init (with-eval-after-load 'notmuch)))
