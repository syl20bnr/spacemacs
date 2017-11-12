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

(setq notmuch-packages '(notmuch helm-notmuch mbsync))

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

    (define-key notmuch-search-mode-map "a" nil)

    (setq notmuch-saved-searches
          '((:key "i" :search-type tree :name "inbox" :query "folder:INBOX")
            (:key "l" :search-type tree :name "lbb" :query "folder:INBOX.lbbhuis OR folder:INBOX.lbbfrietjes")
            (:key "a" :search-type tree :name "linuxAudio-lists" :query "folder:INBOX.LinuxAudio-list OR folder:INBOX.Ardour-list OR folder:INBOX.music-dsp OR folder:INBOX.faust")
            (:key "n" :search-type tree :name "NixOS" :query "folder:INBOX.hydra OR folder:INBOX.NixOS")
            (:key "s" :search-type tree :name "sent" :query "folder:Sent OR folder:Sent.Sent")
            (:key "d" :search-type tree :name "drafts" :query "folder:Drafts")
            (:key "t" :search-type tree :name "trash" :query "folder:Trash")
            (:key "u" :search-type tree :name "unread" :query "tag:unread")
            ))

    ;; We add items later in reverse order with (add-to-list ...):
    (setq notmuch-hello-sections '())

    ;; Add a thousand separator
    (setq notmuch-hello-thousands-separator ".")
;; This is GPLv3. If you still don't know the details, read
;; http://www.gnu.org/licenses/gpl-3.0.en.html

(defun my-notmuch-hello-insert-recent-searches ()
  "Insert recent searches."
  (when notmuch-search-history
    (widget-insert "Recent searches:")
    (widget-insert "\n\n")
    (let ((start (point)))
      (loop for i from 1 to notmuch-hello-recent-searches-max
	    for search in notmuch-search-history do
	    (let ((widget-symbol (intern (format "notmuch-hello-search-%d" i))))
	      (set widget-symbol
		   (widget-create 'editable-field
				  ;; Don't let the search boxes be
				  ;; less than 8 characters wide.
				  :size (max 8
					     (- (window-width)
						;; Leave some space
						;; at the start and
						;; end of the
						;; boxes.
						(* 2 notmuch-hello-indent)
						;; 1 for the space
						;; before the `[del]'
						;; button. 5 for the
						;; `[del]' button.
						1 5))
				  :action (lambda (widget &rest ignore)
					    (notmuch-hello-search (widget-value widget)))
				  search))
	      (widget-insert " ")
	      (widget-create 'push-button
			     :notify (lambda (widget &rest ignore)
				       (when (y-or-n-p "Are you sure you want to delete this search? ")
					 (notmuch-hello-delete-search-from-history widget)))
			     :notmuch-saved-search-widget widget-symbol
			     "del"))
	    (widget-insert "\n"))
      (indent-rigidly start (point) notmuch-hello-indent))
    nil))

  (add-to-list 'notmuch-hello-sections #'my-notmuch-hello-insert-recent-searches)

  (add-to-list 'notmuch-hello-sections #'notmuch-hello-insert-search)

(defface my-notmuch-hello-header-face
  '((t :foreground "black"
       :background "yellow"
       :weight bold))
  "Font for the header in `my-notmuch-hello-insert-searches`."
  :group 'notmuch-faces)

(defun my-count-query (query)
	(with-temp-buffer
	  (insert query "\n")
	  (unless (= (call-process-region (point-min) (point-max) notmuch-command
                                    t t nil "count" "--batch") 0)
      (notmuch-logged-error "notmuch count --batch failed"
                            "Please check that the notmuch CLI is new enough to support `count
--batch'. In general we recommend running matching versions of
the CLI and emacs interface."))

	  (goto-char (point-min))
	  (let ((n (read (current-buffer))))
      (if (= n 0)
          nil
        (notmuch-hello-nice-number n)))))

(defun my-notmuch-hello-query-insert (cnt query elem)
	(if cnt
      (let* ((str (format "%s" cnt))
             (widget-push-button-prefix "")
             (widget-push-button-suffix "")
             (oldest-first (case (plist-get elem :sort-order)
                             (newest-first nil)
                             (oldest-first t)
                             (otherwise notmuch-search-oldest-first))))
        (widget-create 'push-button
                       :notify #'notmuch-hello-widget-search
                       :notmuch-search-terms query
                       :notmuch-search-oldest-first `nil
                       :notmuch-search-type 'tree
                       str)
        (widget-insert (make-string (- 8 (length str)) ? )))
	  (widget-insert "        ")))

(defun my-notmuch-hello-insert-searches ()
	"Insert the saved-searches section."
	(widget-insert (propertize "New     Total      Key  List\n" 'face 'my-notmuch-hello-header-face))
	(mapc (lambda (elem)
          (when elem
            (let* ((q_tot (plist-get elem :query))
                   (q_new (concat q_tot " AND tag:unread"))
                   (n_tot (my-count-query q_tot))
                   (n_new (my-count-query q_new)))
              (my-notmuch-hello-query-insert n_new q_new elem)
              (my-notmuch-hello-query-insert n_tot q_tot elem)
              (widget-insert "   ")
              (widget-insert (plist-get elem :key))
              (widget-insert "    ")
              (widget-insert (plist-get elem :name))
              (widget-insert "\n")
              ))
          )
        notmuch-saved-searches))

(defun my-notmuch-hello-reposition-after-refresh ()
	(goto-char (point-min))
	(forward-line 1))
(add-hook 'notmuch-hello-refresh-hook #'my-notmuch-hello-reposition-after-refresh)

(add-to-list 'notmuch-hello-sections #'my-notmuch-hello-insert-searches)

;; this is the end of use-package notmuch:



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
        "o" 'mbsync
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
