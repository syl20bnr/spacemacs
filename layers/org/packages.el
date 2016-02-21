;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq org-packages
  '(
    company
    company-emoji
    emoji-cheat-sheet-plus
    (evil-org :location local)
    evil-surround
    flyspell
    gnuplot
    htmlize
    ;; org and org-agenda are installed by `org-plus-contrib'
    (org :location built-in)
    (org-agenda :location built-in)
    (org-plus-contrib :step pre)
    org-bullets
    ;; org-mime is installed by `org-plus-contrib'
    (org-mime :location built-in)
    org-pomodoro
    org-present
    org-repo-todo
    persp-mode
    toc-org
    ))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun org/post-init-company ()
    (spacemacs|add-company-hook org-mode)
    (push 'company-capf company-backends-org-mode))
  (defun org/post-init-company-emoji ()
    (push 'company-emoji company-backends-org-mode)))

(defun org/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'org-mode-hook 'spacemacs/delay-emoji-cheat-sheet-hook))

(defun org/init-evil-org ()
  (use-package evil-org
    :commands evil-org-mode
    :init
    (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "C" 'evil-org-recompute-clocks)
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

(defun org/post-init-evil-surround ()
  (defun spacemacs/add-org-surrounds ()
    (push '(?: . spacemacs//surround-drawer) evil-surround-pairs-alist))
  (add-hook 'org-mode-hook 'spacemacs/add-org-surrounds)
  (defun spacemacs//surround-drawer ()
    (let ((dname (read-from-minibuffer "" "")))
      (cons (format ":%s:\n" (or dname "")) "\n:END:"))))

(defun org/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'org-mode-hook))

(defun org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'org-mode
            "tp" 'org-plot/gnuplot)))

;; dummy init function to force installation of `org-plus-contrib'
(defun org/init-org-plus-contrib ())

(defun org/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
    :defer t
    :init
    (progn
      ;; FIXME: This check has been disabled pending a resolution of
      ;; https://github.com/syl20bnr/spacemacs/issues/3933
      ;; (when (featurep 'org)
      ;;   (configuration-layer//set-error)
      ;;   (spacemacs-buffer/append
      ;;    (concat
      ;;     "Org features were loaded before the `org' layer initialized.\n"
      ;;     "Try removing org code from user initialization and private layers.") t))

      (setq org-clock-persist-file
            (concat spacemacs-cache-directory "org-clock-save.el")
            org-id-locations-file
            (concat spacemacs-cache-directory ".org-id-locations")
            org-log-done t
            org-startup-with-inline-images t
            org-src-fontify-natively t)

      (with-eval-after-load 'org-indent
        (spacemacs|hide-lighter org-indent-mode))
      (setq org-startup-indented t)
      (let ((dir (configuration-layer/get-layer-property 'org :dir)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
      (defun spacemacs/insert-keybinding-org (key)
        "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
        (interactive "kType key sequence: ")
        (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -8))))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "'" 'org-edit-special
        "c" 'org-capture
        "d" 'org-deadline
        "D" 'org-insert-drawer
        "e" 'org-export-dispatch
        "f" 'org-set-effort
        "P" 'org-set-property
        ":" 'org-set-tags

        "a" 'org-agenda
        "b" 'org-tree-to-indirect-buffer
        "A" 'org-archive-subtree
        "l" 'org-open-at-point
        "T" 'org-show-todo-tree

        "." 'org-time-stamp
        "!" 'org-time-stamp-inactive

        ;; headings
        "hi" 'org-insert-heading-after-current
        "hI" 'org-insert-heading

        ;; More cycling options (timestamps, headlines, items, properties)
        "L" 'org-shiftright
        "H" 'org-shiftleft
        "J" 'org-shiftdown
        "K" 'org-shiftup

        ;; Change between TODO sets
        "C-S-l" 'org-shiftcontrolright
        "C-S-h" 'org-shiftcontrolleft
        "C-S-j" 'org-shiftcontroldown
        "C-S-k" 'org-shiftcontrolup

        ;; Subtree editing
        "Sl" 'org-demote-subtree
        "Sh" 'org-promote-subtree
        "Sj" 'org-move-subtree-down
        "Sk" 'org-move-subtree-up

        ;; tables
        "ta" 'org-table-align
        "tb" 'org-table-blank-field
        "tc" 'org-table-convert
        "tdc" 'org-table-delete-column
        "tdr" 'org-table-kill-row
        "te" 'org-table-eval-formula
        "tE" 'org-table-export
        "th" 'org-table-previous-field
        "tH" 'org-table-move-column-left
        "tic" 'org-table-insert-column
        "tih" 'org-table-insert-hline
        "tiH" 'org-table-hline-and-move
        "tir" 'org-table-insert-row
        "tI" 'org-table-import
        "tj" 'org-table-next-row
        "tJ" 'org-table-move-row-down
        "tK" 'org-table-move-row-up
        "tl" 'org-table-next-field
        "tL" 'org-table-move-column-right
        "tn" 'org-table-create
        "tN" 'org-table-create-with-table.el
        "tr" 'org-table-recalculate
        "ts" 'org-table-sort-lines
        "ttf" 'org-table-toggle-formula-debugger
        "tto" 'org-table-toggle-coordinate-overlays
        "tw" 'org-table-wrap-region

        ;; Multi-purpose keys
        (or dotspacemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
        "*" 'org-ctrl-c-star
        "RET" 'org-ctrl-c-ret
        "-" 'org-ctrl-c-minus
        "^" 'org-sort
        "/" 'org-sparse-tree

        "I" 'org-clock-in
        "n" 'org-narrow-to-subtree
        "N" 'widen
        "O" 'org-clock-out
        "q" 'org-clock-cancel
        "R" 'org-refile
        "s" 'org-schedule

        ;; insertion of common elements
        "il" 'org-insert-link
        "if" 'org-footnote-new
        "ik" 'spacemacs/insert-keybinding-org

        ;; images and other link types have no commands in org mode-line
        ;; could be inserted using yasnippet?
        ;; region manipulation
        "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
        "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
        "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
        "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
        "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
        "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
        "xv" (spacemacs|org-emphasize spacemacs/org-verbose ?=))

      ;; Add global evil-leader mappings. Used to access org-agenda
      ;; functionalities – and a few others commands – from any other mode.
      (spacemacs/declare-prefix "ao" "org")
      (spacemacs/set-leader-keys
        ;; org-agenda
        "ao#" 'org-agenda-list-stuck-projects
        "ao/" 'org-occur-in-agenda-files
        "aoa" 'org-agenda-list
        "aoe" 'org-store-agenda-views
        "aom" 'org-tags-view
        "aoo" 'org-agenda
        "aos" 'org-search-view
        "aot" 'org-todo-list
        ;; other
        "aoO" 'org-clock-out
        "aoc" 'org-capture
        "aol" 'org-store-link))
    :config
    (progn
      (setq org-default-notes-file "notes.org")
      (font-lock-add-keywords
       'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                    (1 font-lock-comment-face prepend)
                    (2 font-lock-function-name-face)
                    (3 font-lock-comment-face prepend))))

      (require 'org-indent)
      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (define-key global-map "\C-cc" 'org-capture)

      ;; Open links and files with RET in normal state
      (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '")) 'org-edit-src-exit)

      (spacemacs/set-leader-keys
        "Cc" 'org-capture)

      ;; Evilify the calendar tool on C-c .
      (unless (eq 'emacs dotspacemacs-editing-style)
        (define-key org-read-date-minibuffer-local-map (kbd "M-h")
          (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-l")
          (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-k")
          (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-j")
          (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-H")
          (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-L")
          (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-K")
          (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-J")
          (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))))))

(defun org/init-org-agenda ()
  (use-package org-agenda
    :defer t
    :init
    (setq org-agenda-restore-windows-after-quit t)
    :config
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      (kbd "M-j") 'org-agenda-next-item
      (kbd "M-k") 'org-agenda-previous-item
      (kbd "M-h") 'org-agenda-earlier
      (kbd "M-l") 'org-agenda-later
      (kbd "gd") 'org-agenda-toggle-time-grid
      (kbd "gr") 'org-agenda-redo)))

(defun org/init-org-bullets ()
  (use-package org-bullets
    :defer t
    :init (add-hook 'org-mode-hook 'org-bullets-mode)))

(defun org/init-org-mime ()
  (use-package org-mime
    :defer t
    :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'message-mode
        "M" 'org-mime-htmlize)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "m" 'org-mime-org-buffer-htmlize))))

(defun org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "p" 'org-pomodoro))))

(defun org/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (evilified-state-evilify nil org-present-mode-keymap
        "h" 'org-present-prev
        "l" 'org-present-next
        "q" 'org-present-quit)
      (defun spacemacs//org-present-start ()
        "Initiate `org-present' mode"
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only)
        (evil-evilified-state))
      (defun spacemacs//org-present-end ()
        "Terminate `org-present' mode"
        (org-present-small)
        (org-remove-inline-images)
        (org-present-show-cursor)
        (org-present-read-write)
        (evil-normal-state))
      (add-hook 'org-present-mode-hook 'spacemacs//org-present-start)
      (add-hook 'org-present-mode-quit-hook 'spacemacs//org-present-end))))

(defun org/init-org-repo-todo ()
  (use-package org-repo-todo
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "Ct"  'ort/capture-todo
        "CT"  'ort/capture-checkitem)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "gt" 'ort/goto-todos))))

(defun org/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@Org"
    :binding "o"
    :body
    (find-file (first (org-agenda-files)))))

(defun org/init-toc-org ()
  (use-package toc-org
    :defer t
    :init
    (progn
      (setq toc-org-max-depth 10)
      (add-hook 'org-mode-hook 'toc-org-enable))))

(defun org/init-htmlize ()
 (use-package htmlize
    :defer t))
