;;; packages.el --- Org Layer packages File for Spacemacs
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

(setq org-packages
  '(
    company
    company-emoji
    emoji-cheat-sheet-plus
    evil-org
    gnuplot
    htmlize
    ;; org is installed by `org-plus-contrib'
    (org :location built-in)
    (org-plus-contrib :step pre)
    org-bullets
    ;; org-mime is installed by `org-plus-contrib'
    (org-mime :location built-in)
    org-pomodoro
    org-present
    org-repo-todo
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
      (evil-leader/set-key-for-mode 'org-mode
        "mC" 'evil-org-recompute-clocks

        ;; evil-org binds these keys, so we bind them back to their original
        ;; value
        "t" (lookup-key evil-leader--default-map "t")
        "a" (lookup-key evil-leader--default-map "a")
        "b" (lookup-key evil-leader--default-map "b")
        "c" (lookup-key evil-leader--default-map "c")
        "l" (lookup-key evil-leader--default-map "l")
        "o" (lookup-key evil-leader--default-map "o"))
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

(defun org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (evil-leader/set-key-for-mode 'org-mode
            "mtp" 'org-plot/gnuplot)))

;; dummy init function to force installation of `org-plus-contrib'
(defun org/init-org-plus-contrib ())

(defun org/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :defer t
    :init
    (progn
      (setq org-clock-persist-file
            (concat spacemacs-cache-directory "org-clock-save.el")
            org-log-done t
            org-startup-with-inline-images t
            org-src-fontify-natively t)

      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))
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
      (evil-leader/set-key-for-mode 'org-mode
        "m'" 'org-edit-special
        "mc" 'org-capture
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mf" 'org-set-effort
        "m:" 'org-set-tags

        "ma" 'org-agenda
        "mb" 'org-tree-to-indirect-buffer
        "mA" 'org-archive-subtree
        "ml" 'org-open-at-point
        "mT" 'org-show-todo-tree

        "m." 'org-time-stamp

        ;; headings
        "mhi" 'org-insert-heading-after-current
        "mhI" 'org-insert-heading

        ;; More cycling options (timestamps, headlines, items, properties)
        "mL" 'org-shiftright
        "mH" 'org-shiftleft
        "mJ" 'org-shiftdown
        "mK" 'org-shiftup

        ;; Change between TODO sets
        "m C-S-l" 'org-shiftcontrolright
        "m C-S-h" 'org-shiftcontrolleft
        "m C-S-j" 'org-shiftcontroldown
        "m C-S-k" 'org-shiftcontrolup

        ;; Subtree editing
        "mSl" 'org-demote-subtree
        "mSh" 'org-promote-subtree
        "mSj" 'org-move-subtree-down
        "mSk" 'org-move-subtree-up

        ;; tables
        "mta" 'org-table-align
        "mtb" 'org-table-blank-field
        "mtc" 'org-table-convert
        "mtdc" 'org-table-delete-column
        "mtdr" 'org-table-kill-row
        "mte" 'org-table-eval-formula
        "mtE" 'org-table-export
        "mth" 'org-table-previous-field
        "mtH" 'org-table-move-column-left
        "mtic" 'org-table-insert-column
        "mtih" 'org-table-insert-hline
        "mtiH" 'org-table-hline-and-move
        "mtir" 'org-table-insert-row
        "mtI" 'org-table-import
        "mtj" 'org-table-next-row
        "mtJ" 'org-table-move-row-down
        "mtK" 'org-table-move-row-up
        "mtl" 'org-table-next-field
        "mtL" 'org-table-move-column-right
        "mtn" 'org-table-create
        "mtN" 'org-table-create-with-table.el
        "mtr" 'org-table-recalculate
        "mts" 'org-table-sort-lines
        "mttf" 'org-table-toggle-formula-debugger
        "mtto" 'org-table-toggle-coordinate-overlays
        "mtw" 'org-table-wrap-region

        "mI" 'org-clock-in
        (if dotspacemacs-major-mode-leader-key
            (concat "m" dotspacemacs-major-mode-leader-key)
          "m,") 'org-ctrl-c-ctrl-c
          "mn" 'org-narrow-to-subtree
          "mN" 'widen
          "mO" 'org-clock-out
          "mq" 'org-clock-cancel
          "mR" 'org-refile
          "ms" 'org-schedule

          ;; insertion of common elements
          "mil" 'org-insert-link
          "mif" 'org-footnote-new
          "mik" 'spacemacs/insert-keybinding-org

          ;; images and other link types have no commands in org mode-line
          ;; could be inserted using yasnippet?
          ;; region manipulation
          "mxb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
          "mxc" (spacemacs|org-emphasize spacemacs/org-code ?~)
          "mxi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
          "mxr" (spacemacs|org-emphasize spacemacs/org-clear ? )
          "mxs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
          "mxu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
          "mxv" (spacemacs|org-emphasize spacemacs/org-verbose ?=))

      (eval-after-load "org-agenda"
        '(progn
           (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
           (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
           ;; Since we override SPC, let's make RET do that functionality
           (define-key org-agenda-mode-map
             (kbd "RET") 'org-agenda-show-and-scroll-up)
           (define-key org-agenda-mode-map
             (kbd "SPC") evil-leader--default-map))))
    :config
    (progn
      ;; setup org directory
      (unless (file-exists-p org-directory)
        (make-directory org-directory))
      (font-lock-add-keywords
       'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                    (1 font-lock-comment-face prepend)
                    (2 font-lock-function-name-face)
                    (3 font-lock-comment-face prepend))))

      (require 'org-indent)
      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)

      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '")) 'org-edit-src-exit)

      (evil-leader/set-key
        "Cc" 'org-capture))))

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
      (evil-leader/set-key-for-mode 'message-mode
        "mM" 'org-mime-htmlize)
      (evil-leader/set-key-for-mode 'org-mode
        "mm" 'org-mime-org-buffer-htmlize))))

(defun org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (evil-leader/set-key-for-mode 'org-mode
        "mp" 'org-pomodoro))))

(defun org/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (evilify nil org-present-mode-keymap
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
      (evil-leader/set-key
        "Ct"  'ort/capture-todo
        "CT"  'ort/capture-checkitem)
      (evil-leader/set-key-for-mode 'org-mode
        "mgt" 'ort/goto-todos))))

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
