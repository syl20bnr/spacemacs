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
    evil-org
    htmlize
    org
    org-bullets
    org-pomodoro
    org-present
    org-repo-todo
    toc-org
    ))

(defun org/init-evil-org ()
  (use-package evil-org
    :commands evil-org-mode
    :init
    (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'org-mode
           "a" nil "ma" 'org-agenda
           "b" nil "mb" 'org-tree-to-indirect-buffer
           "c" nil "mA" 'org-archive-subtree
           "o" nil "mC" 'evil-org-recompute-clocks
           "l" nil "mo" 'evil-org-open-links
           "t" nil "mT" 'org-show-todo-tree)
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

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

        ;; headings
        "mhi" 'org-insert-heading-after-current
        "mhI" 'org-insert-heading

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

(defun org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (system-is-mac)
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
    :commands (ort/capture-todo
               ort/capture-todo-check
               ort/goto-todos)
    :init
    (progn
      (evil-leader/set-key
        "Ct"  'ort/capture-todo
        "CT"  'ort/capture-todo-check)
      (evil-leader/set-key-for-mode 'org-mode
        "mgt" 'ort/goto-todos))))

(defun org/init-toc-org ()
  (use-package toc-org
    :init
    (add-hook 'org-mode-hook 'toc-org-enable)))

(defun org/init-htmlize ()
 (use-package htmlize
    :defer t))
