;;; packages.el --- Spacemacs UI Layer packages File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-ui-packages
      '(ace-link
        (centered-cursor :location local)
        desktop
        (doc-view :location built-in)
        flx-ido
        info+
        open-junk-file
        paradox
        restart-emacs
        winum))

(defun spacemacs-ui/init-ace-link ()
  (use-package ace-link
    :commands spacemacs/ace-buffer-links
    :init
    (progn
      (define-key spacemacs-buffer-mode-map "o" 'spacemacs/ace-buffer-links)
      (with-eval-after-load 'info
        (define-key Info-mode-map "o" 'ace-link-info))
      (with-eval-after-load 'help-mode
        (define-key help-mode-map "o" 'ace-link-help))
      (with-eval-after-load 'eww
        (define-key eww-link-keymap "o" 'ace-link-eww)
        (define-key eww-mode-map "o" 'ace-link-eww)))
    :config
    (progn
      (defvar spacemacs--link-pattern "~?/.+\\|\s\\[")
      (defun spacemacs//collect-spacemacs-buffer-links ()
        (let ((end (window-end))
              points)
          (save-excursion
            (goto-char (window-start))
            (while (re-search-forward spacemacs--link-pattern end t)
              (push (+ (match-beginning 0) 1) points))
            (nreverse points))))
      (defun spacemacs/ace-buffer-links ()
        "Ace jump to links in `spacemacs' buffer."
        (interactive)
        (let ((res (avy-with spacemacs/ace-buffer-links
                             (avy--process
                              (spacemacs//collect-spacemacs-buffer-links)
                              #'avy--overlay-pre))))
          (when res
            (goto-char (1+ res))
            (widget-button-press (point))))))))

(defun spacemacs-ui/init-centered-cursor ()
  (use-package centered-cursor-mode
    :commands (centered-cursor-mode
               global-centered-cursor-mode)
    :init
    (progn
      (spacemacs|add-toggle centered-point
        :mode centered-cursor-mode
        :documentation
        "Keep point at the center of the window."
        :evil-leader "t-")
      (spacemacs|add-toggle centered-point-globally
        :mode global-centered-cursor-mode
        :documentation
        "Keep point at the center of the window globally."
        :evil-leader "t C--"))
    :config
    (progn
      (setq ccm-recenter-at-end-of-file t
            ccm-ignored-commands '(mouse-drag-region
                                   mouse-set-point
                                   widget-button-click
                                   scroll-bar-toolkit-scroll
                                   evil-mouse-drag-region))
      (spacemacs|diminish centered-cursor-mode " ⊝" " -"))))

(defun spacemacs-ui/init-desktop ()
  (use-package desktop
    :defer t
    :init
    (setq desktop-dirname spacemacs-cache-directory)
    :config
    (push spacemacs-cache-directory desktop-path)))

(defun spacemacs-ui/init-doc-view ()
  (use-package doc-view
    :defer t
    :init
    (evilified-state-evilify doc-view-mode doc-view-mode-map
      "/"  'spacemacs/doc-view-search-new-query
      "?"  'spacemacs/doc-view-search-new-query-backward
      "gg" 'doc-view-first-page
      "G"  'spacemacs/doc-view-goto-page
      "gt" 'doc-view-goto-page
      "h"  'doc-view-previous-page
      "j"  'doc-view-next-line-or-next-page
      "k"  'doc-view-previous-line-or-previous-page
      "K"  'doc-view-kill-proc-and-buffer
      "l"  'doc-view-next-page
      "n"  'doc-view-search
      "N"  'doc-view-search-backward
      (kbd "C-d") 'doc-view-scroll-up-or-next-page
      (kbd "C-k") 'doc-view-kill-proc
      (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
    :config
    (progn
      (defun spacemacs/doc-view-search-new-query ()
        "Initiate a new query."
        (interactive)
        (doc-view-search 'newquery))

      (defun spacemacs/doc-view-search-new-query-backward ()
        "Initiate a new query."
        (interactive)
        (doc-view-search 'newquery t))

      (defun spacemacs/doc-view-goto-page (&optional count)
        (interactive (list
                      (when current-prefix-arg
                        (prefix-numeric-value current-prefix-arg))))
        (if (null count)
            (doc-view-last-page)
          (doc-view-goto-page count)))

      ;; fixed a weird issue where toggling display does not
      ;; swtich to text mode
      (defadvice doc-view-toggle-display
          (around spacemacs/doc-view-toggle-display activate)
        (if (eq major-mode 'doc-view-mode)
            (progn
              ad-do-it
              (text-mode)
              (doc-view-minor-mode))
          ad-do-it)))))

(defun spacemacs-ui/init-flx-ido ()
  (use-package flx-ido
    :init (flx-ido-mode 1)))

(defun spacemacs-ui/init-info+ ()
  (use-package info+
    :defer t
    :init
    (progn
      (with-eval-after-load 'info
        (require 'info+))
      (setq Info-fontify-angle-bracketed-flag nil))))

(defun spacemacs-ui/init-open-junk-file ()
  (use-package open-junk-file
    :defer t
    :commands (open-junk-file)
    :init
    (setq open-junk-file-format (concat spacemacs-cache-directory "junk/%Y/%m/%d-%H%M%S."))
    (defun spacemacs/open-junk-file (&optional arg)
      "Open junk file using helm or ivy.

Interface choice depends on whether the `ivy' layer is used or
not.

When ARG is non-nil search in junk files."
      (interactive "P")
      (let* ((fname (format-time-string open-junk-file-format (current-time)))
             (rel-fname (file-name-nondirectory fname))
             (junk-dir (file-name-directory fname))
             (default-directory junk-dir))
        (cond ((and arg (configuration-layer/layer-usedp 'ivy))
               (spacemacs/counsel-search dotspacemacs-search-tools nil junk-dir))
              ((configuration-layer/layer-usedp 'ivy)
               (require 'counsel)
               (counsel-find-file rel-fname))
              (arg
               (require 'helm)
               (let (helm-ff-newfile-prompt-p)
                 (spacemacs/helm-files-smart-do-search)))
              (t
               (require 'helm)
               (let (helm-ff-newfile-prompt-p)
                 (helm-find-files-1 fname))))))
    (spacemacs/set-leader-keys "fJ" 'spacemacs/open-junk-file)))

(defun spacemacs-ui/init-paradox ()
  (use-package paradox
    :commands paradox-list-packages
    :init
    (progn
      (setq paradox-execute-asynchronously nil)
      (defun spacemacs/paradox-list-packages ()
        "Load depdendencies for auth and open the package list."
        (interactive)
        (require 'epa-file)
        (require 'auth-source)
        (when (and (not (boundp 'paradox-github-token))
                   (file-exists-p "~/.authinfo.gpg"))
          (let ((authinfo-result (car (auth-source-search
                                       :max 1
                                       :host "github.com"
                                       :port "paradox"
                                       :user "paradox"
                                       :require '(:secret)))))
            (let ((paradox-token (plist-get authinfo-result :secret)))
              (setq paradox-github-token (if (functionp paradox-token)
                                             (funcall paradox-token)
                                           paradox-token)))))
        (paradox-list-packages nil))

      (evilified-state-evilify paradox-menu-mode paradox-menu-mode-map
        "H" 'paradox-menu-quick-help
        "J" 'paradox-next-describe
        "K" 'paradox-previous-describe
        "L" 'paradox-menu-view-commit-list
        "o" 'paradox-menu-visit-homepage)
      (spacemacs/set-leader-keys
        "ak" 'spacemacs/paradox-list-packages))))

(defun spacemacs-ui/init-restart-emacs()
  (use-package restart-emacs
    :defer t
    :init
    (defun spacemacs/restart-emacs (&optional args)
      "Restart emacs."
      (interactive)
      (setq spacemacs-really-kill-emacs t)
      (restart-emacs args))
    (defun spacemacs/restart-emacs-resume-layouts (&optional args)
      "Restart emacs and resume layouts."
      (interactive)
      (spacemacs/restart-emacs (cons "--resume-layouts" args)))
    (defun spacemacs/restart-emacs-debug-init (&optional args)
      "Restart emacs and enable debug-init."
      (interactive)
      (spacemacs/restart-emacs (cons "--debug-init" args)))
    (defun spacemacs/restart-stock-emacs-with-packages (packages &optional args)
      "Restart emacs without the spacemacs configuration, enable
debug-init and load the given list of packages."
      (interactive
       (progn
         (unless package--initialized
           (package-initialize t))
         (let ((packages (append (mapcar 'car package-alist)
                                 (mapcar 'car package-archive-contents)
                                 (mapcar 'car package--builtins))))
           (setq packages (mapcar 'symbol-name packages))
           (let ((val (completing-read-multiple "Packages to load (comma separated): "
                                                packages nil t)))
             `(,val)))))
      (let ((load-packages-string (mapconcat (lambda (pkg) (format "(use-package %s)" pkg))
                                             packages " ")))
        (spacemacs/restart-emacs-debug-init
         (append (list "-q" "--execute"
                       (concat "(progn (package-initialize) "
                               "(require 'use-package)"
                               load-packages-string ")"))
                 args))))
    (spacemacs/set-leader-keys
      "qd" 'spacemacs/restart-emacs-debug-init
      "qD" 'spacemacs/restart-stock-emacs-with-packages
      "qr" 'spacemacs/restart-emacs-resume-layouts
      "qR" 'spacemacs/restart-emacs)))

(defun spacemacs-ui/init-winum ()
  (use-package winum
    :config
    (progn
      (defun spacemacs//winum-assign-func ()
        "Custom number assignment for neotree."
        (when (and (boundp 'neo-buffer-name)
                   (string= (buffer-name) neo-buffer-name)
                   ;; in case there are two neotree windows. Example: when
                   ;; invoking a transient state from neotree window, the new
                   ;; window will show neotree briefly before displaying the TS,
                   ;; causing an error message. the error is eliminated by
                   ;; assigning 0 only to the top-left window
                   (eq (selected-window) (frame-first-window)))
          0))
      (setq winum-auto-assign-0-to-minibuffer nil
            winum-assign-func 'spacemacs//winum-assign-func
            winum-auto-setup-mode-line nil
            winum-ignored-buffers '(" *which-key*"))
      (spacemacs/set-leader-keys
        "`" 'winum-select-window-by-number
        "²" 'winum-select-window-by-number
        "0" 'winum-select-window-0-or-10
        "1" 'winum-select-window-1
        "2" 'winum-select-window-2
        "3" 'winum-select-window-3
        "4" 'winum-select-window-4
        "5" 'winum-select-window-5
        "6" 'winum-select-window-6
        "7" 'winum-select-window-7
        "8" 'winum-select-window-8
        "9" 'winum-select-window-9)
      (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
      (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
      (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
      (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
      (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
      (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
      (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
      (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
      (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
      (winum-mode))))
