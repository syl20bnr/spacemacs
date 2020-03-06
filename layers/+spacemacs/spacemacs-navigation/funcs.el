;;; funcs.el --- Spacemacs Navigation Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; ace-window

(defun spacemacs/ace-delete-window (&optional arg)
  "Ace delete window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (require 'ace-window)
  (aw-select
   " Ace - Delete Window"
   (lambda (window)
     (when (equal '(4) arg)
       (with-selected-window window
         (spacemacs/kill-this-buffer arg)))
     (aw-delete-window window))))

(defun spacemacs/ace-kill-this-buffer (&optional arg)
  "Ace kill visible buffer in a window.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (require 'ace-window)
  (let (golden-ratio-mode)
    (aw-select
     " Ace - Kill buffer in Window"
     (lambda (window)
       (with-selected-window window
         (spacemacs/kill-this-buffer arg))))))


;; auto-highlight symbol

(defun spacemacs/goto-last-searched-ahs-symbol ()
  "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
  (interactive)
  (if spacemacs-last-ahs-highlight-p
      (progn (goto-char (nth 1 spacemacs-last-ahs-highlight-p))
             (spacemacs/ahs-highlight-now-wrapper)
             (spacemacs/symbol-highlight-transient-state/body))
    (message "No symbol has been searched for now.")))

(defun spacemacs/integrate-evil-search (forward)
        ;; isearch-string is last searched item.  Next time
        ;; "n" is hit we will use this.
        (let* ((symbol (evil-find-thing forward 'symbol))
               (regexp (concat "\\<" symbol "\\>")))
          (setq isearch-string regexp
                isearch-regexp regexp
                evil-ex-search-pattern (evil-ex-make-search-pattern regexp)))
        ;; Next time "n" is hit, go the correct direction.
        (setq isearch-forward forward)
        (setq evil-ex-search-direction (if forward 'forward 'backward))
        ;; ahs does a case sensitive search.  We could set
        ;; this, but it would break the user's current
        ;; sensitivity settings.  We could save the setting,
        ;; then next time the user starts a search we could
        ;; restore the setting.
        ;;(setq case-fold-search nil)
        ;; Place the search term into the search rings.
        (isearch-update-ring isearch-string t)
        (evil-push-search-history isearch-string forward)
        ;; Use this search term for empty pattern "%s//replacement/"
        ;; Append case sensitivity
        (setq evil-ex-last-was-search nil
              evil-ex-substitute-pattern `(,(concat isearch-string "\\C")
                                           nil (0 0))))

(defun spacemacs/ensure-ahs-enabled-locally ()
  "Ensures ahs is enabled for the local buffer."
  (unless
      (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)
    ))

(defun spacemacs/ahs-highlight-now-wrapper ()
  "Safe wrapper for ahs-highlight-now"
  (eval '(progn
           (spacemacs/ensure-ahs-enabled-locally)
           (ahs-highlight-now)) nil))

(defun spacemacs/enter-ahs-forward ()
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (setq spacemacs--ahs-searching-forward t)
  (spacemacs/quick-ahs-forward))

(defun spacemacs/enter-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (setq spacemacs--ahs-searching-forward nil)
  (spacemacs/quick-ahs-forward))

(defun spacemacs/quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (spacemacs//quick-ahs-move t))

(defun spacemacs/quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (spacemacs//quick-ahs-move nil))

(defun spacemacs//quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (if (eq forward spacemacs--ahs-searching-forward)
      (progn
        (spacemacs/integrate-evil-search t)
        (spacemacs/ahs-highlight-now-wrapper)
        (evil-set-jump)
        (spacemacs/symbol-highlight-transient-state/body)
        (ahs-forward))
    (progn
      (spacemacs/integrate-evil-search nil)
      (spacemacs/ahs-highlight-now-wrapper)
      (evil-set-jump)
      (spacemacs/symbol-highlight-transient-state/body)
      (ahs-backward))))

(defun spacemacs/symbol-highlight ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (spacemacs/ahs-highlight-now-wrapper)
  (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))
  (spacemacs/symbol-highlight-transient-state/body)
  (spacemacs/integrate-evil-search t))

(defun spacemacs//ahs-ts-on-exit ()
  ;; Restore user search direction state as ahs has exitted in a state
  ;; good for <C-s>, but not for 'n' and 'N'"
  (setq isearch-forward spacemacs--ahs-searching-forward))

(defun spacemacs/symbol-highlight-reset-range ()
  "Reset the range for `auto-highlight-symbol'."
  (interactive)
  (ahs-change-range ahs-default-range))

;; transient state
(defun spacemacs//symbol-highlight-doc ()
        (let* ((i 0)
               (overlay-count (length ahs-overlay-list))
               (overlay (format "%s" (nth i ahs-overlay-list)))
               (current-overlay (format "%s" ahs-current-overlay))
               (st (ahs-stat))
               (plighter (ahs-current-plugin-prop 'lighter))
               (plugin (format "%s"
                               (cond ((string= plighter "HS")  "Display")
                                     ((string= plighter "HSA") "Buffer")
                                     ((string= plighter "HSD") "Function"))))
               (face (cond ((string= plighter "HS")  ahs-plugin-defalt-face)
                           ((string= plighter "HSA") ahs-plugin-whole-buffer-face)
                           ((string= plighter "HSD") ahs-plugin-bod-face))))
          (while (not (string= overlay current-overlay))
            (setq i (1+ i))
            (setq overlay (format "%s" (nth i ahs-overlay-list))))
          (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
                 (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" "")))
            (concat
             (propertize (format " %s " plugin) 'face face)
             (propertize (format " %s%s " x/y hidden) 'face
                         `(:foreground "#ffffff" :background "#000000"))))))

(defun spacemacs/ahs-to-iedit ()
  "Trigger iedit from ahs."
  (interactive)
  (cond
   ((and (not (eq dotspacemacs-editing-style 'emacs))
         (configuration-layer/package-used-p 'evil-iedit-state))
    (evil-iedit-state/iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   ((and (eq dotspacemacs-editing-style 'emacs)
         (configuration-layer/package-used-p 'iedit))
    (iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   (t (ahs-edit-mode t))))

(defun spacemacs//symbol-highlight-ts-doc ()
  (spacemacs//transient-state-make-doc
   'symbol-highlight
   (format spacemacs--symbol-highlight-transient-state-doc
           (spacemacs//symbol-highlight-doc))))


;; symbol overlay

(defun spacemacs/symbol-overlay ()
  "Start symbol-overlay-transient-state."
  (interactive)
  (symbol-overlay-put)
  (spacemacs/symbol-overlay-transient-state/body))

(defun spacemacs//symbol-overlay-doc ()
        (let* ((symbol-at-point (symbol-overlay-get-symbol))
               (keyword (symbol-overlay-assoc symbol-at-point))
               (symbol (car keyword))
	             (before (symbol-overlay-get-list -1 symbol))
	             (after (symbol-overlay-get-list 1 symbol))
	             (count (length before))
               (scope (format "%s"
                              (if (cadr keyword)
                                  "Scope"
                                "Buffer")))
               (color (cddr keyword))
               (x/y (format "[%s/%s]" (+ count 1) (+ count (length after)))))
            (concat
             (propertize (format " %s " scope) 'face color))
             (propertize (format " %s " x/y) 'face
                         `(:foreground "#ffffff" :background "#000000"))))

(defun spacemacs//symbol-overlay-ts-doc ()
  (spacemacs//transient-state-make-doc
   'symbol-overlay
   (format spacemacs--symbol-overlay-transient-state-doc
           (spacemacs//symbol-overlay-doc))))


;; golden ratio

(defun spacemacs/no-golden-ratio-for-buffers (bufname)
  "Disable golden-ratio if BUFNAME is the name of a visible buffer."
  (and (get-buffer bufname) (get-buffer-window bufname 'visible)))

(defun spacemacs/no-golden-ratio-guide-key ()
  "Disable golden-ratio for guide-key popwin buffer."
  (or (spacemacs/no-golden-ratio-for-buffers " *guide-key*")
      (spacemacs/no-golden-ratio-for-buffers " *popwin-dummy*")))


;; smooth scrolling

(defun spacemacs/enable-smooth-scrolling ()
  "Enable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 101))

(defun spacemacs/disable-smooth-scrolling ()
  "Disable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 0))


;; ace-link

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
  (require 'avy)
  (let ((res (avy-with spacemacs/ace-buffer-links
               (avy--process
                (spacemacs//collect-spacemacs-buffer-links)
                #'avy--overlay-pre))))
    (when (numberp res)
      (goto-char (1+ res))
      (widget-button-press (point)))))


;; doc-view

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


;; junk-file

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
    (cond ((and arg (configuration-layer/layer-used-p 'ivy))
           (spacemacs/counsel-search dotspacemacs-search-tools nil junk-dir))
          ((configuration-layer/layer-used-p 'ivy)
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


;; paradox

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


;; restart-emacs

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

(defun spacemacs/restart-emacs-timed-requires (&optional args)
  "Restart emacs and time loads / requires."
  (interactive)
  (spacemacs/restart-emacs (cons "--timed-requires" args)))

(defun spacemacs/restart-emacs-adv-timers (&optional args)
  "Restart emacs and time loads / requires and spacemacs configuration."
  (interactive)
  (spacemacs/restart-emacs (cons "--adv-timers" args)))

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
