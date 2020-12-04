;;; funcs.el --- Space-macs Navigation Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; ace-window

(defun space-macs/ace-delete-window (&optional arg)
  "Ace delete window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (require 'ace-window)
  (aw-select
   " Ace - Delete Window"
   (lambda (window)
     (when (equal '(4) arg)
       (with-selected-window window
         (space-macs/kill-this-buffer arg)))
     (aw-delete-window window))))

(defun space-macs/ace-kill-this-buffer (&optional arg)
  "Ace kill visible buffer in a window.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (require 'ace-window)
  (let (golden-ratio-mode)
    (aw-select
     " Ace - Kill buffer in Window"
     (lambda (window)
       (with-selected-window window
         (space-macs/kill-this-buffer arg))))))


;; auto-highlight symbol

(defun space-macs/goto-last-searched-ahs-symbol ()
  "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
  (interactive)
  (if space-macs-last-ahs-highlight-p
      (progn (goto-char (nth 1 space-macs-last-ahs-highlight-p))
             (space-macs/ahs-highlight-now-wrapper)
             (space-macs/symbol-highlight-transient-state/body))
    (message "No symbol has been searched for now.")))

(defun space-macs/integrate-evil-search (forward)
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

(defun space-macs/ensure-ahs-enabled-locally ()
  "Ensures ahs is enabled for the local buffer."
  (unless
      (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)
    ))

(defun space-macs/ahs-highlight-now-wrapper ()
  "Safe wrapper for ahs-highlight-now"
  (eval '(progn
           (space-macs/ensure-ahs-enabled-locally)
           (ahs-highlight-now)) nil))

(defun space-macs/enter-ahs-forward ()
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (setq space-macs--ahs-searching-forward t)
  (space-macs/quick-ahs-forward))

(defun space-macs/enter-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (setq space-macs--ahs-searching-forward nil)
  (space-macs/quick-ahs-forward))

(defun space-macs/quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (space-macs//quick-ahs-move t))

(defun space-macs/quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (space-macs//quick-ahs-move nil))

(defun space-macs//quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (if (eq forward space-macs--ahs-searching-forward)
      (progn
        (space-macs/integrate-evil-search t)
        (space-macs/ahs-highlight-now-wrapper)
        (evil-set-jump)
        (space-macs/symbol-highlight-transient-state/body)
        (ahs-forward))
    (progn
      (space-macs/integrate-evil-search nil)
      (space-macs/ahs-highlight-now-wrapper)
      (evil-set-jump)
      (space-macs/symbol-highlight-transient-state/body)
      (ahs-backward))))

(defun space-macs/symbol-highlight ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (space-macs/ahs-highlight-now-wrapper)
  (setq space-macs-last-ahs-highlight-p (ahs-highlight-p))
  (space-macs/symbol-highlight-transient-state/body)
  (space-macs/integrate-evil-search t))

(defun space-macs//ahs-ts-on-exit ()
  ;; Restore user search direction state as ahs has exitted in a state
  ;; good for <C-s>, but not for 'n' and 'N'"
  (setq isearch-forward space-macs--ahs-searching-forward))

(defun space-macs/symbol-highlight-reset-range ()
  "Reset the range for `auto-highlight-symbol'."
  (interactive)
  (ahs-change-range ahs-default-range))

;; transient state
(defun space-macs//symbol-highlight-doc ()
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

(defun space-macs/ahs-to-iedit ()
  "Trigger iedit from ahs."
  (interactive)
  (cond
   ((and (not (eq dotspace-macs-editing-style 'e-macs))
         (configuration-layer/package-used-p 'evil-iedit-state))
    (evil-iedit-state/iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   ((and (eq dotspace-macs-editing-style 'e-macs)
         (configuration-layer/package-used-p 'iedit))
    (iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   (t (ahs-edit-mode t))))

(defun space-macs//symbol-highlight-ts-doc ()
  (space-macs//transient-state-make-doc
   'symbol-highlight
   (format space-macs--symbol-highlight-transient-state-doc
           (space-macs//symbol-highlight-doc))))


;; symbol overlay

(defun space-macs/symbol-overlay ()
  "Start symbol-overlay-transient-state."
  (interactive)
  (symbol-overlay-put)
  (space-macs/symbol-overlay-transient-state/body))

(defun space-macs//symbol-overlay-doc ()
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

(defun space-macs//symbol-overlay-ts-doc ()
  (space-macs//transient-state-make-doc
   'symbol-overlay
   (format space-macs--symbol-overlay-transient-state-doc
           (space-macs//symbol-overlay-doc))))


;; golden ratio

(defun space-macs/no-golden-ratio-for-buffers (bufname)
  "Disable golden-ratio if BUFNAME is the name of a visible buffer."
  (and (get-buffer bufname) (get-buffer-window bufname 'visible)))

(defun space-macs/no-golden-ratio-guide-key ()
  "Disable golden-ratio for guide-key popwin buffer."
  (or (space-macs/no-golden-ratio-for-buffers " *guide-key*")
      (space-macs/no-golden-ratio-for-buffers " *popwin-dummy*")))


;; smooth scrolling

(defun space-macs/enable-smooth-scrolling ()
  "Enable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 101))

(defun space-macs/disable-smooth-scrolling ()
  "Disable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 0))


;; ace-link

(defvar space-macs--link-pattern "~?/.+\\|\s\\[")

(defun space-macs//collect-space-macs-buffer-links ()
  (let ((end (window-end))
        points)
    (save-excursion
      (goto-char (window-start))
      (while (re-search-forward space-macs--link-pattern end t)
        (push (+ (match-beginning 0) 1) points))
      (nreverse points))))

(defun space-macs/ace-buffer-links ()
  "Ace jump to links in `space-macs' buffer."
  (interactive)
  (require 'avy)
  (let ((res (avy-with space-macs/ace-buffer-links
               (avy--process
                (space-macs//collect-space-macs-buffer-links)
                #'avy--overlay-pre))))
    (when (numberp res)
      (goto-char (1+ res))
      (widget-button-press (point)))))


;; doc-view

(defun space-macs/doc-view-search-new-query ()
  "Initiate a new query."
  (interactive)
  (doc-view-search 'newquery))

(defun space-macs/doc-view-search-new-query-backward ()
  "Initiate a new query."
  (interactive)
  (doc-view-search 'newquery t))

(defun space-macs/doc-view-goto-page (&optional count)
  (interactive (list
                (when current-prefix-arg
                  (prefix-numeric-value current-prefix-arg))))
  (if (null count)
      (doc-view-last-page)
    (doc-view-goto-page count)))


;; junk-file

(defun space-macs/open-junk-file (&optional arg)
  "Open junk file using helm or ivy.

Interface choice depends on whether the `ivy' layer is used or
not.

When ARG is non-nil search in junk files."
  (interactive "P")
  (let* ((fname (format-time-string open-junk-file-format (current-time)))
         (rel-fname (file-name-nondirectory fname))
         (junk-dir (file-name-directory fname))
         (default-directory junk-dir))
    (make-directory junk-dir t)
    (cond ((and arg (configuration-layer/layer-used-p 'ivy))
           (space-macs/counsel-search dotspace-macs-search-tools nil junk-dir))
          ((configuration-layer/layer-used-p 'ivy)
           (require 'counsel)
           ;; HACK: If major-mode is dired, counsel will use
           ;; (dired-current-directory) instead of default-directory. So, trick
           ;; counsel by shadowing major-mode.
           (let ((major-mode nil))
             (counsel-find-file rel-fname)))
          (arg
           (require 'helm)
           (let (helm-ff-newfile-prompt-p)
             (space-macs/helm-files-smart-do-search)))
          (t
           (require 'helm)
           (let (helm-ff-newfile-prompt-p)
             (helm-find-files-1 fname))))))


;; paradox

(defun space-macs/paradox-list-packages ()
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


;; restart-e-macs

(defun space-macs/restart-e-macs (&optional args)
  "Restart e-macs."
  (interactive)
  (setq space-macs-really-kill-e-macs t)
  (restart-e-macs args))

(defun space-macs/restart-e-macs-resume-layouts (&optional args)
  "Restart e-macs and resume layouts."
  (interactive)
  (space-macs/restart-e-macs (cons "--resume-layouts" args)))

(defun space-macs/restart-e-macs-debug-init (&optional args)
  "Restart e-macs and enable debug-init."
  (interactive)
  (space-macs/restart-e-macs (cons "--debug-init" args)))

(defun space-macs/restart-e-macs-timed-requires (&optional args)
  "Restart e-macs and time loads / requires."
  (interactive)
  (space-macs/restart-e-macs (cons "--timed-requires" args)))

(defun space-macs/restart-e-macs-adv-timers (&optional args)
  "Restart e-macs and time loads / requires and space-macs configuration."
  (interactive)
  (space-macs/restart-e-macs (cons "--adv-timers" args)))

(defun space-macs/restart-stock-e-macs-with-packages (packages &optional args)
  "Restart e-macs without the space-macs configuration, enable
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
    (space-macs/restart-e-macs-debug-init
     (append (list "-q" "--execute"
                   (concat "(progn (package-initialize) "
                           "(require 'use-package)"
                           load-packages-string ")"))
             args))))


