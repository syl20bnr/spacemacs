;;; funcs.el --- Spacemacs Navigation Layer functions File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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
;;
;; Parts of this file are used with permission under the terms of other
;; GPL-compatible licenses. Specifically, the functions
;; `spacemacs//ediff-in-comparison-buffer-p' and
;; `spacemacs/ediff-balance-windows' are included under the terms of the MIT
;; license: <https://github.com/roman/golden-ratio.el/blob/master/LICENSE>



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
  (if (bound-and-true-p spacemacs-last-ahs-highlight-p)
      (progn (goto-char (nth 1 spacemacs-last-ahs-highlight-p))
             (spacemacs//ahs-setup)
             (spacemacs/symbol-highlight-transient-state/body))
    (message "No previously searched for symbol found")))

(defun spacemacs//integrate-evil-search (forward)
  "Set relevant variables for repeating the search with 'n' or 'N'
after having left the Symbol Highlight Transient State.

This function has to handle both possible values of `evil-search-module':
'isearch' and 'evil-search'."
  ;; Due to the new `user-error' in `spacemacs//ahs-setup', a `let*'-form
  ;; would suffice here. However, the function in itself only
  ;; makes sense if there is a symbol at point, hence the `when-let*'.
  (when-let* ((symbol (thing-at-point 'symbol t))
              (regexp (concat "\\_<" symbol "\\_>")))
    (setq isearch-string regexp
          isearch-regexp regexp
          evil-ex-search-pattern (evil-ex-make-search-pattern regexp))
    ;; Set the search direction.
    ;; `isearch-forward' is the only variable that has to be set when leaving
    ;; the transient state because it can get changed when navigating using ahs.
    ;; The other variables could in principle be set earlier (for example in
    ;; `spacemacs//ahs-setup') because they would not change.
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
                                       nil (0 0)))))

(defun spacemacs//ahs-setup ()
  "Remember the `auto-highlight-symbol-mode' state,
before enabling it for the transient state.

This function always has to be called just before activating
the Symbol Highlight Transient State. (Setting it as the :on-enter
property of the transient state would not execute it early enough.)"
  (unless (thing-at-point 'symbol t)
    ;; Here it makes no sense to enter the transient state.
    ;; Alternatively, we could implement behaviour similar to
    ;; `evil-ex-search-unbounded-word-forward', i. e. move to
    ;; the next or previous symbol.
    (user-error "No symbol found at point"))
  (unless (bound-and-true-p auto-highlight-symbol-mode)
    (setq-local spacemacs//ahs-was-disabled t))
  (auto-highlight-symbol-mode)
  (ahs-highlight-now))

(defun spacemacs/enter-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol-mode' and enter the Symbol Highlight Transient State."
  (interactive)
  (setq spacemacs--ahs-searching-forward t)
  (spacemacs//ahs-setup)
  (spacemacs/symbol-highlight-transient-state/spacemacs/quick-ahs-forward))

(defun spacemacs/enter-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol-mode' and enter the Symbol Highlight Transient State."
  (interactive)
  (setq spacemacs--ahs-searching-forward nil)
  (spacemacs//ahs-setup)
  (spacemacs/symbol-highlight-transient-state/spacemacs/quick-ahs-forward))

(defun spacemacs/quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol-mode'.

This function should only be used as a head of the hydra defined by
the Symbol Highlight Transient State. Otherwise use `spacemacs/enter-ahs-forward'."
  (interactive)
  (spacemacs//quick-ahs-move t))

(defun spacemacs/quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol-mode'.

This function should only be used as a head of the hydra defined by
the Symbol Highlight Transient State. Otherwise use `spacemacs/enter-ahs-backward'."
  (interactive)
  (spacemacs//quick-ahs-move nil))

(defun spacemacs//quick-ahs-move (forward)
  "Go to the next or previous occurrence of symbol under point with
`auto-highlight-symbol-mode'."
  (evil-set-jump)
  ;; Prevent ahs from blocking when navigating quickly between occurrences.
  (ahs-highlight-now)
  (if (eq forward spacemacs--ahs-searching-forward)
      (ahs-forward)
    (ahs-backward)))

(defun spacemacs/symbol-highlight ()
  "Highlight the symbol under point with `auto-highlight-symbol-mode' and
enter the Symbol Highlight Transient State."
  (interactive)
  (setq spacemacs--ahs-searching-forward t)
  (spacemacs//remember-last-ahs-highlight)
  (spacemacs//ahs-setup)
  (spacemacs/symbol-highlight-transient-state/body))

(defun spacemacs//remember-last-ahs-highlight ()
  (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p)))

(defvar-local spacemacs//ahs-was-disabled t
  "This is used to disable `auto-highlight-symbol-mode',
when the Symbol Highlight Transient State is closed.
If ahs mode was disabled before a symbol was highlighted.")

(defun spacemacs//ahs-was-disabled-in-ahs-ts-exit-window-p ()
  (let ((prev-win (selected-window)))
    (select-window spacemacs//ahs-ts-exit-window)
    (prog1 spacemacs//ahs-was-disabled
      (select-window prev-win))))

(defvar spacemacs//ahs-ts-exit-window nil
  "Remember the selected window when the
Symbol Highlight Transient State is closed.

This is used to disable `auto-highlight-symbol-mode',
in the window where the Symbol Highlight Transient State was closed,
when the TS was closed by opening a prompt. For example:
 SPC SPC (or M-x)       ;; spacemacs/helm-M-x-fuzzy-matching
 SPC ?                  ;; helm-descbinds
 M-:                    ;; eval-expression
 :                      ;; evil-ex

ahs mode is only disabled if it was disabled before a symbol was highlighted.")

(defun spacemacs//ahs-ts-on-exit ()
  (setq spacemacs//ahs-ts-exit-window (selected-window))
  (spacemacs//integrate-evil-search spacemacs--ahs-searching-forward)
  (spacemacs//disable-symbol-highlight-after-ahs-ts-exit))

(defun spacemacs//disable-symbol-highlight-after-ahs-ts-exit ()
  "Disable `auto-highlight-symbol-mode' if it was disabled before a symbol was highlighted."
  (cond ((and (spacemacs//prompt-opened-from-ahs-ts-p)
              (spacemacs//ahs-was-disabled-in-ahs-ts-exit-window-p))
         (spacemacs//disable-ahs-mode-in-ahs-ts-exit-window))
        (spacemacs//ahs-was-disabled
         (spacemacs//disable-symbol-highlight))))

(defun spacemacs//prompt-opened-from-ahs-ts-p ()
  "Was a prompt opened (for example: M-x),
from the Symbol Highlight Transient State?"
  (not (eq spacemacs//ahs-ts-exit-window (selected-window))))

(defun spacemacs//disable-ahs-mode-in-ahs-ts-exit-window ()
  "Disable `auto-highlight-symbol-mode',
in the window where the Symbol Highlight Transient State was closed."
  (let ((prev-win (selected-window)))
    (select-window spacemacs//ahs-ts-exit-window)
    (spacemacs//disable-symbol-highlight)
    (setq spacemacs//ahs-ts-exit-window nil)
    (select-window prev-win)))

(defun spacemacs//disable-symbol-highlight ()
  (auto-highlight-symbol-mode -1)
  (setq-local spacemacs//ahs-was-disabled nil))

(defun spacemacs/symbol-highlight-reset-range ()
  "Reset the range for `auto-highlight-symbol-mode'."
  (interactive)
  (ahs-change-range ahs-default-range))

;; transient state
(defun spacemacs//symbol-highlight-doc ()
  (let* ((i 0)
         (overlay-list (ahs-overlay-list-window))
         (overlay-count (length overlay-list))
         (overlay (format "%s" (nth i overlay-list)))
         (current-overlay (format "%s" (ahs-current-overlay-window)))
         (st (ahs-stat))
         (plighter (ahs-current-plugin-prop 'lighter))
         (plugin (format "%s"
                         (cond ((string= plighter "HS")  "Display")
                               ((string= plighter "HSA") "Buffer")
                               ((string= plighter "HSD") "Function"))))
         (face (cond ((string= plighter "HS")  ahs-plugin-default-face)
                     ((string= plighter "HSA") ahs-plugin-whole-buffer-face)
                     ((string= plighter "HSD") ahs-plugin-bod-face))))
    (while (not (string= overlay current-overlay))
      (setq i (1+ i))
      (setq overlay (format "%s" (nth i overlay-list))))
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


;; ediff

(defun spacemacs//ediff-in-comparison-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is part of an ediff comparison."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'ediff-this-buffer-ediff-sessions)
         ediff-this-buffer-ediff-sessions)))

(defun spacemacs/ediff-balance-windows ()
  "Balance the width of ediff windows."
  (interactive)
  (ediff-toggle-split)
  (ediff-toggle-split))


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

(defun spacemacs//collect-spacemacs-buffer-links ()
  "Return a list of widget-button positions."
  (let (widget-button-positions)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) (window-end))
        (when (eq (car (get-char-property-and-overlay (point) 'face))
                  'widget-button)
          (push (point) widget-button-positions))
        (goto-char (next-overlay-change (point)))))
    (nreverse widget-button-positions)))

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
  "Create a junk file with the initial name that's based on the variable
`open-junk-file-format'
`~/.emacs.d/.cache/junk/%Y/%m/%d-%H%M%S.'

Or erase the name and open an existing junk file.

When ARG is non-nil, search in the junk files.

The interface depends on the current completion layer:
compleseus
helm
ivy"
  (interactive "P")
  (let* ((fname (format-time-string open-junk-file-format (current-time)))
         (rel-fname (file-name-nondirectory fname))
         (junk-dir (file-name-directory fname))
         (default-directory junk-dir))
    (make-directory junk-dir t)
    (cond ((and arg (configuration-layer/layer-used-p 'compleseus))
           (cond ((executable-find "rg") (consult-ripgrep junk-dir))
                 ((executable-find "grep") (consult-grep junk-dir))
                 (t (message "Couldn't find either executable: rg or grep"))))
          ((configuration-layer/layer-used-p 'compleseus)
           (find-file
            (completing-read
             junk-dir
             (directory-files junk-dir nil directory-files-no-dot-files-regexp)
             nil nil rel-fname)))
          ((and arg (configuration-layer/layer-used-p 'ivy))
           (spacemacs/counsel-search dotspacemacs-search-tools nil junk-dir))
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
  (let ((spacemacs-really-kill-emacs t))
    (restart-emacs args)))

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
