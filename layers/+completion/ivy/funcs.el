;;; funcs.el --- Ivy Layer functions File for Space-macs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; Counsel

;;; async

(defvar space-macs--counsel-initial-cands-shown nil)

(defun space-macs//counsel-async-command (cmd)
  (let* ((counsel--process " *counsel*")
         (proc (get-process counsel--process))
         (buff (get-buffer counsel--process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))
    (setq proc (start-process-shell-command
                counsel--process
                counsel--process
                cmd))
    (setq space-macs--counsel-initial-cands-shown nil)
    (setq counsel--async-time (current-time))
    (set-process-sentinel proc #'counsel--async-sentinel)
    (set-process-filter proc #'space-macs//counsel-async-filter)))

(defun space-macs//counsel-async-filter (process str)
  (with-current-buffer (process-buffer process)
    (insert str))
  (when (or (null space-macs--counsel-initial-cands-shown)
            (time-less-p
             ;; 0.5s
             '(0 0 500000 0)
             (time-since counsel--async-time)))
    (let (size display-now)
      (with-current-buffer (process-buffer process)
        (goto-char (point-min))
        (setq size (- (buffer-size) (forward-line (buffer-size))))
        (when (and (null space-macs--counsel-initial-cands-shown)
                   (> size space-macs--counsel-initial-number-cand))
          (setq ivy--all-candidates
                (split-string (buffer-string) "\n" t))
          (setq display-now t)
          (setq space-macs--counsel-initial-cands-shown t)))
      (let ((ivy--prompt
             (ivy-add-prompt-count
              (format (ivy-state-prompt ivy-last)
                      size))))
        (if display-now
            (ivy--insert-minibuffer
             (ivy--format ivy--all-candidates))
          (ivy--insert-prompt))))
    (setq counsel--async-time (current-time))))

;;; find-file functions, leaving large file check to `space-macs/check-large-file'

(defun space-macs//counsel-find-file-action (x)
  "Find file X."
  (with-ivy-window
    (cond ((and counsel-find-file-speedup-remote
                (file-remote-p ivy--directory))
           (let ((find-file-hook nil))
             (find-file (expand-file-name x ivy--directory))))
          ((member (file-name-extension x) counsel-find-file-extern-extensions)
           (counsel-find-file-extern x))
          (t
           (switch-to-buffer (find-file-noselect (expand-file-name x ivy--directory) t nil t))))))

(defun space-macs/counsel-find-file (&optional initial-input)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (counsel--find-file-1
   "Find file: " initial-input
   #'space-macs//counsel-find-file-action
   'counsel-find-file))

(defun space-macs/counsel-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (require 'recentf)
  (recentf-mode)
  (ivy-read "Recentf: " (counsel-recentf-candidates)
            :action (lambda (f)
                      (with-ivy-window
                        (switch-to-buffer (find-file-noselect f t nil t))))
            :require-match t
            :caller 'counsel-recentf))

;;; search

(defvar space-macs--counsel-search-cmd)

;; see `counsel-ag-function'
(defun space-macs//make-counsel-search-function (tool)
  (let ((base-cmd (cdr (assoc-string tool space-macs--counsel-commands))))
    (lambda (string &optional _pred &rest _unused)
      "Grep in the current directory for STRING."
      ;; `ivy-more-chars' returns non-nil when more chars are needed,
      ;; minimal chars count is configurable via `ivy-more-chars-alist'
      (or (ivy-more-chars)
          (let* ((default-directory (ivy-state-directory ivy-last))
                 (args (if (string-match-p " -- " string)
                           (let ((split (split-string string " -- ")))
                             (prog1 (pop split)
                               (setq string (mapconcat #'identity split " -- "))))
                         ""))
                 (regex (counsel--elisp-to-pcre
                         (setq ivy--old-re
                               (ivy--regex string)))))
            (setq space-macs--counsel-search-cmd (format base-cmd args regex))
            (space-macs//counsel-async-command space-macs--counsel-search-cmd)
            nil)))))

(defun space-macs//counsel-save-in-buffer ()
  (interactive)
  (ivy-quit-and-run
    (let ((buf "*ivy results*"))
      (with-current-buffer (get-buffer-create buf)
        (erase-buffer)
        (dolist (c ivy--all-candidates)
          (insert c "\n"))
        (space-macs//gne-init-counsel))
      (pop-to-buffer buf))))

(defun space-macs//counsel-edit ()
  "Edit the current search results in a buffer using wgrep."
  (interactive)
  (run-with-idle-timer 0 nil 'space-macs/ivy-wgrep-change-to-wgrep-mode)
  (ivy-occur))

(defun space-macs//gne-init-counsel ()
  (with-current-buffer "*ivy results*"
    (setq space-macs--gne-min-line 1
          space-macs--gne-max-line
          (save-excursion
            (goto-char (point-max))
            (previous-line)
            (line-number-at-pos))
          space-macs--gne-line-func
          (lambda (c)
            (counsel-git-grep-action c))
          next-error-function 'space-macs/gne-next)))

(defun space-macs//counsel-search-add-extra-bindings (map)
  "Add extra counsel-search related keybindings to MAP, then return MAP.
See `space-macs/counsel-search' and `counsel-ag'."
  (define-key map (kbd "<f3>") 'space-macs//counsel-save-in-buffer)
  (define-key map (kbd "C-c C-e") 'space-macs//counsel-edit)
  map)

(defvar space-macs--counsel-map (space-macs//counsel-search-add-extra-bindings
                                (make-sparse-keymap)))

(defun space-macs/ivy--regex-plus (str)
  "Build a regex sequence from STR.
Same as `ivy--regex-plus', but with special consideration for
`space-macs/counsel-search', thus providing correct highlighting
in the search results. Can be used in `ivy-re-builders-alist',
for example by setting the variable's value to:
  ((t . space-macs/ivy--regex-plus))
"
  (if (and (eq (ivy-state-caller ivy-last) 'space-macs/counsel-search)
           (string-match-p " -- " str))
      (ivy--regex-plus (car (last (split-string str " -- "))))
    (ivy--regex-plus str)))

;; see `counsel-ag'
(defun space-macs/counsel-search
    (&optional tools use-initial-input initial-directory)
  "Search using the first available tool in TOOLS. Default tool
to try is grep. If INPUT is non nil, use the region or the symbol
around point as the initial input. If DIR is non nil start in
that directory."
  (interactive)
  (require 'counsel)
  (cl-letf* ((initial-input (if use-initial-input
                                (if (region-active-p)
                                    (buffer-substring-no-properties
                                     (region-beginning) (region-end))
                                  (thing-at-point 'symbol t))
                              ""))
             (tool (catch 'tool
                     (dolist (tool tools)
                       (when (and (assoc-string tool space-macs--counsel-commands)
                                  (executable-find tool))
                         (throw 'tool tool)))
                     (throw 'tool "grep")))
             (default-directory
               (or initial-directory (read-directory-name "Start from directory: ")))
             (display-directory
              (if (< (length default-directory)
                     space-macs--counsel-search-max-path-length)
                  default-directory
                (concat
                 "..." (substring default-directory
                                  (- (length default-directory)
                                     space-macs--counsel-search-max-path-length)
                                  (length default-directory))))))
    (cond ((string= tool "ag")
           (counsel-ag initial-input default-directory nil
                       (format "ag from [%s]: " display-directory)))
          ((string= tool "rg")
           (counsel-rg initial-input default-directory nil
                       (format "rg from [%s]: " display-directory)))
          (t
           (ivy-read
            (format "%s from [%s]: "
                    tool
                    display-directory)
            (space-macs//make-counsel-search-function tool)
            :initial-input (when initial-input (rxt-quote-pcre initial-input))
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :caller 'space-macs/counsel-search
            :keymap space-macs--counsel-map
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup)))))))

;;; Define search functions for each tool
(cl-loop
 for (tools tool-name) in '((dotspace-macs-search-tools "auto")
                            ((list "rg") "rg")
                            ((list "ag") "ag")
                            ((list "pt") "pt")
                            ((list "ack") "ack")
                            ((list "grep") "grep"))
 do
 (eval
  `(progn
     (defun ,(intern (format "space-macs/search-%s" tool-name)) ()
       ,(format
         "Use `space-macs/counsel-search' to search in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspace-macs-search-tools'."
                       tool-name))
       (interactive)
       (space-macs/counsel-search ,tools))
     (defun ,(intern (format "space-macs/search-%s-region-or-symbol"
                             tool-name)) ()
       ,(format
         "Use `space-macs/counsel-search' to search for
 the selected region or the symbol around point in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspace-macs-search-tools'."
                       tool-name))
       (interactive)
       (space-macs/counsel-search ,tools t))
     (defun ,(intern (format "space-macs/search-project-%s" tool-name)) ()
       ,(format
         "Use `space-macs/counsel-search' to search in the current
 project with %s." (if (string= tool-name "auto")
                       "a tool selected from `dotspace-macs-search-tools'."
                     tool-name))
       (interactive)
       (space-macs/counsel-search ,tools nil (projectile-project-root)))
     (defun ,(intern (format "space-macs/search-project-%s-region-or-symbol"
                             tool-name)) ()
       ,(format
         "Use `space-macs/counsel-search' to search for
 the selected region or the symbol around point in the current
 project with %s." (if (string= tool-name "auto")
                       "a tool selected from `dotspace-macs-search-tools'."
                     tool-name))
       (interactive)
       (space-macs/counsel-search ,tools t (projectile-project-root)))
     (defun ,(intern (format "space-macs/search-dir-%s" tool-name)) ()
       ,(format
         "Use `space-macs/counsel-search' to search in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspace-macs-search-tools'."
                       tool-name))
       (interactive)
       (space-macs/counsel-search ,tools nil default-directory))
     (defun ,(intern (format "space-macs/search-dir-%s-region-or-symbol" tool-name)) ()
       ,(format
         "Use `space-macs/counsel-search' to search for
 the selected region or the symbol around point in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspace-macs-search-tools'."
                       tool-name))
       (interactive)
       (space-macs/counsel-search ,tools t default-directory)))))

(defun space-macs/counsel-git-grep-region-or-symbol ()
  "Use `counsel-git-grep' to search for the selected region or
 the symbol around point in the current project with git grep."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (counsel-git-grep input)))

(defun space-macs/counsel-search-docs ()
  "Search space-macs docs using `space-macs/counsel-search'"
  (interactive)
  (space-macs/counsel-search dotspace-macs-search-tools
                            nil space-macs-docs-directory))

(defun space-macs//counsel-occur (&optional candidates)
  "Generate a custom occur buffer for `counsel-git-grep'."
  (ivy-occur-grep-mode)
  (setq default-directory (ivy-state-directory ivy-last))
  (let ((cands (or candidates ivy--old-cands))
        (inhibit-read-only t))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines
     (mapcar
      (lambda (cand) (concat "./" cand))
      cands))))

(defun space-macs/counsel-up-directory-no-error ()
  "`counsel-up-directory' ignoring errors."
  (interactive)
  (ignore-errors
    (call-interactively 'counsel-up-directory)))

(when (configuration-layer/package-used-p 'counsel)
  (with-eval-after-load 'counsel
    (defun space-macs/describe-mode ()
      "Dummy wrapper to prevent an key binding error from helm.

By default the e-macs leader is M-m, turns out that Helm does this:
   (cl-dolist (k (where-is-internal 'describe-mode global-map))
        (define-key map k 'helm-help))
after doing this:
   (define-key map (kbd \"M-m\") 'helm-toggle-all-marks)
So when Helm is loaded we get the error:
   Key sequence M-m h d m starts with non-prefix key M-m

To prevent this error we just wrap `describe-mode' to defeat the
 Helm hack."
      (interactive)
      (call-interactively 'describe-mode))))

(defun space-macs//counsel-with-git-grep (func x)
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (with-ivy-window
      (let ((file-name (match-string-no-properties 1 x))
            (line-number (match-string-no-properties 2 x)))
        (funcall func
                 (expand-file-name file-name (ivy-state-directory ivy-last)))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line-number)))
        (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (unless (eq ivy-exit 'done)
          (swiper--cleanup)
          (swiper--add-overlays (ivy--regex ivy-text)))))))

;;; org

;; see https://github.com/abo-abo/swiper/issues/177
(defun space-macs//counsel-org-ctrl-c-ctrl-c-org-tag ()
  "Hook for `org-ctrl-c-ctrl-c-hook' to use `counsel-org-tag'."
  (if (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
      (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
          (user-error "C-c C-c can do nothing useful at this location"))
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (cl-case type
        ;; When at a link, act according to the parent instead.
        (link (setq context (org-element-property :parent context))
              (setq type (org-element-type context)))
        ;; Unsupported object types: refer to the first supported
        ;; element or object containing it.
        ((bold code entity export-snippet inline-babel-call inline-src-block
               italic latex-fragment line-break macro strike-through subscript
               superscript underline verbatim)
         (setq context
               (org-element-lineage
                context '(radio-target paragraph verse-block table-cell)))))
      ;; For convenience: at the first line of a paragraph on the
      ;; same line as an item, apply function on that item instead.
      (when (eq type 'paragraph)
        (let ((parent (org-element-property :parent context)))
          (when (and (eq (org-element-type parent) 'item)
                     (= (line-beginning-position)
                        (org-element-property :begin parent)))
            (setq context parent type 'item))))

      ;; Act according to type of element or object at point.
      (cl-case type
        ((headline inlinetask)
         (save-excursion (goto-char (org-element-property :begin context))
                         (call-interactively 'counsel-org-tag)) t)))))

(defun space-macs/counsel-jump-in-buffer ()
  "Jump in buffer with `counsel-imenu' or `counsel-org-goto' if in org-mode"
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'counsel-org-goto)
    (t 'counsel-imenu))))


;;; Ivy

(defun space-macs//ivy-command-not-implemented-yet (key)
  (let ((-key key))
    (space-macs/set-leader-keys
      -key (lambda ()
             (interactive)
             (message (concat "The command usually bound to %s %s has "
                              "not been implemented for the `ivy' layer yet.")
                      dotspace-macs-leader-key -key)))))

(defun space-macs/ivy-available-repls ()
  "Show available repls."
  (interactive)
  (ivy-read "Repls: "
            (mapcar #'car space-macs-repl-list)
            :action (lambda (candidate)
                      (let ((repl (cdr (assoc candidate space-macs-repl-list))))
                        (require (car repl))
                        (call-interactively (cdr repl))))))

(defun space-macs/ivy-wgrep-change-to-wgrep-mode ()
  (interactive)
  (ivy-wgrep-change-to-wgrep-mode)
  (evil-normal-state))

;;; Evil

(defun space-macs/ivy-evil-registers ()
  "Show evil registers"
  (interactive)
  (let ((ivy-height 24))
    (ivy-read "Evil Registers:"
              (cl-loop for (key . val) in (evil-register-list)
                       collect (eval `(format "%s : %s" (propertize ,(char-to-string key) 'face 'font-lock-builtin-face)
                                              ,(or (and val
                                                        (stringp val)
                                                        (replace-regexp-in-string "\n" "^J" val))
                                                   ""))))
              :action #'space-macs/ivy-insert-evil-register)))

(defun space-macs/ivy-insert-evil-register (candidate)
  (insert (replace-regexp-in-string "\\^J" "\n"
                                    (substring-no-properties candidate 4))))

;;; Layouts

(defun space-macs/ivy-space-macs-layouts ()
  "Control Panel for Space-macs layouts. Has many actions.
If match is found
\(default) Select layout
c: Close Layout(s) <- mark with C-SPC to close more than one-window
k: Kill Layout(s)
n: Copy current layout
p: Create project layout

If match is not found
<enter> Creates layout

Closing doesn't kill buffers inside the layout while killing layouts does."
  (interactive)
  (ivy-read "Layouts: "
            (persp-names)
            :caller 'space-macs/ivy-space-macs-layouts
            :action 'space-macs//create-persp-with-home-buffer))

(defun space-macs/ivy-space-macs-layout-buffer ()
  "Switch to layout buffer using ivy."
  (interactive)
  (let (ivy-use-virtual-buffers)
    (with-persp-buffer-list ()
                            (call-interactively 'ivy-switch-buffer))))

(defun space-macs/ivy-space-macs-layout-close-other ()
  "Kills layouts without killing the buffers"
  (interactive)
  (ivy-read (format "Close layout [current %s]: "
                    (space-macs//current-layout-name))
            (persp-names)
            :action 'persp-kill-without-buffers))

(defun space-macs/ivy-space-macs-layout-kill-other ()
  "Kills layouts with all their buffers"
  (interactive)
  (ivy-read (format "Kill layout [current %s]: "
                    (space-macs//current-layout-name))
            (persp-names)
            :action 'persp-kill))


