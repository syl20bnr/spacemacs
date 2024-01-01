;;; funcs.el --- Ivy Layer functions File for Spacemacs -*- lexical-binding: t; -*-
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



;; Counsel

;;; async

(defvar spacemacs--counsel-initial-cands-shown nil)

(defun spacemacs//counsel-async-command (cmd)
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
    (setq spacemacs--counsel-initial-cands-shown nil)
    (setq counsel--async-time (current-time))
    (set-process-sentinel proc #'counsel--async-sentinel)
    (set-process-filter proc #'spacemacs//counsel-async-filter)))

(defun spacemacs//counsel-async-filter (process str)
  (with-current-buffer (process-buffer process)
    (insert str))
  (when (or (null spacemacs--counsel-initial-cands-shown)
            (time-less-p
             ;; 0.5s
             '(0 0 500000 0)
             (time-since counsel--async-time)))
    (let (size display-now)
      (with-current-buffer (process-buffer process)
        (goto-char (point-min))
        (setq size (- (buffer-size) (forward-line (buffer-size))))
        (when (and (null spacemacs--counsel-initial-cands-shown)
                   (> size spacemacs--counsel-initial-number-cand))
          (setq ivy--all-candidates
                (split-string (buffer-string) "\n" t))
          (setq display-now t)
          (setq spacemacs--counsel-initial-cands-shown t)))
      (let ((ivy--prompt
             (ivy-add-prompt-count
              (format (ivy-state-prompt ivy-last)
                      size))))
        (if display-now
            (ivy--insert-minibuffer
             (ivy--format ivy--all-candidates))
          (ivy--insert-prompt))))
    (setq counsel--async-time (current-time))))

;;; find-file functions, leaving large file check to `spacemacs/check-large-file'

(defun spacemacs//counsel-find-file-action (x)
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

(defun spacemacs/counsel-find-file (&optional initial-input)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (counsel--find-file-1
   "Find file: " initial-input
   #'spacemacs//counsel-find-file-action
   'counsel-find-file))

(defun spacemacs/counsel-recentf ()
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

(defvar spacemacs--counsel-search-cmd)

;; see `counsel-ag-function'
(defun spacemacs//make-counsel-search-function (tool)
  (let ((base-cmd (cdr (assoc-string tool spacemacs--counsel-commands))))
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
            (setq spacemacs--counsel-search-cmd (format base-cmd args regex))
            (spacemacs//counsel-async-command spacemacs--counsel-search-cmd)
            nil)))))

(defun spacemacs//counsel-save-in-buffer ()
  (interactive)
  (ivy-quit-and-run
    (let ((buf "*ivy results*"))
      (with-current-buffer (get-buffer-create buf)
        (erase-buffer)
        (dolist (c ivy--all-candidates)
          (insert c "\n"))
        (spacemacs//gne-init-counsel))
      (pop-to-buffer buf))))

(defun spacemacs//counsel-edit ()
  "Edit the current search results in a buffer using wgrep."
  (interactive)
  (run-with-idle-timer 0 nil 'spacemacs/ivy-wgrep-change-to-wgrep-mode)
  (ivy-occur))

(defun spacemacs//gne-init-counsel ()
  (with-current-buffer "*ivy results*"
    (setq spacemacs--gne-min-line 1
          spacemacs--gne-max-line
          (save-excursion
            (goto-char (point-max))
            (previous-line)
            (line-number-at-pos))
          spacemacs--gne-line-func
          (lambda (c)
            (counsel-git-grep-action c))
          next-error-function 'spacemacs/gne-next)))

(defun spacemacs//counsel-search-add-extra-bindings (map)
  "Add extra counsel-search related keybindings to MAP, then return MAP.
See `spacemacs/counsel-search' and `counsel-ag'."
  (define-key map (kbd "<f3>") 'spacemacs//counsel-save-in-buffer)
  (define-key map (kbd "C-c C-e") 'spacemacs//counsel-edit)
  map)

(defvar spacemacs--counsel-map (spacemacs//counsel-search-add-extra-bindings
                                (make-sparse-keymap)))

(defun spacemacs/ivy--regex-plus (str)
  "Build a regex sequence from STR.
Same as `ivy--regex-plus', but with special consideration for
`spacemacs/counsel-search', thus providing correct highlighting
in the search results. Can be used in `ivy-re-builders-alist',
for example by setting the variable's value to:
  ((t . spacemacs/ivy--regex-plus))
"
  (if (and (eq (ivy-state-caller ivy-last) 'spacemacs/counsel-search)
           (string-match-p " -- " str))
      (ivy--regex-plus (car (last (split-string str " -- "))))
    (ivy--regex-plus str)))

;; see `counsel-ag'
(defun spacemacs/counsel-search
    (&optional tools use-initial-input initial-directory)
  "Search using the first available tool in TOOLS. Default tool
to try is grep. If INPUT is non nil, use the region or the symbol
around point as the initial input. If DIR is non nil start in
that directory."
  (interactive)
  (require 'counsel)
  (cl-letf* ((initial-input (if use-initial-input
                                (rxt-quote-pcre
                                 (if (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   (or (thing-at-point 'symbol t) "")))
                              ""))
             (tool (catch 'tool
                     (dolist (tool tools)
                       (when (and (assoc-string tool spacemacs--counsel-commands)
                                  (executable-find tool))
                         (throw 'tool tool)))
                     (throw 'tool "grep")))
             (default-directory
               (or initial-directory (read-directory-name "Start from directory: ")))
             (display-directory
              (if (< (length default-directory)
                     spacemacs--counsel-search-max-path-length)
                  default-directory
                (concat
                 "..." (substring default-directory
                                  (- (length default-directory)
                                     spacemacs--counsel-search-max-path-length)
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
            (spacemacs//make-counsel-search-function tool)
            :initial-input (when initial-input (rxt-quote-pcre initial-input))
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :caller 'spacemacs/counsel-search
            :keymap spacemacs--counsel-map
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup)))))))

;;; Define search functions for each tool
(cl-loop
 for (tools tool-name) in '((dotspacemacs-search-tools "auto")
                            ((list "rg") "rg")
                            ((list "ag") "ag")
                            ((list "pt") "pt")
                            ((list "ack") "ack")
                            ((list "grep") "grep"))
 do
 (eval
  `(progn
     (defun ,(intern (format "spacemacs/search-%s" tool-name)) ()
       ,(format
         "Use `spacemacs/counsel-search' to search in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspacemacs-search-tools'."
                       tool-name))
       (interactive)
       (spacemacs/counsel-search ,tools))
     (defun ,(intern (format "spacemacs/search-%s-region-or-symbol"
                             tool-name)) ()
       ,(format
         "Use `spacemacs/counsel-search' to search for
 the selected region or the symbol around point in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspacemacs-search-tools'."
                       tool-name))
       (interactive)
       (spacemacs/counsel-search ,tools t))
     (defun ,(intern (format "spacemacs/search-project-%s" tool-name)) ()
       ,(format
         "Use `spacemacs/counsel-search' to search in the current
 project with %s." (if (string= tool-name "auto")
                       "a tool selected from `dotspacemacs-search-tools'."
                     tool-name))
       (interactive)
       (spacemacs/counsel-search ,tools nil (projectile-project-root)))
     (defun ,(intern (format "spacemacs/search-project-%s-region-or-symbol"
                             tool-name)) ()
       ,(format
         "Use `spacemacs/counsel-search' to search for
 the selected region or the symbol around point in the current
 project with %s." (if (string= tool-name "auto")
                       "a tool selected from `dotspacemacs-search-tools'."
                     tool-name))
       (interactive)
       (spacemacs/counsel-search ,tools t (projectile-project-root)))
     (defun ,(intern (format "spacemacs/search-dir-%s" tool-name)) ()
       ,(format
         "Use `spacemacs/counsel-search' to search in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspacemacs-search-tools'."
                       tool-name))
       (interactive)
       (spacemacs/counsel-search ,tools nil default-directory))
     (defun ,(intern (format "spacemacs/search-dir-%s-region-or-symbol" tool-name)) ()
       ,(format
         "Use `spacemacs/counsel-search' to search for
 the selected region or the symbol around point in the current
 directory with %s." (if (string= tool-name "auto")
                         "a tool selected from `dotspacemacs-search-tools'."
                       tool-name))
       (interactive)
       (spacemacs/counsel-search ,tools t default-directory)))))

(defun spacemacs/counsel-git-grep-region-or-symbol ()
  "Use `counsel-git-grep' to search for the selected region or
 the symbol around point in the current project with git grep."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (counsel-git-grep input)))

(defun spacemacs/counsel-search-docs ()
  "Search spacemacs docs using `spacemacs/counsel-search'"
  (interactive)
  (spacemacs/counsel-search dotspacemacs-search-tools
                            nil spacemacs-docs-directory))

(defun spacemacs//counsel-occur (&optional candidates)
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

(defun spacemacs/counsel-up-directory-no-error ()
  "`counsel-up-directory' ignoring errors."
  (interactive)
  (ignore-errors
    (call-interactively 'counsel-up-directory)))

(when (configuration-layer/package-used-p 'counsel)
  (with-eval-after-load 'counsel
    (defun spacemacs/describe-mode ()
      "Dummy wrapper to prevent an key binding error from helm.

By default the emacs leader is M-m, turns out that Helm does this:
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

(defun spacemacs//counsel-with-git-grep (func x)
  "This function should be kept in sync with `counsel-git-grep-action'.

We copy exactly that function and modify it a bit which allows us
to programatically add extra actions to counsel git-grep based
commands."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      ;; this line is the difference to `counsel-git-grep-action'
      (funcall func
               (expand-file-name file-name (ivy-state-directory ivy-last)))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (when swiper-goto-start-of-match
          (goto-char (match-beginning 0))))
      (swiper--ensure-visible)
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

;;; org

;; see https://github.com/abo-abo/swiper/issues/177
(defun spacemacs//counsel-org-ctrl-c-ctrl-c-org-tag ()
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

(defun spacemacs/counsel-jump-in-buffer ()
  "Jump in buffer with `counsel-imenu' or `counsel-org-goto' if in org-mode"
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'counsel-org-goto)
    (t 'counsel-imenu))))


;;; Ivy

(defun spacemacs//ivy-command-not-implemented-yet (key)
  (let ((-key key))
    (spacemacs/set-leader-keys
      -key (lambda ()
             (interactive)
             (message (concat "The command usually bound to %s %s has "
                              "not been implemented for the `ivy' layer yet.")
                      dotspacemacs-leader-key -key)))))

(defun spacemacs/ivy-available-repls ()
  "Show available repls."
  (interactive)
  (ivy-read "Repls: "
            (mapcar #'car spacemacs-repl-list)
            :action (lambda (candidate)
                      (let ((repl (cdr (assoc candidate spacemacs-repl-list))))
                        (require (car repl))
                        (call-interactively (cdr repl))))))

(defun spacemacs/ivy-wgrep-change-to-wgrep-mode ()
  (interactive)
  (ivy-wgrep-change-to-wgrep-mode)
  (evil-normal-state))

;;; Evil

(defun spacemacs/ivy-evil-registers ()
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
              :action #'spacemacs/ivy-insert-evil-register)))

(defun spacemacs/ivy-insert-evil-register (candidate)
  (insert (replace-regexp-in-string "\\^J" "\n"
                                    (substring-no-properties candidate 4))))

;;; Layouts

(defun spacemacs/ivy-spacemacs-layouts ()
  "Control Panel for Spacemacs layouts. Has many actions.
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
            :caller 'spacemacs/ivy-spacemacs-layouts
            :action 'spacemacs//create-persp-with-home-buffer))

(defun spacemacs/ivy-spacemacs-layout-buffer ()
  "Switch to layout buffer using ivy."
  (interactive)
  (let (ivy-use-virtual-buffers)
    (with-persp-buffer-list ()
                            (call-interactively 'ivy-switch-buffer))))

(defun spacemacs/ivy-spacemacs-layout-close-other ()
  "Kills layouts without killing the buffers"
  (interactive)
  (ivy-read (format "Close layout [current %s]: "
                    (spacemacs//current-layout-name))
            (persp-names)
            :action 'persp-kill-without-buffers))

(defun spacemacs/ivy-spacemacs-layout-kill-other ()
  "Kills layouts with all their buffers"
  (interactive)
  (ivy-read (format "Kill layout [current %s]: "
                    (spacemacs//current-layout-name))
            (persp-names)
            :action 'persp-kill))

(defun spacemacs/ivy-xref-open-in-other-window (candidate)
  "Open candidate in other window."
  (let* ((marker (xref-location-marker (cdr candidate)))
         (buf (marker-buffer marker)))
    (select-window
     (xref--show-pos-in-buf marker (switch-to-buffer-other-window buf)))))
