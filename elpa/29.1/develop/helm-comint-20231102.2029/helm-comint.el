;;; helm-comint.el --- Comint prompt navigation for helm -*- lexical-binding: t -*-

;; Copyright (C) 2020 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Benedict Wang <foss@bhw.name>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (helm "3.9.4"))
;; Keywords: processes, matching
;; Homepage: https://github.com/benedicthw/helm-comint.git

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; You can bind this as follows in .emacs:
;;
;; (add-hook 'comint-mode-hook
;;           (lambda ()
;;               (define-key comint-mode-map (kbd "M-s f") 'helm-comint-prompts-all)))

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-lib)
(require 'helm-help)
(require 'helm-elisp)

;;; Comint prompts
;;
(defface helm-comint-prompts-promptidx
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       (:foreground "cyan")))
  "Face used to highlight comint prompt index."
  :group 'helm-comint-faces)

(defface helm-comint-prompts-buffer-name
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       (:foreground "green")))
  "Face used to highlight comint buffer name."
  :group 'helm-comint-faces)

(defcustom helm-comint-prompts-promptidx-p t
  "Show prompt number."
  :group 'helm-comint
  :type 'boolean)

(defcustom helm-comint-mode-list '(comint-mode slime-repl-mode sly-mrepl-mode sql-interactive-mode)
  "Supported modes for prompt navigation.
Derived modes (e.g., Geiser's REPL) are automatically supported."
  :group 'helm-comint
  :type '(repeat (choice symbol)))

(defcustom helm-comint-next-prompt-function '((sly-mrepl-mode . (lambda ()
                                                                  (sly-mrepl-next-prompt)
                                                                  (point))))
 "Alist of (MODE . NEXT-PROMPT-FUNCTION) to use.
If the current major mode is a key in this list, the associated
function will be used to navigate the prompts.
The function must return the point after the prompt.
Otherwise (comint-next-prompt 1) will be used."
  :group 'helm-comint
  :type '(alist :key-type symbol :value-type function))

(defcustom helm-comint-max-offset 400
  "Max number of chars displayed per candidate in `comint-input-ring' browser.
When t, don't truncate candidate, show all.
By default it is approximatively the number of bits contained in
five lines of 80 chars each i.e 80*5.
Note that if you set this to nil multiline will be disabled, i.e
you will not have anymore separators between candidates."
  :type '(choice (const :tag "Disabled" t)
          (integer :tag "Max candidate offset"))
  :group 'helm-misc)

(defvar helm-comint-prompts-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c C-o")   #'helm-comint-prompts-other-window)
    (define-key map (kbd "C-c C-f") #'helm-comint-prompts-other-frame)
    map)
  "Keymap for `helm-comint-prompt-all'.")

(defun helm-comint-prompts-list (mode &optional buffer)
  "List the prompts in BUFFER in mode MODE.

Return a list of (\"prompt\" (point) (buffer-name) prompt-index))
E.g. (\"ls\" 162 \"*shell*\" 3).
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p mode)
      (save-excursion
        (goto-char (point-min))
        (let (result (count 1))
          (save-mark-and-excursion
            (helm-awhile (and (not (eobp))
                              (helm-aif (alist-get major-mode helm-comint-next-prompt-function)
                                  (funcall it)
                                (comint-next-prompt 1)))
              (push (list (buffer-substring-no-properties
                           it (pos-eol))
                          it (buffer-name) count)
                    result)
              (setq count (1+ count))))
          (nreverse result))))))

(defun helm-comint-prompts-list-all (mode)
  "List the prompts of all buffers in mode MODE.
See `helm-comint-prompts-list'."
  (cl-loop for b in (buffer-list)
           append (helm-comint-prompts-list mode b)))

(defun helm-comint-prompts-transformer (candidates &optional all)
  "Transform comint prompts CANDIDATES.

Include prompt number if `helm-comint-prompts-promptidx-p' is
TRUE. When ALL is TRUE, all candidates are transformed. See
`helm-comint-prompts-list-all'.

E.g. (\"ls\" 162 \"*shell*\" 3) =>
     (\"*shell*:3:ls\" . (\"ls\" 162 \"*shell*\" 3))"
  (cl-loop for (prt pos buf id) in candidates
           collect `(,(concat
                       (when all
                         (concat (propertize
                                  buf
                                  'face 'helm-comint-prompts-buffer-name)
                                 ":"))
                       (when helm-comint-prompts-promptidx-p
                         (concat (propertize
                                  (number-to-string id)
                                  'face 'helm-comint-prompts-promptidx)
                                 ":"))
                       prt)
                      . ,(list prt pos buf id))))

(defun helm-comint-prompts-all-transformer (candidates)
  "Tranform all comint prompt CANDIDATES."
  (helm-comint-prompts-transformer candidates t))

(cl-defun helm-comint-prompts-goto (candidate &optional (action 'switch-to-buffer))
  "Switch the active buffer to the selected comint prompt.

CANDIDATE format: (\"ls\" 162 \"*shell*\" 3)
ACTION specifies if we should goto the other window or frame."
  (let ((buf (nth 2 candidate)))
    (unless (and (string= (buffer-name) buf)
                 (eq action 'switch-to-buffer))
      (funcall action buf))
    (goto-char (nth 1 candidate))
    (recenter)))

(defun helm-comint-prompts-goto-other-window (candidate)
  "Goto comint prompt CANDIDATE in other window."
  (helm-comint-prompts-goto candidate 'switch-to-buffer-other-window))

(defun helm-comint-prompts-goto-other-frame (candidate)
  "Goto comint prompt CANDIDATE in other frame."
  (helm-comint-prompts-goto candidate 'switch-to-buffer-other-frame))

(helm-make-command-from-action helm-comint-prompts-other-window
    "Switch to comint prompt in other window."
  'helm-comint-prompts-goto-other-window)

(helm-make-command-from-action helm-comint-prompts-other-frame
    "Switch to comint prompt in other frame."
  'helm-comint-prompts-goto-other-frame)

;;;###autoload
(defun helm-comint-prompts ()
  "Pre-configured `helm' to browse the prompts of the current comint buffer."
  (interactive)
  (if (apply #'derived-mode-p helm-comint-mode-list)
      (helm :sources
            (helm-build-sync-source "Comint prompts"
              :candidates (helm-comint-prompts-list major-mode)
              :candidate-transformer #'helm-comint-prompts-transformer
              :action '(("Go to prompt" . helm-comint-prompts-goto)))
            :buffer "*helm comint prompts*")
    (message "Current buffer is not a comint buffer")))

;;;###autoload
(defun helm-comint-prompts-all ()
  "Pre-configured `helm' to browse the prompts of all comint sessions."
  (interactive)
  (if (apply #'derived-mode-p helm-comint-mode-list)
      (helm :sources
            (helm-build-sync-source "All comint prompts"
              :candidates (helm-comint-prompts-list-all major-mode)
              :candidate-transformer #'helm-comint-prompts-all-transformer
              :action (quote (("Go to prompt" . helm-comint-prompts-goto)
                              ("Go to prompt in other window `C-c o`" .
                               helm-comint-prompts-goto-other-window)
                              ("Go to prompt in other frame `C-c C-o`" .
                               helm-comint-prompts-goto-other-frame)))
              :keymap helm-comint-prompts-keymap)
            :buffer "*helm comint all prompts*")
    (message "Current buffer is not a comint buffer")))

;;; Comint history
;;
;;
(defun helm-comint-input-ring-action (candidate)
  "Paste selected comint prompt CANDIDATE as the next comint prompt.
Default action for comint history."
  (with-helm-current-buffer
    (delete-region (comint-line-beginning-position) (point-max))
    (insert candidate)))

(defvar helm-comint-input-ring
  (helm-build-sync-source "Comint history"
    :candidates (lambda ()
                  (with-helm-current-buffer
                    (cl-loop for elm in (ring-elements comint-input-ring)
                             unless (string= elm "")
                             collect elm)))
    :action 'helm-comint-input-ring-action
    ;; Multiline does not work for `shell' because of an Emacs bug.
    ;; It works in other REPLs like Geiser.
    :multiline 'helm-comint-max-offset)
  "Source that provides Helm completion against `comint-input-ring'.")

;;;###autoload
(defun helm-comint-input-ring ()
  "Preconfigured `helm' that provide completion of `comint' history."
  (interactive)
  (when (or (derived-mode-p 'comint-mode)
            (member major-mode helm-comint-mode-list))
    (helm :sources 'helm-comint-input-ring
          :input (buffer-substring-no-properties (comint-line-beginning-position)
                                                 (pos-eol))
          :buffer "*helm comint history*")))

(provide 'helm-comint)

;;; helm-comint.el ends here
