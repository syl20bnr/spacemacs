;;; helm-eshell.el --- pcomplete and eshell completion for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

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
;; Enable like this in .emacs:
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;               (eshell-cmpl-initialize)
;;               (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
;;               (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all)))
;;               (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))


;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-lib)
(require 'helm-help)
(require 'helm-elisp)

(declare-function eshell-read-aliases-list "em-alias")
(declare-function eshell-send-input "esh-mode" (&optional use-region queue-p no-newline))
(declare-function eshell-bol "esh-mode")
(declare-function eshell-parse-arguments "esh-arg" (beg end))
(declare-function eshell-backward-argument "esh-mode" (&optional arg))
(declare-function helm-quote-whitespace "helm-lib")
(declare-function eshell-skip-prompt "em-prompt")
(defvar eshell-special-chars-outside-quoting)


(defgroup helm-eshell nil
  "Helm completion and history for Eshell."
  :group 'helm)


(defcustom helm-eshell-fuzzy-match nil
  "Enable fuzzy matching in `helm-esh-pcomplete' when non-nil."
  :type 'boolean)


(defvar helm-eshell-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-p") #'helm-next-line)
    map)
  "Keymap for `helm-eshell-history'.")

(defvar helm-esh-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "TAB") #'helm-next-line)
    map)
  "Keymap for `helm-esh-pcomplete'.")

(defvar helm-eshell--quit-flag nil)


;; Internal.
(defvar helm-ec-target "")
(defun helm-ec-insert (_candidate)
  "Replace text at point with CANDIDATE.
The function that call this should set `helm-ec-target' to thing
at point."
  (set (make-local-variable 'comint-file-name-quote-list)
       eshell-special-chars-outside-quoting)
  (let ((pt (point)))
    (when (and helm-ec-target
               (search-backward helm-ec-target nil t)
               (string= (buffer-substring (point) pt) helm-ec-target))
      (delete-region (point) pt)))
  (when (string-match "\\`\\*" helm-ec-target) (insert "*"))
  (let ((marked (helm-marked-candidates)))
    (prog1 t ;; Makes helm returns t on action.
      (insert
       (mapconcat
        (lambda (x)
          (cond ((string-match "\\`~/" helm-ec-target)
                 ;; Strip out the first escape char added by
                 ;; `comint-quote-filename' before "~" (Bug#1803).
                 (substring (comint-quote-filename (abbreviate-file-name x)) 1))
                ((string-match "\\`/" helm-ec-target)
                 (comint-quote-filename x))
                (t
                 (concat (and (string-match "\\`[.]/" helm-ec-target) "./")
                         (comint-quote-filename
                          (file-relative-name x))))))
        marked " ")
       (or (helm-aand (car (last marked))
                      (string-match-p "/\\'" it)
                      "")
           " ")))))

(defun helm-esh-transformer (candidates _sources)
  (cl-loop
   for i in candidates
   collect
   (cond ((string-match "\\`~/?" helm-ec-target)
          (abbreviate-file-name i))
         ((string-match "\\`/" helm-ec-target) i)
         (t
          (file-relative-name i)))
   into lst
   finally return (sort lst #'helm-generic-sort-fn)))

(defclass helm-esh-source (helm-source-sync)
  ((init :initform (lambda ()
                     (setq pcomplete-current-completions nil
                           pcomplete-last-completion-raw nil)
                     ;; Eshell-command add this hook in all minibuffers
                     ;; Remove it for the helm one. (Fixed in Emacs24)
                     (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
   (candidates :initform 'helm-esh-get-candidates)
   ;(nomark :initform t)
   (persistent-action :initform 'ignore)
   (nohighlight :initform t)
   (filtered-candidate-transformer :initform #'helm-esh-transformer)
   (action :initform 'helm-ec-insert))
  "Helm class to define source for Eshell completion.")

(defun helm-esh-get-candidates ()
  "Get candidates for Eshell completion using `pcomplete'."
  (catch 'pcompleted
    (with-helm-current-buffer
      (let* ((pcomplete-stub)
             pcomplete-seen pcomplete-norm-func
             pcomplete-args pcomplete-last pcomplete-index
             (pcomplete-autolist pcomplete-autolist)
             (pcomplete-suffix-list pcomplete-suffix-list)
             (table (pcomplete-completions))
             (entry (or (try-completion helm-pattern
                                        (pcomplete-entries))
                        helm-pattern)))
        (cl-loop ;; expand entry too to be able to compare it with file-cand.
              with exp-entry = (and (stringp entry)
                                    (not (string= entry ""))
                                    (file-name-as-directory
                                     (expand-file-name entry default-directory)))
              with comps = (all-completions pcomplete-stub table)
              unless comps return (prog1 nil
                                    ;; Don't add final space when
                                    ;; there is no completion (Bug#1990).
                                    (setq helm-eshell--quit-flag t)
                                    (message "No completions of %s" pcomplete-stub))
              for i in comps
              ;; Transform the relative names to abs names.
              for file-cand = (and exp-entry
                                   (if (file-remote-p i) i
                                     (expand-file-name
                                      i (file-name-directory
                                         (if (directory-name-p pcomplete-stub)
                                             entry
                                           (directory-file-name entry))))))
              ;; Compare them to avoid dups.
              for file-entry-p = (and (stringp exp-entry)
                                      (stringp file-cand)
                                      ;; Fix :/tmp/foo/ $ cd foo
                                      (not (file-directory-p file-cand))
                                      (file-equal-p exp-entry file-cand))
              if (and file-cand (or (file-remote-p file-cand)
                                    (file-exists-p file-cand))
                      (not file-entry-p))
              collect file-cand into ls
              else
              ;; Avoid adding entry here.
              unless file-entry-p collect i into ls
              finally return
              (if (and exp-entry
                       (file-directory-p exp-entry)
                       ;; If the car of completion list is
                       ;; an executable, probably we are in
                       ;; command completion, so don't add a
                       ;; possible file related entry here.
                       (and ls (not (executable-find (car ls))))
                       ;; Don't add entry if already in prompt.
                       (not (file-equal-p exp-entry pcomplete-stub)))
                  (append (list exp-entry)
                          ;; Entry should not be here now but double check.
                          (remove entry ls))
                ls))))))

;;; Eshell history.
;;
;;
(defclass helm-eshell-history-source (helm-source-sync)
  ((init :initform
         (lambda ()
           ;; Same comment as in `helm-source-esh'.
           (remove-hook 'minibuffer-setup-hook 'eshell-mode)))
   (candidates
    :initform
    (lambda ()
      (with-helm-current-buffer
        (cl-loop for c from 0 to (ring-length eshell-history-ring)
                 for elm = (eshell-get-history c)
                 unless (and (member elm lst)
                             eshell-hist-ignoredups)
                 collect elm into lst
                 finally return lst))))
   (nomark :initform t)
   (multiline :initform t)
   (keymap :initform 'helm-eshell-history-map)
   (candidate-number-limit :initform 9999)
   (action :initform (lambda (candidate)
                       (eshell-kill-input)
                       (insert candidate))))
  "Helm class to define source for Eshell history.")


(defun helm-esh-pcomplete-input (target users-comp last)
  (if (and (stringp last)
           (not (string= last ""))
           (not users-comp)
           ;; Fix completion on "../" see Bug#1832.
           (or (file-exists-p last)
               (helm-aand
                (file-name-directory last)
                (file-directory-p it))))
      (if (and (file-directory-p last)
               (string-match "\\`[~.]*.*/[.]\\'" target))
          ;; Fix completion on "~/.", "~/[...]/.", and "../."
          (expand-file-name
           (concat (helm-basedir (file-name-as-directory last))
                   (regexp-quote (helm-basename target))))
        (expand-file-name last))
    ;; Don't add "~" to input to provide completion on all users instead of only
    ;; on current $HOME (#1832).
    (unless users-comp last)))

(defun helm-esh-pcomplete-default-source ()
  "Make and return the default source for Eshell completion."
  (helm-make-source "Eshell completions" 'helm-esh-source
    :fuzzy-match helm-eshell-fuzzy-match
    :keymap helm-esh-completion-map))

(defvar helm-esh-pcomplete-build-source-fn #'helm-esh-pcomplete-default-source
  "Function that builds a source or a list of sources.")

(defun helm-esh-pcomplete--make-helm (&optional input)
  (helm :sources (funcall helm-esh-pcomplete-build-source-fn)
        :buffer "*helm pcomplete*"
        :resume 'noresume
        :input input))

;;;###autoload
(defun helm-esh-pcomplete ()
  "Preconfigured `helm' to provide Helm completion in Eshell."
  (interactive)
  (let* ((helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         (end (point-marker))
         (beg (save-excursion (eshell-bol) (point)))
         (args (catch 'eshell-incomplete
                 (eshell-parse-arguments beg end)))
         (target
          (or (and (looking-back " " (1- (point))) " ")
              (buffer-substring-no-properties
               (save-excursion
                 (eshell-backward-argument 1) (point))
               end)))
         (users-comp (string= target "~"))
         (first (car args)) ; Maybe lisp delimiter "(".
         last ; Will be the last but parsed by pcomplete.
         del-space
         del-dot)
    (setq helm-ec-target (or target " ")
          end (point)
          ;; Reset beg for `with-helm-show-completion'.
          beg (or (and target (not (string= target " "))
                       (- end (length target)))
                  ;; Nothing at point.
                  (progn (insert " ") (setq del-space t) (point))))
    (when (string-match "\\`[~.]*.*/[.]\\'" target)
      ;; Fix completion on
      ;; "~/.", "~/[...]/.", and "../."
      (delete-char -1) (setq del-dot t)
      (setq helm-ec-target (substring helm-ec-target 0 (1- (length helm-ec-target)))))
    (cond ((eq first ?\()
           (helm-lisp-completion-at-point))
          ;; In eshell `pcomplete-parse-arguments' is called
          ;; with `pcomplete-parse-arguments-function'
          ;; locally bound to `eshell-complete-parse-arguments'
          ;; which is calling `lisp-complete-symbol',
          ;; calling it before would popup the
          ;; *completions* buffer.
          (t (setq last (replace-regexp-in-string
                         "\\`\\*" ""
                         (car (last (ignore-errors
                                      (pcomplete-parse-arguments))))))
             ;; Set helm-eshell--quit-flag to non-nil only on
             ;; quit, this tells to not add final suffix when quitting
             ;; helm.
             (add-hook 'helm-quit-hook #'helm-eshell--quit-hook-fn)
             (with-helm-show-completion beg end
               (unwind-protect
                   (or (helm-esh-pcomplete--make-helm
                        (helm-esh-pcomplete-input target users-comp last))
                       ;; Delete removed dot on quit
                       (and del-dot (prog1 t (insert ".")))
                       ;; A space is needed to have completion, remove
                       ;; it when nothing found.
                       (and del-space (looking-back "\\s-" (1- (point)))
                            (delete-char -1))
                       (if (and (null helm-eshell--quit-flag)
                                (and (stringp last) (file-directory-p last))
                                (looking-back "\\([.]\\{1,2\\}\\|[^/]\\)\\'"
                                              (1- (point))))
                           (prog1 t (insert "/"))
                         ;; We need another flag for space here, but
                         ;; global to pass it to `helm-quit-hook', this
                         ;; space is added when point is just after
                         ;; previous completion and there is no
                         ;; more completion, see Bug#1832.
                         (unless (or helm-eshell--quit-flag
                                     (looking-back "/\\'" (1- (point))))
                           (prog1 t (insert " ")))
                         (when (and helm-eshell--quit-flag
                                    (string-match-p "[.]\\{2\\}\\'" last))
                           (insert "/"))))
                 (remove-hook 'helm-quit-hook #'helm-eshell--quit-hook-fn)
                 (setq helm-eshell--quit-flag nil)))))))

(defun helm-eshell--quit-hook-fn ()
  (setq helm-eshell--quit-flag t))

;;;###autoload
(defun helm-eshell-history ()
  "Preconfigured Helm for Eshell history."
  (interactive)
  (let* ((end   (point))
         (beg   (save-excursion (eshell-bol) (point)))
         (input (buffer-substring beg end))
         flag-empty)
    (when (eq beg end)
      (insert " ")
      (setq flag-empty t)
      (setq end (point)))
    (unwind-protect
         (with-helm-show-completion beg end
           (helm :sources (helm-make-source "Eshell history"
                              'helm-eshell-history-source
                            :fuzzy-match helm-eshell-fuzzy-match)
                 :buffer "*helm eshell history*"
                 :resume 'noresume
                 :input input))
      (when (and flag-empty
                 (looking-back " " (1- (point))))
        (delete-char -1)))))


;;; Eshell prompts
;;
(defface helm-eshell-prompts-promptidx
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "cyan"))
  "Face used to highlight Eshell prompt index.")

(defface helm-eshell-prompts-buffer-name
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "green"))
  "Face used to highlight Eshell buffer name.")

(defcustom helm-eshell-prompts-promptidx-p t
  "Show prompt number."
  :type 'boolean)

(defvar helm-eshell-prompts-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o")   #'helm-eshell-prompts-other-window)
    (define-key map (kbd "C-c C-o") #'helm-eshell-prompts-other-frame)
    map)
  "Keymap for `helm-eshell-prompt-all'.")

(defvar eshell-prompt-regexp)
(defvar eshell-highlight-prompt)

(defun helm-eshell-prompts-list (&optional buffer)
  "List the prompts in Eshell BUFFER.

Return a list of (\"prompt\" (point) (buffer-name) prompt-index))
E.g. (\"ls\" 162 \"*eshell*\" 3).
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (eq major-mode 'eshell-mode)
      (save-excursion
        (goto-char (point-min))
        (let (result (count 1))
          (helm-awhile (re-search-forward eshell-prompt-regexp nil t)
            (when (or (and eshell-highlight-prompt
                           (get-text-property (match-beginning 0) 'read-only))
                      (null eshell-highlight-prompt))
              (push (list (buffer-substring-no-properties
                           it (pos-eol))
                          it (buffer-name) count)
                    result)
              (setq count (1+ count))))
          (nreverse result))))))

(defun helm-eshell-prompts-list-all ()
  "List the prompts of all Eshell buffers.
See `helm-eshell-prompts-list'."
  (cl-loop for b in (buffer-list)
           append (helm-eshell-prompts-list b)))

(defun helm-eshell-prompts-transformer (candidates &optional all)
  ;; ("ls" 162 "*eshell*" 3) => ("*eshell*:3:ls" . ("ls" 162 "*eshell*" 3))
  (cl-loop for (prt pos buf id) in candidates
           collect `(,(concat
                       (when all
                         (concat (propertize
                                  buf
                                  'face 'helm-eshell-prompts-buffer-name)
                                 ":"))
                       (when helm-eshell-prompts-promptidx-p
                         (concat (propertize
                                  (number-to-string id)
                                  'face 'helm-eshell-prompts-promptidx)
                                 ":"))
                       prt)
                      . ,(list prt pos buf id))))

(defun helm-eshell-prompts-all-transformer (candidates)
  (helm-eshell-prompts-transformer candidates t))

(cl-defun helm-eshell-prompts-goto (candidate &optional (action 'switch-to-buffer))
  ;; Candidate format: ("ls" 162 "*eshell*" 3)
  (let ((buf (nth 2 candidate)))
    (unless (and (string= (buffer-name) buf)
                 (eq action 'switch-to-buffer))
      (funcall action buf))
    (goto-char (nth 1 candidate))
    (recenter)))

(defun helm-eshell-prompts-goto-other-window (candidate)
  (helm-eshell-prompts-goto candidate 'switch-to-buffer-other-window))

(defun helm-eshell-prompts-goto-other-frame (candidate)
  (helm-eshell-prompts-goto candidate 'switch-to-buffer-other-frame))

(helm-make-command-from-action helm-eshell-prompts-other-window
    "Switch to eshell prompt in other window."
  'helm-eshell-prompts-goto-other-window)

(helm-make-command-from-action helm-eshell-prompts-other-frame
    "Switch to eshell prompt in other frame."
  'helm-eshell-prompts-goto-other-frame)

;;;###autoload
(defun helm-eshell-prompts ()
  "Pre-configured `helm' to browse the prompts of the current Eshell."
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (helm :sources
            (helm-build-sync-source "Eshell prompts"
              :candidates (helm-eshell-prompts-list)
              :candidate-transformer 'helm-eshell-prompts-transformer
              :action '(("Go to prompt" . helm-eshell-prompts-goto)))
            :buffer "*helm Eshell prompts*")
    (message "Current buffer is not an Eshell buffer")))

;;;###autoload
(defun helm-eshell-prompts-all ()
  "Pre-configured `helm' to browse the prompts of all Eshell sessions."
  (interactive)
  (helm :sources
        (helm-build-sync-source "All Eshell prompts"
          :candidates (helm-eshell-prompts-list-all)
          :candidate-transformer 'helm-eshell-prompts-all-transformer
          :action '(("Go to prompt" . helm-eshell-prompts-goto)
                    ("Go to prompt in other window `C-c o`" .
                     helm-eshell-prompts-goto-other-window)
                    ("Go to prompt in other frame `C-c C-o`" .
                     helm-eshell-prompts-goto-other-frame))
          :keymap helm-eshell-prompts-keymap)
        :buffer "*helm Eshell all prompts*"))

(provide 'helm-eshell)

;;; helm-eshell ends here
