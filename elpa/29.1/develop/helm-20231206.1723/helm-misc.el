;;; helm-misc.el --- Various functions for helm -*- lexical-binding: t -*-

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

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'helm-types)

(declare-function display-time-world-display "time.el")
(defvar display-time-world-list)
(declare-function LaTeX-math-mode "ext:latex.el")
(declare-function jabber-chat-with "ext:jabber.el")
(declare-function jabber-read-account "ext:jabber.el")
(declare-function helm-comp-read "helm-mode")


(defgroup helm-misc nil
  "Various Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-time-zone-home-location "Paris"
  "The time zone of your home."
  :group 'helm-misc
  :type 'string)

(defcustom helm-timezone-actions
  '(("Set timezone env (TZ)" . (lambda (candidate)
                                 (setenv "TZ" candidate))))
  "Actions for helm-timezone."
  :group 'helm-misc
  :type '(alist :key-type string :value-type function))

(defface helm-time-zone-current
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "green"))
  "Face used to colorize current time in `helm-world-time'."
  :group 'helm-misc)

(defface helm-time-zone-home
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "red"))
  "Face used to colorize home time in `helm-world-time'."
  :group 'helm-misc)



;;; Latex completion
;;
;; Test
;; (setq LaTeX-math-menu '("Math"
;; ["foo" val0 t]
;; ("bar"
;; ["baz" val1 t])
;; ("aze"
;; ["zer" val2 t])
;; ("AMS"
;; ("rec"
;; ["fer" val3 t])
;; ("rty"
;; ["der" val4 t]))
;; ("ABC"
;; ("xcv"
;; ["sdf" val5 t])
;; ("dfg"
;; ["fgh" val6 t]))))
;; (helm-latex-math-candidates)
;; =>
;; (("foo" . val0)
;; ("baz" . val1)
;; ("zer" . val2)
;; ("fer" . val3)
;; ("der" . val4)
;; ("sdf" . val5)
;; ("fgh" . val6))

(defvar LaTeX-math-menu)
(defun helm-latex-math-candidates ()
  (cl-labels ((helm-latex--math-collect (L)
                (cond ((vectorp L)
                       (list (cons (aref L 0) (aref L 1))))
                      ((listp L)
                       (cl-loop for a in L nconc
                                (helm-latex--math-collect a))))))
    (helm-latex--math-collect LaTeX-math-menu)))

(defvar helm-source-latex-math
  (helm-build-sync-source "Latex Math Menu"
    :init (lambda ()
            (with-helm-current-buffer
              (LaTeX-math-mode 1)))
    :candidate-number-limit 9999
    :candidates 'helm-latex-math-candidates
    :action (lambda (candidate)
              (call-interactively candidate))))


;;; Jabber Contacts (jabber.el)
(defun helm-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (cl-loop for item in (jabber-concat-rosters)
          when (get item 'connected)
          collect
          (if (get item 'name)
              (cons (get item 'name) item)
            (cons (symbol-name item) item)))))

(defvar helm-source-jabber-contacts
  (helm-build-sync-source "Jabber Contacts"
    :init (lambda () (require 'jabber))
    :candidates (lambda () (mapcar 'car (helm-jabber-online-contacts)))
    :action (lambda (x)
              (jabber-chat-with
               (jabber-read-account)
               (symbol-name
                (cdr (assoc x (helm-jabber-online-contacts))))))))

;;; World time
;;
(defvar zoneinfo-style-world-list)
(defvar legacy-style-world-list)

(defun helm-time-zone-transformer (candidates _source)
  (cl-loop for i in candidates
           for (z . p) in display-time-world-list
           collect
           (cons
            (cond ((string-match (format-time-string "%H:%M" (current-time)) i)
                   (propertize i 'face 'helm-time-zone-current))
                  ((string-match helm-time-zone-home-location i)
                   (propertize i 'face 'helm-time-zone-home))
                  (t i))
            z)))

(defvar helm-source-time-world
  (helm-build-in-buffer-source "Time World List"
    :init (lambda ()
            (require 'time)
            (unless (and display-time-world-list
                         (listp display-time-world-list))
              ;; adapted from `time--display-world-list' from
              ;; emacs-27 for compatibility as
              ;; `display-time-world-list' is set by default to t.
              (setq display-time-world-list
                    ;; Determine if zoneinfo style timezones are
                    ;; supported by testing that America/New York and
                    ;; Europe/London return different timezones.
                    (let ((nyt (format-time-string "%z" nil "America/New_York"))
                          (gmt (format-time-string "%z" nil "Europe/London")))
                      (if (string-equal nyt gmt)
                          legacy-style-world-list
                        zoneinfo-style-world-list)))))
    :data (lambda ()
            (with-temp-buffer
              (display-time-world-display display-time-world-list)
              (buffer-string)))
    :action 'helm-timezone-actions
    :filtered-candidate-transformer 'helm-time-zone-transformer))

;;; Commands
;;
(defun helm-call-interactively (cmd-or-name)
  "Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`helm-current-prefix-arg' is used as the command's prefix argument."
  (setq extended-command-history
        (cons (helm-stringify cmd-or-name)
              (delete (helm-stringify cmd-or-name) extended-command-history)))
  (let ((current-prefix-arg helm-current-prefix-arg)
        (cmd (helm-symbolify cmd-or-name)))
    (if (stringp (symbol-function cmd))
        (execute-kbd-macro (symbol-function cmd))
      (setq this-command cmd)
      (call-interactively cmd))))

;;; Minibuffer History
;;
;;
(defvar helm-minibuffer-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map [remap helm-minibuffer-history] 'undefined)
    map))

(defcustom helm-minibuffer-history-must-match t
  "Allow inserting non matching elements when nil or \\='confirm."
  :group 'helm-misc
  :type '(choice
          (const :tag "Must match" t)
          (const :tag "Confirm" confirm)
          (const :tag "Always allow" nil)))

(defcustom helm-minibuffer-history-key "C-r"
  "The key `helm-minibuffer-history' is bound to in minibuffer local maps."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :group 'helm-mode)

(defconst helm-minibuffer-history-old-key
  (cl-loop for map in '(minibuffer-local-completion-map
                        minibuffer-local-filename-completion-map
                        minibuffer-local-filename-must-match-map ; Emacs 23.1.+
                        minibuffer-local-isearch-map
                        minibuffer-local-map
                        minibuffer-local-must-match-filename-map ; Older Emacsen
                        minibuffer-local-must-match-map
                        minibuffer-local-ns-map)
           when (and (boundp map) (symbol-value map))
           collect (cons map (lookup-key (symbol-value map) "\C-r"))))

;;;###autoload
(define-minor-mode helm-minibuffer-history-mode
    "Bind `helm-minibuffer-history-key' in al minibuffer maps.
This mode is enabled by `helm-mode', so there is no need to enable it directly."
  :group 'helm-misc
  :global t
  (if helm-minibuffer-history-mode
      (let ((key helm-minibuffer-history-key))
        (dolist (map '(minibuffer-local-completion-map
                          minibuffer-local-filename-completion-map
                          minibuffer-local-filename-must-match-map ; Emacs 23.1.+
                          minibuffer-local-isearch-map
                          minibuffer-local-map
                          minibuffer-local-must-match-filename-map ; Older Emacsen
                          minibuffer-local-must-match-map
                          minibuffer-local-ns-map))
          (let ((vmap (and (boundp map) (symbol-value map))))
            (when (keymapp vmap)
              (let ((val (and (boundp 'helm-minibuffer-history-key)
                              (symbol-value 'helm-minibuffer-history-key))))
                (when val
                  (define-key vmap
                      (if (stringp val) (read-kbd-macro val) val)
                    nil)))
              (when key
                (define-key (symbol-value map)
                    (if (stringp key) (read-kbd-macro key) key)
                  'helm-minibuffer-history))))))
    (dolist (map '(minibuffer-local-completion-map
                      minibuffer-local-filename-completion-map
                      minibuffer-local-filename-must-match-map
                      minibuffer-local-isearch-map
                      minibuffer-local-map
                      minibuffer-local-must-match-filename-map
                      minibuffer-local-must-match-map
                      minibuffer-local-ns-map))
      (let ((vmap (and (boundp map) (symbol-value map))))
        (when (keymapp vmap)
          (let ((val (and (boundp 'helm-minibuffer-history-key)
                          (symbol-value 'helm-minibuffer-history-key))))
            (when val
              (define-key vmap
                (if (stringp val) (read-kbd-macro val) val)
                (assoc-default map helm-minibuffer-history-old-key)))))))))


;;; Helm ratpoison UI
;;
;;
(defvar helm-source-ratpoison-commands
  (helm-build-in-buffer-source "Ratpoison Commands"
    :init 'helm-ratpoison-commands-init
    :action (helm-make-actions
             "Execute the command" 'helm-ratpoison-commands-execute)
    :display-to-real 'helm-ratpoison-commands-display-to-real
    :candidate-number-limit 999999))

(defun helm-ratpoison-commands-init ()
  (unless (helm-candidate-buffer)
    (with-current-buffer (helm-candidate-buffer 'global)
      ;; with ratpoison prefix key
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "<ratpoison> \\1: \\2"))
      (goto-char (point-max))
      ;; direct binding
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help top"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "\\1: \\2")))))

(defun helm-ratpoison-commands-display-to-real (display)
  (and (string-match ": " display)
       (substring display (match-end 0))))

(defun helm-ratpoison-commands-execute (candidate)
  (call-process "ratpoison" nil nil nil "-ic" candidate))

;;; Helm stumpwm UI
;;
;;
(defvar helm-source-stumpwm-commands
  (helm-build-in-buffer-source "Stumpwm Commands"
    :init 'helm-stumpwm-commands-init
    :action (helm-make-actions
             "Execute the command" 'helm-stumpwm-commands-execute)
    :candidate-number-limit 999999))

(defun helm-stumpwm-commands-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (save-excursion
      (call-process "stumpish" nil (current-buffer) nil "commands"))
    (while (re-search-forward "[ ]*\\([^ ]+\\)[ ]*\n?" nil t)
      (replace-match "\n\\1\n"))
    (delete-blank-lines)
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-max))))

(defun helm-stumpwm-commands-execute (candidate)
  (call-process "stumpish" nil nil nil  candidate))

;;;###autoload
(defun helm-world-time ()
  "Preconfigured `helm' to show world time.
Default action change TZ environment variable locally to emacs."
  (interactive)
  (helm-other-buffer 'helm-source-time-world "*helm world time*"))

;;;###autoload
(defun helm-insert-latex-math ()
  "Preconfigured helm for latex math symbols completion."
  (interactive)
  (helm-other-buffer 'helm-source-latex-math "*helm latex*"))

;;;###autoload
(defun helm-ratpoison-commands ()
  "Preconfigured `helm' to execute ratpoison commands."
  (interactive)
  (helm-other-buffer 'helm-source-ratpoison-commands
                     "*helm ratpoison commands*"))

;;;###autoload
(defun helm-stumpwm-commands()
  "Preconfigured helm for stumpwm commands."
  (interactive)
  (helm-other-buffer 'helm-source-stumpwm-commands
                     "*helm stumpwm commands*"))

;;;###autoload
(defun helm-minibuffer-history ()
  "Preconfigured `helm' for `minibuffer-history'."
  (interactive)
  (cl-assert (minibuffer-window-active-p (selected-window)) nil
             "Error: Attempt to use minibuffer history outside a minibuffer")
  (let* ((enable-recursive-minibuffers t)
         (query-replace-p (or (eq last-command 'query-replace)
                              (eq last-command 'query-replace-regexp)))
         (elm (helm-comp-read "Next element matching (regexp): "
                              (cl-loop for i in
                                       (symbol-value minibuffer-history-variable)
                                       unless (equal "" i) collect i into history
                                       finally return
                                       (if (consp (car history))
                                           (mapcar 'prin1-to-string history)
                                           history))
                              :header-name
                              (lambda (name)
                                (format "%s (%s)" name minibuffer-history-variable))
                              :buffer "*helm minibuffer-history*"
                              :must-match helm-minibuffer-history-must-match
                              :multiline t
                              :keymap helm-minibuffer-history-map
                              :allow-nest t)))
    ;; Fix Bug#1667 with emacs-25+ `query-replace-from-to-separator'.
    (when (and (boundp 'query-replace-from-to-separator) query-replace-p)
      (let ((pos (string-match "\0" elm)))
        (and pos
             (add-text-properties
              pos (1+ pos)
              `(display ,query-replace-from-to-separator separator t)
              elm))))
    (delete-minibuffer-contents)
    (insert elm)))


(provide 'helm-misc)

;;; helm-misc.el ends here
