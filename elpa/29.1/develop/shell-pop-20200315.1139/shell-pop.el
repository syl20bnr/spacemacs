;;; shell-pop.el --- helps you to use shell easily on Emacs. Only one key action to work. -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017  Kazuo Yagi

;; Author:        Kazuo YAGI <kazuo.yagi@gmail.com>
;; Maintainer:    Kazuo YAGI <kazuo.yagi@gmail.com>
;; URL:           http://github.com/kyagi/shell-pop-el
;; Version:       0.64
;; Created:       2009-05-31 23:57:08
;; Keywords:      shell, terminal, tools
;; Compatibility: GNU 24.x
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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
;; This is a utility which helps you pop up and pop out shell buffer
;; window easily.  Just do M-x shell-pop, and it is strongly recommended
;; to assign one hot-key to this function.
;;
;; I hope this is useful for you, and ENJOY YOUR HAPPY HACKING!

;;; Configuration:
;;
;; Use M-x customize-variable RET `shell-pop-shell-type' RET to
;; customize the shell to use.  Four pre-set options are: `shell',
;; `terminal', `ansi-term', and `eshell'.  You can also set your
;; custom shell if you use other configuration.

;; For `terminal' and `ansi-term' options, you can set the underlying
;; shell by customizing `shell-pop-term-shell'.  By default,
;; `shell-file-name' is used.
;;
;; Use M-x customize-group RET shell-pop RET to set further options
;; such as hotkey, window height and position.

;;; Code:

(eval-when-compile
  (defvar shell-pop-universal-key)
  (defvar eshell-last-input-start)
  (defvar eshell-last-input-end))

(declare-function eshell-send-input "esh-mode")
(declare-function eshell-reset "esh-mode")
(declare-function eshell-process-interact "esh-proc")

(require 'term)
(require 'cl-lib)

(defgroup shell-pop ()
  "Shell-pop"
  :group 'shell)

;; internal{
(defvar shell-pop-internal-mode "shell")
(defvar shell-pop-internal-mode-buffer "*shell*")
(defvar shell-pop-internal-mode-func '(lambda () (shell)))
(defvar shell-pop-last-buffer nil)
(defvar shell-pop-last-window nil)
(defvar shell-pop-last-shell-buffer-index 1)
(defvar shell-pop-last-shell-buffer-name "")
(defvar shell-pop-window-configuration nil)
;; internal}

(defcustom shell-pop-window-size 30
  "Percentage for shell-buffer window size."
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (and (integerp x)
                            (<= x 100)
                            (<= 0 x)))))
  :group 'shell-pop)
(defvaralias 'shell-pop-window-height 'shell-pop-window-size)

(defcustom shell-pop-full-span nil
  "If non-nil, the shell spans full width of a window"
  :type 'boolean
  :group 'shell-pop)

(defcustom shell-pop-window-position "bottom"
  "Position of the popped buffer."
  :type '(choice
          (const "top")
          (const "bottom")
          (const "left")
          (const "right")
          (const "full"))
  :group 'shell-pop)

(defcustom shell-pop-default-directory nil
  "If non-nil, when first starting the shell, cd to this directory."
  :type 'directory
  :group 'shell-pop)

(defun shell-pop--set-shell-type (symbol value)
  (set-default symbol value)
  (setq shell-pop-internal-mode (nth 0 value)
        shell-pop-internal-mode-buffer (nth 1 value)
        shell-pop-internal-mode-func (nth 2 value))
  (when (and (string= shell-pop-internal-mode "ansi-term")
             shell-pop-universal-key)
    (define-key term-raw-map (read-kbd-macro shell-pop-universal-key) 'shell-pop)))

(defcustom shell-pop-shell-type '("shell" "*shell*" (lambda () (shell)))
  "Type of shell that is launched when first popping into a shell.

The value is a list with these items:
 - Internal name of the shell type.  This should be unique \"id\".
 - Name of the buffer this shell opens.
 - A function that launches the shell."
  :type '(choice
          (list :tag "Custom" string string function)
          (const :tag "shell"
                 ("shell" "*shell*" (lambda () (shell))))
          (const :tag "terminal"
                 ("terminal" "*terminal*" (lambda () (term shell-pop-term-shell))))
          (const :tag "ansi-term"
                 ("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
          (const :tag "eshell"
                 ("eshell" "*eshell*" (lambda () (eshell)))))
  :set 'shell-pop--set-shell-type
  :group 'shell-pop)

(defcustom shell-pop-term-shell shell-file-name
  "Shell used in `term' and `ansi-term'."
  :type 'string
  :group 'shell-pop)

(defcustom shell-pop-autocd-to-working-dir t
  "If non-nil, automatically `cd' to working directory of the
buffer from which the `shell-pop' command was invoked."
  :type 'boolean
  :group 'shell-pop)

(defcustom shell-pop-restore-window-configuration t
  "If non-nil, restore the original window configuration when
shell-pop is closed.

shell-pop's window is deleted in any case. This variable has no
effect when `shell-pop-window-position' value is \"full\"."
  :type 'boolean
  :group 'shell-pop)

(defcustom shell-pop-cleanup-buffer-at-process-exit t
  "If non-nil, cleanup the shell's buffer after its process exits."
  :type 'boolean
  :group 'shell-pop)

(defun shell-pop--set-universal-key (symbol value)
  (set-default symbol value)
  (when value (global-set-key (read-kbd-macro value) 'shell-pop))
  (when (and (string= shell-pop-internal-mode "ansi-term")
             shell-pop-universal-key)
    (define-key term-raw-map (read-kbd-macro value) 'shell-pop)))

;;;###autoload
(defcustom shell-pop-universal-key nil
  "Key binding used to pop in and out of the shell.

The input format is the same as that of `kbd'."
  :type '(choice string (const nil))
  :set 'shell-pop--set-universal-key
  :group 'shell-pop)

(defcustom shell-pop-in-hook nil
  "Hook run before buffer pop-up."
  :type 'hook
  :group 'shell-pop)

(defcustom shell-pop-in-after-hook nil
  "Hook run after buffer pop-up."
  :type 'hook
  :group 'shell-pop)

(defcustom shell-pop-out-hook nil
  "Hook run before buffer pop-out"
  :type 'hook
  :group 'shell-pop)

(defcustom shell-pop-process-exit-hook nil
  "Hook run when the shell's process exits."
  :type 'hook
  :group 'shell-pop)

(defun shell-pop--shell-buffer-name (index)
  (if (string-match-p "*\\'" shell-pop-internal-mode-buffer)
      (replace-regexp-in-string
       "*\\'" (format "-%d*" index) shell-pop-internal-mode-buffer)
    (format "%s-%d" shell-pop-internal-mode-buffer index)))

(defun shell-pop-check-internal-mode-buffer (index)
  (let ((bufname (shell-pop--shell-buffer-name index)))
    (when (get-buffer bufname)
      (if (or (term-check-proc bufname)
              (string= shell-pop-internal-mode "eshell"))
          bufname
        (kill-buffer bufname)
        nil))
    bufname))

(defun shell-pop-get-internal-mode-buffer-window (index)
  (get-buffer-window (shell-pop-check-internal-mode-buffer index)))

;;;###autoload
(defun shell-pop (arg)
  (interactive "P")
  (if (string= (buffer-name) shell-pop-last-shell-buffer-name)
      (if (null arg)
          (shell-pop-out)
        (shell-pop--switch-to-shell-buffer (prefix-numeric-value arg)))
    (shell-pop-up (or arg shell-pop-last-shell-buffer-index))))

(defun shell-pop--cd-to-cwd-eshell (cwd)
  (if (eshell-process-interact 'process-live-p)
      (message "Won't change CWD because of running process.")
    (setq default-directory cwd)
    (eshell-reset)))

(defun shell-pop--cd-to-cwd-shell (cwd)
  (goto-char (point-max))
  (comint-kill-input)
  (insert (concat "cd " (shell-quote-argument cwd)))
  (let ((comint-process-echoes t))
    (comint-send-input))
  (recenter 0))

(defun shell-pop--cd-to-cwd-term (cwd)
  (term-send-raw-string (concat "cd " (shell-quote-argument cwd) "\n"))
  (term-send-raw-string "\C-l"))

(defun shell-pop--cd-to-cwd (cwd)
  (let ((abspath (expand-file-name cwd)))
    (cond ((string= shell-pop-internal-mode "eshell")
           (shell-pop--cd-to-cwd-eshell abspath))
          ((string= shell-pop-internal-mode "shell")
           (shell-pop--cd-to-cwd-shell abspath))
          (t
           (shell-pop--cd-to-cwd-term abspath)))))

(defsubst shell-pop--full-p ()
  (string= shell-pop-window-position "full"))

(defsubst shell-pop--split-side-p ()
  (member shell-pop-window-position '("left" "right")))

(defun shell-pop--calculate-window-size ()
  (let* ((win (and shell-pop-full-span (frame-root-window)))
         (size (if (shell-pop--split-side-p)
                   (window-width)
                 (window-height win))))
    (round (* size (/ (- 100 shell-pop-window-height) 100.0)))))

(defun shell-pop--kill-and-delete-window ()
  (unless (one-window-p)
    (delete-window)))

(defun shell-pop--set-exit-action ()
  (if (string= shell-pop-internal-mode "eshell")
      (add-hook 'eshell-exit-hook 'shell-pop--kill-and-delete-window nil t)
    (let ((process (get-buffer-process (current-buffer))))
      (when process
        (set-process-sentinel
         process
         (lambda (_proc change)
           (when (string-match-p "\\(?:finished\\|exited\\)" change)
             (run-hooks 'shell-pop-process-exit-hook)
             (when shell-pop-cleanup-buffer-at-process-exit
               (kill-buffer))
             (if (one-window-p)
                 (switch-to-buffer shell-pop-last-buffer)
               (delete-window)))))))))

(defun shell-pop--switch-to-shell-buffer (index)
  (let ((bufname (shell-pop--shell-buffer-name index)))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (funcall (eval shell-pop-internal-mode-func))
      (rename-buffer bufname)
      (shell-pop--set-exit-action))
    (setq shell-pop-last-shell-buffer-name bufname
          shell-pop-last-shell-buffer-index index)))

(defun shell-pop--translate-position (pos)
  (cond
    ((string= pos "top") 'above)
    ((string= pos "bottom") 'below)
    ((string= pos "left") 'left)
    ((string= pos "right") 'right)))

(defun shell-pop-get-unused-internal-mode-buffer-window ()
  (let ((finish nil)
        (index 1)
        bufname)
    (while (not finish)
      (setq bufname (shell-pop--shell-buffer-name index))
      (if (get-buffer bufname)
          (setq index (1+ index))
        (setq finish t)))
    (cons index (get-buffer-window bufname))))

(defun shell-pop-up (index)
  (run-hooks 'shell-pop-in-hook)
  (let ((w (if (listp index)
               (let ((ret (shell-pop-get-unused-internal-mode-buffer-window)))
                 (setq index (car ret))
                 (cdr ret))
             (shell-pop-get-internal-mode-buffer-window index)))
        (cwd (replace-regexp-in-string "\\\\" "/" default-directory)))
    (when (shell-pop--full-p)
      (setq shell-pop-window-configuration
            (list (current-window-configuration) (point-marker)))
      (delete-other-windows))
    (if w
        (select-window w)
      ;; save shell-pop-last-buffer and shell-pop-last-window to return
      (setq shell-pop-last-buffer (buffer-name)
            shell-pop-last-window (selected-window))
      (when (and (not (= shell-pop-window-height 100))
                 (not (shell-pop--full-p)))
        (let ((new-window (shell-pop-split-window)))
          (select-window new-window)))
      (when (and shell-pop-default-directory (file-directory-p shell-pop-default-directory))
        (cd shell-pop-default-directory))
      (shell-pop--switch-to-shell-buffer index))
    (when (and shell-pop-autocd-to-working-dir
               (not (string= cwd default-directory)))
      (shell-pop--cd-to-cwd cwd))
    (run-hooks 'shell-pop-in-after-hook)))

(defun shell-pop-out ()
  (run-hooks 'shell-pop-out-hook)
  (if (shell-pop--full-p)
      (let ((window-conf (cl-first shell-pop-window-configuration))
            (marker (cl-second shell-pop-window-configuration)))
        (set-window-configuration window-conf)
        (when (marker-buffer marker)
          (goto-char marker)))
    (when (and (not (one-window-p)) (not (= shell-pop-window-height 100)))
      (bury-buffer)
      (delete-window)
      (select-window shell-pop-last-window))
    (when shell-pop-restore-window-configuration
      (switch-to-buffer shell-pop-last-buffer))))

(defun shell-pop-split-window ()
  (unless (shell-pop--full-p)
    (cond
     (shell-pop-full-span
      (split-window
       (frame-root-window) ; window
       (shell-pop--calculate-window-size) ; size
       (shell-pop--translate-position shell-pop-window-position))) ; side
     (t
      (split-window (selected-window) (shell-pop--calculate-window-size)
                    (shell-pop--translate-position shell-pop-window-position))))))

(provide 'shell-pop)

;;; shell-pop.el ends here
