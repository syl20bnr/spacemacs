;;; helm-fd.el --- helm interface for fd command line tool. -*- lexical-binding: t -*-

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

(require 'helm)
(require 'helm-types)

(declare-function ansi-color-apply "ansi-color.el")
(declare-function split-string-shell-command "shell.el")

(defvar helm-fd-executable "fd"
  "The fd shell command executable.")

(defcustom helm-fd-switches '("--no-ignore" "--hidden" "--type" "f" "--type" "d" "--color" "always")
  "A list of options to pass to fd shell command."
  :type '(repeat string)
  :group 'helm-files)

(defcustom helm-fd-mode-line-function 'helm-fd-default-mode-line
  "Function called when `fd' process is finished to format mode-line."
  :type 'function
  :group 'helm-files)

(defface helm-fd-finish
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Green"))
  "Face used in mode line when fd process ends."
  :group 'helm-grep-faces)

(defvar helm-fd-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    (define-key map (kbd "C-]")      'undefined)
    (define-key map (kbd "DEL")      'helm-delete-backward-no-update)
    (define-key map (kbd "M-<down>") 'helm-fd-next-directory)
    (define-key map (kbd "M-<up>")   'helm-fd-previous-directory)
    map))

(defun helm-fd-next-directory-1 (arg)
  (with-helm-window
    (let ((cur-dir (helm-basedir (helm-get-selection))))
      (while (equal cur-dir (helm-basedir (helm-get-selection)))
        (if (> arg 0)
            (helm-next-line)
          (helm-previous-line))))))

(defun helm-fd-next-directory ()
  "Move to next directory in a helm-fd source."
  (interactive)
  (with-helm-alive-p
    (helm-fd-next-directory-1 1)))

(defun helm-fd-previous-directory ()
  "Move to previous directory in a helm-fd source."
  (interactive)
  (with-helm-alive-p
    (helm-fd-next-directory-1 -1)))

(defclass helm-fd-class (helm-source-async)
  ((candidates-process :initform 'helm-fd-process)
   (requires-pattern :initform 2)
   (candidate-number-limit :initform 20000)
   (nohighlight :initform t)
   (help-message :initform 'helm-fd-help-message)
   (filtered-candidate-transformer :initform 'helm-fd-fct)
   (action :initform 'helm-type-file-actions)
   (keymap :initform 'helm-fd-map)))

(defun helm-fd-process ()
  "Initialize fd process in an helm async source."
  (let* (process-connection-type
         (cmd (append helm-fd-switches
                      (or (and (fboundp #'split-string-shell-command)
                               (split-string-shell-command helm-pattern))
                          (split-string helm-pattern))))
         (proc (apply #'start-process "fd" nil helm-fd-executable cmd))
         (start-time (float-time))
         (fd-version (replace-regexp-in-string
                      "\n" ""
                      (shell-command-to-string
                       (concat helm-fd-executable " --version")))))
    (helm-log "helm-fd-process" "Fd command:\nfd %s"
              (mapconcat 'identity cmd " "))
    (helm-log "helm-fd-process" "VERSION: %s" fd-version)
    (prog1
        proc
      (set-process-sentinel
       proc (lambda (_process event)
              (if (string= event "finished\n")
                  (with-helm-window
                    (when helm-fd-mode-line-function
                      (funcall helm-fd-mode-line-function start-time fd-version)
                      (force-mode-line-update)))
                (helm-log "helm-fd-process sentinel" "Error: Fd %s"
                          (replace-regexp-in-string "\n" "" event))))))))

(defun helm-fd-default-mode-line (start-time fd-version)
  "Format mode-line with START-TIME and FD-VERSION, as well as `fd' results."
  (setq mode-line-format
        `(" " mode-line-buffer-identification " "
          (:eval (format "L%s" (helm-candidate-number-at-point))) " "
          (:eval (propertize
                  (format
                   "[%s process finished in %.2fs - (%s results)] "
                   ,fd-version
                   ,(- (float-time) start-time)
                   (helm-get-candidate-number))
                  'face 'helm-fd-finish)))))

(defun helm-fd-fct (candidates _source)
  "The filtered-candidate-transformer function for helm-fd."
  (cl-loop for i in candidates
           collect (ansi-color-apply i)))

(defun helm-fd-1 (directory)
  "Run fd shell command on DIRECTORY with helm interface."
  (cl-assert (executable-find helm-fd-executable) nil "Could not find fd executable")
  (cl-assert (not (file-remote-p directory)) nil "Fd not supported on remote directories")
  (let ((default-directory directory))
    (helm :sources (helm-make-source
                       (format "fd (%s)"
                               (abbreviate-file-name default-directory))
                       'helm-fd-class)
          :buffer "*helm fd*")))


(provide 'helm-fd)

;;; helm-fd.el ends here
