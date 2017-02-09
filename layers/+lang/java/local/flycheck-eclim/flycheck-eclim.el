;; flycheck-eclim.el --- an interface to the Eclipse IDE. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015 Łukasz Klich
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
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
;;; Contributors
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name external program invocations
;; "eclim/command-name", like eclim/project-list.
;;; Description
;;
;; flycheck-eclim.el -- flycheck extension to use error/warning information
;; provided by eclim
;;

(require 'flycheck)
(require 'eclim)
(require 'json)

(defun eclim--problems-for-file ()
  (when (buffer-modified-p)
    (let ((backup-inhibited t)
          auto-save-default)
      (save-buffer 0)))
  (eclim/execute-command "project_build" "-p")
  (setq eclim--problems-filter nil)
  (eclim--problems-mode-init t)
  (eclim-problems-buffer-refresh)
  eclim--problems-list)

(defun flycheck-verify-eclim ()
  "Verify the Eclim syntax checker"
  (list
   (flycheck-verification-result-new
    :label "Eclim Mode"
    :message (if eclim-mode "Enabled" "Disabled")
    :face (if eclim-mode 'success '(bold warning)))))

(defun flycheck-eclim-parse-problems (problems checker)
  "Parse Eclim PROBLEMS for CHECKER into Flycheck errors."
  (mapcar (lambda (p) (eclim-problem-->-flycheck-error p checker)) problems))

(defun flycheck-eclim-start (checker callback)
  "Start a syntax CHECKER with Eclim."
  (condition-case err
      (let* ((problems (eclim--problems-for-file))
             (errors (flycheck-eclim-parse-problems problems checker)))
        (funcall callback 'finished errors))
    (error (funcall callback 'errored (error-message-string err)))))

(defun eclim-problem-->-flycheck-error (problem checker)
  (let* ((warning? (cdr (assoc 'warning problem)))
         (severity (if (equal warning? :json-false) 'error 'warning)))
    (flycheck-error-new-at
     (cdr (assoc 'line problem))
     (cdr (assoc 'column problem))
     severity
     (cdr (assoc 'message problem))
     :checker checker
     :filename (cdr (assoc 'filename problem))
     :buffer (current-buffer))))

(flycheck-define-generic-checker 'java-eclim
  "A Java syntax checker using Eclim.
See URL `https://github.com/senny/emacs-eclim'."
  :start #'flycheck-eclim-start
  :verify #'flycheck-verify-eclim
  :modes '(java-mode)
  :predicate (lambda () (and eclim-mode)))

(defun flycheck-eclim-setup ()
  "Setup Flycheck for Eclim."
  (interactive)
  (add-to-list 'flycheck-checkers 'java-eclim))

(provide 'flycheck-eclim)
