;;; helm-global-bindings.el --- Bind global helm commands -*- lexical-binding: t -*-

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

(require 'helm-lib) ; For helm-aif (bug #2520).


;;; Command Keymap
;;
;;
(defgroup helm-global-bindings nil
  "Global bindings for Helm."
  :group 'helm)

(defcustom helm-command-prefix-key
  (helm-aif (car (where-is-internal 'Control-X-prefix (list global-map)))
      (concat it [?c]))
  "The prefix key used to call Helm commands from the `global-map'.

Its default value is `C-x c'.
This key is bound to the function `helm-command-prefix' in the global map.
The definition of `helm-command-prefix' is the keymap `helm-command-map'.
Using `setq' to modify this variable will have no effect."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :set
  (lambda (var key)
    (helm-aif (and (boundp var) (symbol-value var))
        (global-unset-key (read-kbd-macro it)))
    (when key
      (global-set-key (read-kbd-macro key) 'helm-command-prefix))
    (set var key)))

(defvar helm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a")         'helm-apropos)
    (define-key map (kbd "e")         'helm-etags-select)
    (define-key map (kbd "l")         'helm-locate)
    (define-key map (kbd "L")         'helm-locate-library)
    (define-key map (kbd "s")         'helm-surfraw)
    (define-key map (kbd "r")         'helm-regexp)
    (define-key map (kbd "m")         'helm-man-woman)
    (define-key map (kbd "t")         'helm-top)
    (define-key map (kbd "/")         'helm-find)
    (define-key map (kbd "i")         'helm-imenu)
    (define-key map (kbd "I")         'helm-imenu-in-all-buffers)
    (define-key map (kbd "<tab>")     'helm-lisp-completion-at-point)
    (define-key map (kbd "p")         'helm-list-emacs-process)
    (define-key map (kbd "C-x r b")   'helm-filtered-bookmarks)
    (define-key map (kbd "M-y")       'helm-show-kill-ring)
    (define-key map (kbd "C-c <SPC>") 'helm-all-mark-rings)
    (define-key map (kbd "C-x C-f")   'helm-find-files)
    (define-key map (kbd "f")         'helm-multi-files)
    (define-key map (kbd "C-:")       'helm-eval-expression-with-eldoc)
    (define-key map (kbd "C-,")       'helm-calcul-expression)
    (define-key map (kbd "M-x")       'helm-M-x)
    (define-key map (kbd "M-s o")     'helm-occur)
    (define-key map (kbd "M-g a")     'helm-do-grep-ag)
    (define-key map (kbd "c")         'helm-colors)
    (define-key map (kbd "F")         'helm-select-xfont)
    (define-key map (kbd "8")         'helm-ucs)
    (define-key map (kbd "C-c f")     'helm-recentf)
    (define-key map (kbd "C-c g")     'helm-google-suggest)
    (define-key map (kbd "h i")       'helm-info-at-point)
    (define-key map (kbd "h r")       'helm-info-emacs)
    (define-key map (kbd "h g")       'helm-info-gnus)
    (define-key map (kbd "h h")       'helm-documentation)
    (define-key map (kbd "C-x C-b")   'helm-buffers-list)
    (define-key map (kbd "C-x r i")   'helm-register)
    (define-key map (kbd "C-c C-x")   'helm-run-external-command)
    (define-key map (kbd "b")         'helm-resume)
    (define-key map (kbd "M-g i")     'helm-gid)
    (define-key map (kbd "@")         'helm-packages)
    map)
  "Default keymap for \\[helm-command-prefix] commands.
The normal global definition of the character \\[helm-command-prefix] indirects to this keymap.")

(fset 'helm-command-prefix helm-command-map)


;;; Menu

(require 'helm-easymenu)


;;; Provide

(provide 'helm-global-bindings)

;;; helm-global-bindings.el ends here
