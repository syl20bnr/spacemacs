;;; evil-collection-cider.el --- Evil bindings for Cider -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, cider, tools

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
;; Evil bindings for Cider.

;;; Code:
(require 'cl-lib)
(require 'cider nil t)
(require 'evil-collection)

(declare-function cider-debug-mode-send-reply "cider-debug")

(defconst evil-collection-cider-maps '(cider-mode-map
                                       cider-repl-mode-map
                                       cider-repl-history-mode-map
                                       cider-test-report-mode-map
                                       cider-macroexpansion-mode-map
                                       cider-connections-buffer-mode-map))

(defun evil-collection-cider-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

(defmacro evil-collection-cider-make-debug-command (&rest cider-commands)
  "Make functions that wrap `cider-debug' commands.

Cider debug commands are sent through `cider-debug-mode-send-reply'.

ex. \(cider-debug-mode-send-reply \":next\"\)"
  (let ((commands (if (consp cider-commands)
                      cider-commands
                    (list cider-commands))))
    `(progn
       ,@(cl-loop
          for command in commands
          collect
          (let ((funsymbol
                 (intern (format "evil-collection-cider-debug-%s" command))))
            `(defun ,funsymbol ()
               ,(format
                 "Send :%s to `cider-debug-mode-send-reply'." command)
               (interactive)
               (cider-debug-mode-send-reply ,(format ":%s" command))))))))

(evil-collection-cider-make-debug-command "next"
                                          "continue"
                                          "continue-all"
                                          "out"
                                          "quit"
                                          "eval"
                                          "inject"
                                          "inspect"
                                          "locals"
                                          "in"
                                          "stacktrace")

;;;###autoload
(defun evil-collection-cider-setup ()
  "Set up `evil' bindings for `cider'."
  (unless evil-move-beyond-eol
    (advice-add 'cider-eval-last-sexp :around 'evil-collection-cider-last-sexp)
    (advice-add 'cider-eval-last-sexp-and-replace :around 'evil-collection-cider-last-sexp)
    (advice-add 'cider-eval-last-sexp-to-repl :around 'evil-collection-cider-last-sexp)
    (with-eval-after-load 'cider-eval-sexp-fu
      (advice-add 'cider-esf--bounds-of-last-sexp :around 'evil-collection-cider-last-sexp)))

  (when evil-collection-setup-debugger-keys
    (add-hook 'cider-mode-hook #'evil-normalize-keymaps)
    (add-hook 'cider--debug-mode-hook #'evil-normalize-keymaps)
    (evil-collection-define-key 'normal 'cider-mode-map
      [f6] 'cider-browse-instrumented-defs
      [f9] 'cider-debug-defun-at-point)

    (evil-collection-define-key 'normal 'cider--debug-mode-map
      "b" 'cider-debug-defun-at-point
      "n" 'evil-collection-cider-debug-next
      "c" 'evil-collection-cider-debug-continue
      "C" 'evil-collection-cider-debug-continue-all
      "o" 'evil-collection-cider-debug-out
      "q" 'evil-collection-cider-debug-quit
      "e" 'evil-collection-cider-debug-eval
      "J" 'evil-collection-cider-debug-inject
      "I" 'evil-collection-cider-debug-in
      "p" 'evil-collection-cider-debug-inspect
      "s" 'evil-collection-cider-debug-stacktrace
      "L" 'evil-collection-cider-debug-locals
      "H" 'cider-debug-move-here))

  (evil-collection-define-key '(normal visual) 'cider-mode-map
    "gd" 'cider-find-var
    (kbd "C-t") 'cider-pop-back
    "gz" 'cider-switch-to-repl-buffer
    "gf" 'cider-find-resource
    "K" 'cider-doc)

  (evil-collection-define-key '(normal visual) 'cider-repl-mode-map
    ;; FIXME: This seems to get overwritten by `cider-switch-to-repl-buffer'.
    "gz" 'cider-switch-to-last-clojure-buffer
    (kbd "RET") 'cider-repl-return

    "gd" 'cider-find-var
    (kbd "C-t") 'cider-pop-back
    "gr" 'cider-refresh
    "gf" 'cider-find-resource
    "K" 'cider-doc)

  (evil-collection-define-key '(normal visual) 'cider-repl-history-mode-map
    (kbd "C-k") 'cider-repl-history-previous
    (kbd "C-j") 'cider-repl-history-forward
    "gk" 'cider-repl-history-previous
    "gj" 'cider-repl-history-forward
    "[[" 'cider-repl-history-previous
    "]]" 'cider-repl-history-forward

    (kbd "RET") 'cider-repl-history-insert-and-quit
    "gr" 'cider-repl-history-update
    "q" 'cider-repl-history-quit
    "u" 'cider-repl-history-undo-other-window)

  (evil-collection-define-key 'normal 'cider-test-report-mode-map
    (kbd "C-c ,") 'cider-test-commands-map
    (kbd "C-c C-t") 'cider-test-commands-map
    (kbd "M-p") 'cider-test-previous-result
    (kbd "M-n") 'cider-test-next-result

    ;; goto
    "gd" 'cider-test-jump

    (kbd "<backtab>") 'cider-test-previous-result
    (kbd "<tab>") 'cider-test-next-result
    (kbd "RET") 'cider-test-jump
    "t" 'cider-test-jump
    "d" 'cider-test-ediff
    "e" 'cider-test-stacktrace
    "f" 'cider-test-rerun-failed-tests
    "n" 'cider-test-run-ns-tests
    "L" 'cider-test-run-loaded-tests
    "p" 'cider-test-run-project-tests
    "gr" 'cider-test-run-test
    "q" 'cider-popup-buffer-quit-function)

  (evil-collection-define-key 'normal 'cider-macroexpansion-mode-map
    ;; quit
    "q" 'cider-popup-buffer-quit-function

    "r" 'cider-macroexpand-again
    "K" 'cider-doc ; Evil has `evil-lookup'.
    "J" 'cider-javadoc
    "." 'cider-find-var
    "m" 'cider-macroexpand-1-inplace
    "a" 'cider-macroexpand-all-inplace
    "u" 'cider-macroexpand-undo
    [remap undo] 'cider-macroexpand-undo)

  (evil-collection-define-key 'normal 'cider-connections-buffer-mode-map
    "d" 'cider-connections-make-default
    "c" 'cider-connection-browser
    "x" 'cider-connections-close-connection
    (kbd "RET") 'cider-connections-goto-connection
    "g?" 'describe-mode)

  (evil-set-initial-state 'cider-stacktrace-mode 'normal)
  (evil-collection-define-key 'normal 'cider-stacktrace-mode-map
    (kbd "C-k") 'cider-stacktrace-previous-cause
    (kbd "C-j") 'cider-stacktrace-next-cause
    (kbd "gk") 'cider-stacktrace-previous-cause
    (kbd "gj") 'cider-stacktrace-next-cause
    (kbd "[[") 'cider-stacktrace-previous-cause
    (kbd "]]") 'cider-stacktrace-next-cause
    "gd" 'cider-stacktrace-jump
    "q" 'cider-popup-buffer-quit-function
    "J" 'cider-stacktrace-toggle-java
    "C" 'cider-stacktrace-toggle-clj
    "R" 'cider-stacktrace-toggle-repl
    "T" 'cider-stacktrace-toggle-tooling
    "D" 'cider-stacktrace-toggle-duplicates
    "P" 'cider-stacktrace-show-only-project
    "A" 'cider-stacktrace-toggle-all
    "1" 'cider-stacktrace-cycle-cause-1
    "2" 'cider-stacktrace-cycle-cause-2
    "3" 'cider-stacktrace-cycle-cause-3
    "4" 'cider-stacktrace-cycle-cause-4
    "5" 'cider-stacktrace-cycle-cause-5
    "0" 'cider-stacktrace-cycle-all-causes
    (kbd "TAB") 'cider-stacktrace-cycle-current-cause
    [backtab] 'cider-stacktrace-cycle-all-causes)

  (add-hook 'cider-inspector-mode-hook #'evil-normalize-keymaps)
  (evil-collection-define-key 'normal 'cider-inspector-mode-map
    "q" 'quit-window
    (kbd "RET") 'cider-inspector-operate-on-point
    [mouse-1] 'cider-inspector-operate-on-click
    "L" 'cider-inspector-pop
    "gr" 'cider-inspector-refresh
    ;; Page-up/down
    (kbd "C-j") 'cider-inspector-next-page
    (kbd "C-k") 'cider-inspector-prev-page
    " " 'cider-inspector-next-page
    "s" 'cider-inspector-set-page-size
    (kbd "]]") 'cider-inspector-next-inspectable-object
    (kbd "[[") 'cider-inspector-previous-inspectable-object
    "gj" 'cider-inspector-next-inspectable-object
    "gk" 'cider-inspector-previous-inspectable-object))

(provide 'evil-collection-cider)
;;; evil-collection-cider.el ends here
