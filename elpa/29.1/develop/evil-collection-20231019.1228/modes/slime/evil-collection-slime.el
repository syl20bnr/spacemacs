;;; evil-collection-slime.el --- Evil bindings for `slime' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, slime, tools

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
;; Evil bindings for `slime-mode'.

;;; Code:
(require 'evil-collection)
(require 'slime nil t)

(defvar slime-parent-map)
(defvar sldb-mode-map)
(defvar slime-inspector-mode-map)
(defvar slime-mode-map)
(defvar slime-popup-buffer-mode-map)
(defvar slime-thread-control-mode-map)
(defvar slime-xref-mode-map)

(defconst evil-collection-slime-maps '(slime-parent-map
                                       sldb-mode-map
                                       slime-inspector-mode-map
                                       slime-mode-map
                                       slime-popup-buffer-mode-map
                                       slime-thread-control-mode-map
                                       slime-xref-mode-map))

(defun evil-collection-slime-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

;;;###autoload
(defun evil-collection-slime-setup ()
  "Set up `evil' bindings for `slime'."
  (unless evil-move-beyond-eol
    (advice-add 'slime-eval-last-expression :around 'evil-collection-slime-last-sexp)
    (advice-add 'slime-pprint-eval-last-expression :around 'evil-collection-slime-last-sexp)
    (advice-add 'slime-eval-print-last-expression :around 'evil-collection-slime-last-sexp)
    (advice-add 'slime-eval-last-expression-in-repl
                :around 'evil-collection-slime-last-sexp))

  (evil-set-initial-state 'sldb-mode 'normal)
  (evil-set-initial-state 'slime-inspector-mode 'normal)
  (evil-set-initial-state 'slime-popup-buffer-mode 'normal)
  (evil-set-initial-state 'slime-thread-control-mode 'normal)
  (evil-set-initial-state 'slime-xref-mode 'normal)

  (evil-collection-define-key 'normal 'slime-parent-map
    "gd" 'slime-edit-definition
    (kbd "C-t") 'slime-pop-find-definition-stack)

  (evil-collection-define-key 'normal 'sldb-mode-map
    (kbd "RET") 'sldb-default-action
    [mouse-2]  'sldb-default-action/mouse
    [follow-link] 'mouse-face
    "\C-i" 'sldb-cycle
    "g?" 'describe-mode
    "S" 'sldb-show-source
    "e" 'sldb-eval-in-frame
    "d" 'sldb-pprint-eval-in-frame
    "D" 'sldb-disassemble
    "i" 'sldb-inspect-in-frame
    "gj" 'sldb-down
    "gk" 'sldb-up
    (kbd "C-j") 'sldb-down
    (kbd "C-k") 'sldb-up
    "]]" 'sldb-details-down
    "[[" 'sldb-details-up
    (kbd "M-j") 'sldb-details-down
    (kbd "M-k") 'sldb-details-up
    "gg" 'sldb-beginning-of-backtrace
    "G" 'sldb-end-of-backtrace
    "t" 'sldb-toggle-details
    "gr" 'sldb-restart-frame
    "I" 'sldb-invoke-restart-by-name
    "R" 'sldb-return-from-frame
    "c" 'sldb-continue
    "s" 'sldb-step
    "n" 'sldb-next
    "o" 'sldb-out
    "b" 'sldb-break-on-return
    "a" 'sldb-abort
    "q" 'sldb-quit
    "A" 'sldb-break-with-system-debugger
    "B" 'sldb-break-with-default-debugger
    "P" 'sldb-print-condition
    "C" 'sldb-inspect-condition
    "g:" 'slime-interactive-eval
    "0" 'sldb-invoke-restart-0
    "1" 'sldb-invoke-restart-1
    "2" 'sldb-invoke-restart-2
    "3" 'sldb-invoke-restart-3
    "4" 'sldb-invoke-restart-4
    "5" 'sldb-invoke-restart-5
    "6" 'sldb-invoke-restart-6
    "7" 'sldb-invoke-restart-7
    "8" 'sldb-invoke-restart-8
    "9" 'sldb-invoke-restart-9)

  (evil-collection-define-key 'normal 'slime-inspector-mode-map
    (kbd "RET") 'slime-inspector-operate-on-point
    [mouse-1] 'slime-inspector-operate-on-click
    [mouse-2] 'slime-inspector-operate-on-click
    [mouse-6] 'slime-inspector-pop
    [mouse-7] 'slime-inspector-next
    ;; TODO: `slime-inspector-next' and `slime-inspector-pop' should probably
    ;; just be bound to C-i and C-o.
    "gk" 'slime-inspector-pop
    (kbd "C-k") 'slime-inspector-pop
    "[[" 'slime-inspector-pop
    (kbd "C-o") 'slime-inspector-pop
    "gj" 'slime-inspector-next
    (kbd "C-j") 'slime-inspector-next
    "]]" 'slime-inspector-next
    (kbd "C-i") 'slime-inspector-next
    "K" 'slime-inspector-describe
    "p" 'slime-inspector-pprint
    "e" 'slime-inspector-eval
    "M-p" 'slime-inspector-history
    "gr" 'slime-inspector-reinspect
    "gv" 'slime-inspector-toggle-verbose
    (kbd "<tab>") 'slime-inspector-next-inspectable-object
    (kbd "C-i") 'slime-inspector-next-inspectable-object
    (kbd "<S-tab>") 'slime-inspector-previous-inspectable-object ; Emacs translates S-TAB
    (kbd "<backtab>") 'slime-inspector-previous-inspectable-object ; to BACKTAB on X.
    "." 'slime-inspector-show-source
    "gd" 'slime-inspector-show-source
    "gR" 'slime-inspector-fetch-all
    "q" 'slime-inspector-quit)

  (evil-collection-define-key 'normal 'slime-mode-map
    (kbd "K") 'slime-describe-symbol
    (kbd "C-t") 'slime-pop-find-definition-stack
    ;; goto
    "gd" 'slime-edit-definition
    "gz" 'slime-switch-to-output-buffer)

  (when evil-collection-want-find-usages-bindings
    (evil-collection-define-key 'normal 'slime-mode-map
      "gr" 'slime-who-references))

  (evil-collection-define-key 'normal 'slime-popup-buffer-mode-map
    ;; quit
    "q" 'quit-window

    (kbd "C-t") 'slime-pop-find-definition-stack

    ;; goto
    "gd" 'slime-edit-definition)

  (evil-collection-inhibit-insert-state 'slime-thread-control-mode-map)
  (evil-collection-define-key 'normal 'slime-thread-control-mode-map
    "a" 'slime-thread-attach
    "d" 'slime-thread-debug
    "x" 'slime-thread-kill
    "gr" 'slime-update-threads-buffer)

  (evil-collection-define-key 'normal 'slime-xref-mode-map
    (kbd "RET") 'slime-goto-xref
    (kbd "S-<return>") 'slime-goto-xref
    "go" 'slime-show-xref
    "gj" 'slime-xref-next-line
    "gk" 'slime-xref-prev-line
    (kbd "C-j") 'slime-xref-next-line
    (kbd "C-k") 'slime-xref-prev-line
    "]]" 'slime-xref-next-line
    "[[" 'slime-xref-prev-line
    "gr" 'slime-recompile-xref
    "gR" 'slime-recompile-all-xrefs
    "r" 'slime-xref-retract)

  (evil-collection-define-key 'normal 'slime-repl-mode-map
    "gj" 'slime-repl-next-prompt
    "gk" 'slime-repl-previous-prompt
    (kbd "C-j") 'slime-repl-next-prompt
    (kbd "C-k") 'slime-repl-previous-prompt
    "]]" 'slime-repl-next-prompt
    "[[" 'slime-repl-previous-prompt
    (kbd "C-p") 'slime-repl-previous-input
    (kbd "C-n") 'slime-repl-next-input)

  (add-hook 'slime-popup-buffer-mode-hook #'evil-normalize-keymaps))

(provide 'evil-collection-slime)
;;; evil-collection-slime.el ends here
