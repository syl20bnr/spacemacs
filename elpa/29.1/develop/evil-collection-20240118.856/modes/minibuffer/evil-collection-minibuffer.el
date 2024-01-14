;;; evil-collection-minibuffer.el --- Evil bindings for the minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, minibuffer, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for the minibuffer.

;;; Code:
(require 'evil-collection)

(defconst evil-collection-minibuffer-maps '(minibuffer-local-map
                                            minibuffer-local-ns-map
                                            minibuffer-local-completion-map
                                            minibuffer-local-must-match-map
                                            minibuffer-local-isearch-map
                                            evil-ex-completion-map))

(defun evil-collection-minibuffer-insert ()
  "Switch to insert state.

This function is meant to be hooked in the minibuffer:

  (add-hook \='minibuffer-setup-hook \='evil-collection-minibuffer-insert)

`evil-set-initial-state' can not be used for the minibuffer since
it does not have a mode."
  (set (make-local-variable 'evil-echo-state) nil)
  ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
  ;; way to do this, but the minibuffer doesn't have a mode.
  ;; The alternative is to create a minibuffer mode (here), but
  ;; then it may conflict with other packages' if they do the same.
  (evil-insert 1))

(evil-define-operator evil-collection-change-in-minibuffer
  (beg end type register yank-handler delete-func)
  "A version of `evil-change' that won't insert a new line on buffers without one."
  (interactive "<R><x><y>")
  ;; If there was no new line before the change, there should be none after.
  ;; Delete any new line that might have been inserted and ignore an error if
  ;; one wasn't.
  (let ((new-inserted (and (eq type 'line) (/= ?\n (char-before end)))))
    (evil-change beg end type register yank-handler delete-func)
    (when new-inserted (ignore-errors (delete-char 1)))))

;;;###autoload
(defun evil-collection-minibuffer-setup ()
  "Initialize minibuffer for `evil'."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html
  (dolist (map evil-collection-minibuffer-maps)
    (evil-collection-define-key 'normal map "c" 'evil-collection-change-in-minibuffer)
    (evil-collection-define-key 'normal map (kbd "<escape>") 'abort-recursive-edit)
    (evil-collection-define-key 'normal map (kbd "RET") 'exit-minibuffer))

  (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
  ;; Because of the above minibuffer-setup-hook, some evil-ex bindings need be reset.
  (evil-collection-define-key 'normal 'evil-ex-completion-map (kbd "<escape>") 'abort-recursive-edit)
  (evil-collection-define-key 'insert 'evil-ex-completion-map (kbd "C-p") 'previous-complete-history-element)
  (evil-collection-define-key 'insert 'evil-ex-completion-map (kbd "C-n") 'next-complete-history-element)
  (evil-collection-define-key 'normal 'evil-ex-completion-map (kbd "C-p") 'previous-history-element)
  (evil-collection-define-key 'normal 'evil-ex-completion-map (kbd "C-n") 'next-history-element))

(provide 'evil-collection-minibuffer)
;;; evil-collection-minibuffer.el ends here
