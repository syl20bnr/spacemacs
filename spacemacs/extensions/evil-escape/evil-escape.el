;;; evil-escape.el --- Escape from anything with a customizable key sequence

;; Copyright (C) 2014-2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil
;; Created: 22 Oct 2014
;; Version: 2.06
;; Package-Requires: ((emacs "24") (evil "1.0.9"))
;; URL: https://github.com/syl20bnr/evil-escape

;; This file is not part of GNU Emacs.

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

;; Press `fd` quickly to:
;; ----------------------

;;   - escape from all stock evil states to normal state
;;   - escape from evil-lisp-state to normal state
;;   - escape from evil-iedit-state to normal state
;;   - abort evil ex command
;;   - quit minibuffer
;;   - abort isearch
;;   - quit magit buffers
;;   - quit help buffers
;;   - quit apropos buffers
;;   - quit ert buffers
;;   - quit undo-tree buffer
;;   - quit paradox
;;   - quit gist-list menu
;;   - hide neotree buffer
;; And more to come !

;; Configuration:
;; --------------

;; The key sequence can be customized with the variable
;; `evil-escape-key-sequence'
;; It must be set before requiring evil-escape.

;; The delay between the two key presses can be customized with
;; the variable `evil-escape-delay'. Default is `0.1'.
;; It must be set before requiring evil-escape.

;; More information in the readme of the repository:
;; https://github.com/syl20bnr/evil-escape

;;; Code:

(require 'evil)

(defgroup evil-escape nil
  "Key sequence to escape insert state and everything else."
  :prefix "evil-escape-"
  :group 'evil)

(eval-and-compile
  (defcustom evil-escape-key-sequence (kbd "fd")
    "Two keys sequence to escape from insert state."
    :type 'key-sequence
    :group 'evil-escape)

  (defcustom evil-escape-delay 0.1
    "Max time delay between the two key press to be considered successful."
    :type 'number
    :group 'evil-escape))

(defvar evil-escape-motion-state-shadowed-func nil
  "Original function of `evil-motion-state' shadowed by `evil-espace'.
This variable is used to restore the original function bound to the
first key of the escape key sequence when `evil-escape'
mode is disabled.")

(defvar evil-escape-isearch-shadowed-func nil
  "Original function of `isearch-mode-map' shadowed by `evil-escape'.
This variable is used to restore the original function bound to the
first key of the escape key sequence when `evil-escape'
mode is disabled.")

;;;###autoload
(define-minor-mode evil-escape-mode
  "Buffer-local minor mode to escape insert state and everythin else
with a key sequence."
  :lighter (:eval (concat " " evil-escape-key-sequence))
  :group 'evil
  :global t
  (if evil-escape-mode
      (progn
        (evil-escape--define-keys)
        (message "evil-escape enabled, press \"%s\" to escape from anything."
                 evil-escape-key-sequence))
    (evil-escape--undefine-keys)))

(eval-and-compile
  (defun evil-escape--first-key ()
    "Return the first key string in the key sequence."
    (let* ((first-key (elt evil-escape-key-sequence 0))
           (fkeystr (char-to-string first-key)))
      fkeystr)))

(defun evil-escape--escape-function-symbol (from)
  "Return the function symbol for the passed FROM string."
  (intern (format "evil-escape-%s" from)))

(defmacro evil-escape-define-escape (from map command &rest properties)
  "Define a function to escape from FROM in MAP keymap by executing COMMAND.

`:shadowed-func FUNCTION'
     If non nil specify the shadowed function from the first key of the
     sequence.

`:insert-func FUNCTION'
     Specify the insert function to call when inserting the first key.

`:delete-func FUNCTION'
     Specify the delete function to call when deleting the first key."
  (let* ((shadowed-func (plist-get properties :shadowed-func))
         (evil-func-props (when shadowed-func
                            (evil-get-command-properties shadowed-func)))
         (insert-func (plist-get properties :insert-func))
         (delete-func (plist-get properties :delete-func)))
    `(progn
       (define-key ,map ,(evil-escape--first-key)
         (evil-define-motion ,(evil-escape--escape-function-symbol from)
           (count)
           ,@evil-func-props
           ;; called by the user
           (if (called-interactively-p 'interactive)
               (evil-escape--escape ,evil-escape-key-sequence
                                    ',command
                                    ',shadowed-func
                                    ',insert-func
                                    ',delete-func)
             ;; not called by the user, i.e. called by a keyboard macro
             (when (fboundp ',insert-func)
               (funcall ',insert-func ,(evil-escape--first-key)))))))))

(defun evil-escape--define-keys ()
  "Set the key bindings to escape _everything!_"
  (setq evil-escape-motion-state-shadowed-func
        (lookup-key evil-motion-state-map (evil-escape--first-key)))
  ;; evil states
  ;; insert state
  (let ((insert-func (lambda (key) (interactive)
                       (cond ((eq 'term-mode major-mode)
                              (call-interactively 'term-send-raw))
                             (t (evil-escape--default-insert-func key))))))
    (eval `(evil-escape-define-escape "insert-state" evil-insert-state-map evil-normal-state
                                      :insert-func ,insert-func
                                      :delete-func evil-escape--default-delete-func)))
  ;; emacs state
  (let ((exit-func (lambda () (interactive)
                     (cond ((string-match "magit" (symbol-name major-mode))
                              (evil-escape--escape-with-q))
                             ((eq 'paradox-menu-mode major-mode)
                              (evil-escape--escape-with-q))
                             ((eq 'gist-list-menu-mode major-mode)
                              (quit-window))
                             (t  evil-normal-state)))))
    (eval `(evil-escape-define-escape "emacs-state" evil-emacs-state-map ,exit-func)))
  ;; visual state
  (eval `(evil-escape-define-escape "visual-state" evil-visual-state-map evil-exit-visual-state
                                    :shadowed-func ,evil-escape-motion-state-shadowed-func))
  ;; motion state
  (let ((exit-func (lambda () (interactive)
                     (cond ((or (eq 'apropos-mode major-mode)
                                (eq 'help-mode major-mode)
                                (eq 'ert-results-mode major-mode)
                                (eq 'ert-simple-view-mode major-mode))
                            (quit-window))
                           ((eq 'undo-tree-visualizer-mode major-mode)
                            (undo-tree-visualizer-quit))
                           ((eq 'neotree-mode major-mode) (neotree-hide))
                           (t (evil-normal-state))))))
    (eval `(evil-escape-define-escape "motion-state" evil-motion-state-map ,exit-func
                                      :shadowed-func ,evil-escape-motion-state-shadowed-func)))
  ;; mini-buffer
  (eval `(evil-escape-define-escape "minibuffer" minibuffer-local-map abort-recursive-edit
                                    :insert-func evil-escape--default-insert-func
                                    :delete-func evil-escape--default-delete-func))
  ;; evil ex command
  (eval `(evil-escape-define-escape "ex-command" evil-ex-completion-map abort-recursive-edit
                                    :insert-func evil-escape--default-insert-func
                                    :delete-func evil-escape--default-delete-func))
  ;; isearch
  (setq evil-escape-isearch-shadowed-func
        (lookup-key isearch-mode-map (evil-escape--first-key)))
  (eval `(evil-escape-define-escape "isearch" isearch-mode-map isearch-abort
                                    :insert t
                                    :delete t
                                    :shadowed-func ,evil-escape-isearch-shadowed-func
                                    :insert-func evil-escape--isearch-insert-func
                                    :delete-func isearch-delete-char))
  ;; lisp state if installed
  (eval-after-load 'evil-lisp-state
    '(eval '(evil-escape-define-escape "lisp-state" evil-lisp-state-map evil-normal-state)))
  ;; iedit state if installed
  (eval-after-load 'evil-iedit-state
    '(progn
       (eval `(evil-escape-define-escape "iedit-state" evil-iedit-state-map
                                         evil-iedit-state/quit-iedit-mode
                                         :shadowed-func ,evil-escape-motion-state-shadowed-func))
       (eval '(evil-escape-define-escape "iedit-insert-state" evil-iedit-insert-state-map
                                         evil-iedit-state/quit-iedit-mode
                                         :insert-func evil-escape--default-insert-func
                                         :delete-func evil-escape--default-delete-func)))))

(defun evil-escape--undefine-keys ()
  "Unset the key bindings defined in `evil-escape--define-keys'."
  (let ((first-key (evil-escape--first-key)))
    ;; motion state
    (if evil-escape-motion-state-shadowed-func
        (define-key evil-motion-state-map
          (kbd first-key) evil-escape-motion-state-shadowed-func))
    ;; isearch
    (if evil-escape-isearch-shadowed-func
        (define-key isearch-mode-map
          (kbd first-key) evil-escape-isearch-shadowed-func))
    ;; list state
    (eval-after-load 'evil-lisp-state
      '(define-key evil-lisp-state-map (kbd first-key) nil))
    ;; iedit state
    (eval-after-load 'evil-iedit-state
      '(progn (define-key evil-iedit-state-map (kbd first-key) nil)
              (define-key evil-iedit-insert-state-map (kbd first-key) nil)))))

(defun evil-escape--default-insert-func (key)
  "Insert KEY in current buffer if not read only."
  (when (not buffer-read-only) (insert key)))

(defun evil-escape--isearch-insert-func (key)
  "Insert KEY in current buffer if not read only."
  (isearch-printing-char))

(defun evil-escape--default-delete-func ()
  "Delete char in current buffer if not read only."
  (when (not buffer-read-only) (delete-char -1)))

(defun evil-escape--escape-with-q ()
  "Send `q' key press event to exit from a buffer."
  (setq unread-command-events (listify-key-sequence "q")))

(defun evil-escape--term-insert-func (key)
  "Insert KEY in current term buffer."
  (term-send-raw))

(defun evil-escape--execute-shadowed-func (func)
  "Execute the passed FUNC if the context allows it."
  (unless (or (null func)
              (eq 'insert evil-state)
              (and (boundp 'isearch-mode) (symbol-value 'isearch-mode))
              (minibufferp))
    (call-interactively func)))

(defun evil-escape--passthrough (from key map hfunc)
  "Allow the next command KEY to pass through MAP so they can reach
the underlying major or minor modes map.
Once the command KEY passed through MAP the function HFUNC is removed
from the `post-command-hook'."
  (if (lookup-key map key)
      (define-key map key nil)
    (let ((escape-func (evil-escape--escape-function-symbol from)))
      (define-key map key escape-func)
      (remove-hook 'post-command-hook hfunc))))

(defun evil-escape--emacs-state-passthrough ()
  "Allow next command KEY to pass through `evil-emcs-state-map'"
  (evil-escape--passthrough "emacs-state"
                            (evil-escape--first-key)
                            evil-emacs-state-map
                            'evil-escape--emacs-state-passthrough))

(defun evil-escape--setup-emacs-state-passthrough ()
  "Setup a pass through for emacs state map"
  (when (eq 'emacs evil-state)
    (add-hook 'post-command-hook 'evil-escape--emacs-state-passthrough)
    (setq unread-command-events
          (append unread-command-events (listify-key-sequence
                                         (evil-escape--first-key))))))

(defun evil-escape--escape
    (keys callback &optional shadowed-func insert-func delete-func)
  "Execute the passed CALLBACK using KEYS. KEYS is a cons cell of 2 characters.

If the first key insertion shadowed a function then pass the shadowed function
in SHADOWED-FUNC and it will be executed if the key sequence was not
 successfull.

If INSERT-FUNC is not nil then the first key pressed is inserted using the
 function INSERT-FUNC.

If DELETE-FUNC is not nil then the first key is deleted using the function
DELETE-FUNC when calling CALLBACK. "
  (let* ((modified (buffer-modified-p))
         (fkey (elt keys 0))
         (fkeystr (char-to-string fkey))
         (skey (elt keys 1)))
    (if insert-func (funcall insert-func fkey))
    (let* ((evt (read-event nil nil evil-escape-delay)))
      (cond
       ((null evt)
        (evil-escape--setup-emacs-state-passthrough)
        (evil-escape--execute-shadowed-func shadowed-func))
       ((and (integerp evt)
             (char-equal evt skey))
        ;; remove the f character
        (if delete-func (funcall delete-func))
        (set-buffer-modified-p modified)
        (call-interactively callback))
       (t ; otherwise
        (evil-escape--setup-emacs-state-passthrough)
        (setq unread-command-events
              (append unread-command-events (list evt)))
        (evil-escape--execute-shadowed-func shadowed-func))))))

(provide 'evil-escape)

;;; evil-escape.el ends here
