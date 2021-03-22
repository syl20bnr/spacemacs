;;; evil-evilified-state.el --- A minimalistic evil state
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil spacemacs
;; Created: 22 Mar 2015
;; Version: 1.0
;; Package-Requires: ((evil "1.0.9"))

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

;; Define a `evilified' evil state inheriting from `emacs' state and
;; setting a minimalist list of Vim key bindings (like navigation, search, ...)

;; The shadowed original mode key bindings are automatically reassigned
;; following a set of rules:
;; Keys such as
;; /,:,h,j,k,l,n,N,v,V,gg,G,C-f,C-b,C-d,C-e,C-u,C-y and C-z
;; are working as in Evil.
;; Other keys will be moved according to this pattern:
;; a -> A -> C-a -> C-A
;; The first unreserved key will be used.
;; There is an exception for g, which will be directly
;; bound to C-G, since G and C-g (latest being an important escape key in Emacs)
;; are already being used.

;;; Code:

(require 'evil)
(require 'bind-map)

(defvar evilified-state--normal-state-map nil
  "Local backup of normal state keymap.")
(make-variable-buffer-local 'evilified-state--normal-state-map)

(defvar evilified-state--visual-state-map nil
  "Local backup of visual state keymap.")
(make-variable-buffer-local 'evilified-state--visual-state-map)

(defvar evilified-state--evil-surround-was-enabled nil
  "Used to restore evil-surround-mode when exiting evilified state.")
(make-variable-buffer-local 'evilified-state--evil-surround-was-enabled)

(evil-define-state evilified
  "Evilified state.
 Hybrid `emacs state' with carefully selected Vim key bindings.
 See spacemacs conventions for more info."
  :tag " <N'> "
  :enable (emacs)
  :message "-- EVILIFIED BUFFER --"
  :cursor box)

(bind-map spacemacs-default-map
  :prefix-cmd spacemacs-cmds
  :evil-keys (dotspacemacs-leader-key)
  :evil-states (evilified)
  :override-minor-modes t
  :override-mode-name spacemacs-leader-override-mode)

(evil-define-command evil-force-evilified-state ()
  "Switch to evilified state without recording current command."
  :repeat abort
  :suppress-operator t
  (evil-evilified-state))

(defun evilified-state--pre-command-hook ()
  "Redirect key bindings to `evilified-state'.
Needed to bypass keymaps set as text properties."
  (unless (bound-and-true-p isearch-mode)
    (when (memq evil-state '(evilified visual))
      (let* ((map-or-symbol (get-char-property (point) 'keymap))
             (map (if (and (symbolp map-or-symbol) (boundp map-or-symbol))
                      (symbol-value map-or-symbol)
                    map-or-symbol))
             (evilified-map (when map (cdr (assq 'evilified-state map))))
             (command (when (and evilified-map
                                 (eq 1 (length (this-command-keys))))
                        (lookup-key evilified-map (this-command-keys)))))
        (when command (setq this-command command))))))

(defun evilified-state--setup-normal-state-keymap ()
  "Setup the normal state keymap."
  (unless evilified-state--normal-state-map
    (setq-local evilified-state--normal-state-map
                (copy-keymap evil-normal-state-map)))
  (setq-local evil-normal-state-map
              (copy-keymap evilified-state--normal-state-map))
  (define-key evil-normal-state-map [escape] 'evil-evilified-state))

(defun evilified-state--restore-normal-state-keymap ()
  "Restore the normal state keymap."
  (setq-local evil-normal-state-map evilified-state--normal-state-map)
  (define-key evil-normal-state-map [escape] 'evil-force-normal-state)
  (evil-normal-state))

(defun evilified-state--clear-normal-state-keymap ()
  "Clear the normal state keymap."
  (setq-local evil-normal-state-map (cons 'keymap nil))
  (evil-normalize-keymaps))

(defun evilified-state--setup-visual-state-keymap ()
  "Setup the visual state keymap."
  (unless evilified-state--visual-state-map
    (setq-local evilified-state--visual-state-map
                (copy-keymap evil-visual-state-map)))
  (setq-local evil-visual-state-map
              (cons 'keymap (list (cons ?a evil-outer-text-objects-map)
                                  (cons ?i evil-inner-text-objects-map)
                                  (cons ?o 'exchange-point-and-mark)
                                  (cons ?y 'evil-yank)
                                  (cons 'escape 'evil-exit-visual-state)))))

(defun evilified-state--restore-visual-state-keymap ()
  "Restore the visual state keymap."
  (setq-local evil-visual-state-map evilified-state--visual-state-map))

(defun evilified-state--evilified-state-on-entry ()
  "Setup evilified state."
  (when (derived-mode-p 'magit-mode)
    ;; Courtesy of evil-magit package
    ;; without this set-mark-command activates visual-state which is just
    ;; annoying ;; and introduces possible bugs
    (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t))
  (when (bound-and-true-p evil-surround-mode)
    (setq evilified-state--evil-surround-was-enabled t)
    (make-local-variable 'evil-surround-mode)
    (evil-surround-mode -1))
  (evilified-state--setup-normal-state-keymap)
  (evilified-state--setup-visual-state-keymap)
  (add-hook 'pre-command-hook 'evilified-state--pre-command-hook nil 'local)
  (add-hook 'evil-visual-state-entry-hook
            'evilified-state--visual-state-on-entry nil 'local)
  (add-hook 'evil-visual-state-exit-hook
            'evilified-state--visual-state-on-exit nil 'local))

(defun evilified-state--evilified-state-on-exit ()
  "Restore evil normal and visual states."
  (when evilified-state--evil-surround-was-enabled
    (evil-surround-mode 1)
    (setq evilified-state--evil-surround-was-enabled nil))
  (evilified-state--restore-normal-state-keymap)
  (evilified-state--restore-visual-state-keymap)
  (remove-hook 'pre-command-hook 'evilified-state--pre-command-hook 'local)
  (remove-hook 'evil-visual-state-entry-hook
               'evilified-state--visual-state-on-entry 'local)
  (remove-hook 'evil-visual-state-exit-hook
               'evilified-state--visual-state-on-exit 'local))

(defalias 'evil-evilified-state-exit 'evilified-state--evilified-state-on-exit)

(defun evilified-state--visual-state-on-entry ()
  "Setup visual state."
  ;; we need to clear temporarily the normal state keymap in order to reach
  ;; the mode keymap
  (when (eq 'evilified evil-previous-state)
    (evilified-state--clear-normal-state-keymap)))

(defun evilified-state--visual-state-on-exit ()
  "Clean visual state"
  (evilified-state--restore-visual-state-keymap))

(add-hook 'evil-evilified-state-entry-hook
          'evilified-state--evilified-state-on-entry)

;; default key bindings for all evilified buffers
(define-key evil-evilified-state-map "/" 'evil-search-forward)
(define-key evil-evilified-state-map ":" 'evil-ex)
(define-key evil-evilified-state-map "h" 'evil-backward-char)
(define-key evil-evilified-state-map "j" 'evil-next-visual-line)
(define-key evil-evilified-state-map "k" 'evil-previous-visual-line)
(define-key evil-evilified-state-map "l" 'evil-forward-char)
(define-key evil-evilified-state-map "n" 'evil-search-next)
(define-key evil-evilified-state-map "N" 'evil-search-previous)
(define-key evil-evilified-state-map "v" 'evil-visual-char)
(define-key evil-evilified-state-map "V" 'evil-visual-line)
(define-key evil-evilified-state-map "gg" 'evil-goto-first-line)
(define-key evil-evilified-state-map "G" 'evil-goto-line)
(define-key evil-evilified-state-map (kbd "C-f") 'evil-scroll-page-down)
(define-key evil-evilified-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-evilified-state-map (kbd "C-e") 'evil-scroll-line-down)
(define-key evil-evilified-state-map (kbd "C-y") 'evil-scroll-line-up)
(define-key evil-evilified-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-evilified-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-evilified-state-map (kbd "C-z") 'evil-emacs-state)
(define-key evil-evilified-state-map (kbd "C-w") 'evil-window-map)
(setq evil-evilified-state-map-original (copy-keymap evil-evilified-state-map))

;; old macro
;;;###autoload
(defmacro evilified-state-evilify (mode map &rest body)
  "Set `evilified state' as default for MODE.

BODY is a list of additional key bindings to apply for the given MAP in
`evilified state'."
  (let ((defkey (when body `(evil-define-key 'evilified ,map ,@body))))
    `(progn (unless ,(null mode)
              (unless (or (bound-and-true-p holy-mode)
                          (eq 'evilified (evil-initial-state ',mode)))
                (evil-set-initial-state ',mode 'evilified)))
            (unless ,(null defkey) (,@defkey)))))
(put 'evilified-state-evilify 'lisp-indent-function 'defun)

;; new macro
;;;###autoload
(defmacro evilified-state-evilify-map (map &rest props)
  "Evilify MAP.

Available PROPS:

`:mode SYMBOL'
A mode SYMBOL associated with MAP. Used to add SYMBOL to the list of modes
defaulting to `evilified-state'.

`:evilified-map SYMBOL'
A map SYMBOL of an alternate evilified map, if nil then
`evil-evilified-state-map' is used.

`:eval-after-load SYMBOL'
If specified the evilification of MAP is deferred to the loading of the feature
bound to SYMBOL. May be required for some lazy-loaded maps.

`:pre-bindings EXPRESSIONS'
One or several EXPRESSIONS with the form `KEY FUNCTION':
   KEY1 FUNCTION1
   KEY2 FUNCTION2
These bindings are set in MAP before the evilification happens.

`:bindings EXPRESSIONS'
One or several EXPRESSIONS with the form `KEY FUNCTION':
   KEY1 FUNCTION1
   KEY2 FUNCTION2
These bindings are set directly in evil-evilified-state-map submap.
   ...
Each pair KEYn FUNCTIONn is defined in MAP after the evilification of it."
  (declare (indent 1))
  (let* ((mode (plist-get props :mode))
         (evilified-map (or (plist-get props :evilified-map)
                            'evil-evilified-state-map-original))
         (eval-after-load (plist-get props :eval-after-load))
         (pre-bindings (evilified-state--mplist-get props :pre-bindings))
         (bindings (evilified-state--mplist-get props :bindings))
         (defkey (when bindings `(evil-define-key 'evilified ,map ,@bindings)))
         (body
          (progn
            (evilified-state--define-pre-bindings map pre-bindings)
            `(
              ;; we need to work on a local copy of the evilified keymap to
              ;; prevent the original keymap from being mutated.
              (setq evil-evilified-state-map (copy-keymap ,evilified-map))
              (let* ((sorted-map (evilified-state--sort-keymap
                                  evil-evilified-state-map))
                    processed)
                (mapc (lambda (map-entry)
                        (unless (member (car map-entry) processed)
                          (setq processed (evilified-state--evilify-event
                                           ,map ',map evil-evilified-state-map
                                           (car map-entry) (cdr map-entry)))))
                      sorted-map)
                (unless ,(null defkey)
                  (,@defkey)))
              (unless ,(null mode)
                (evilified-state--configure-default-state ',mode))))))
    (if (null eval-after-load)
        `(progn ,@body)
      `(with-eval-after-load ',eval-after-load (progn ,@body)))))
(put 'evilified-state-evilify-map 'lisp-indent-function 'defun)

(defun evilified-state--define-pre-bindings (map pre-bindings)
  "Define PRE-BINDINGS in MAP."
  (while pre-bindings
    (let ((key (pop pre-bindings))
          (func (pop pre-bindings)))
      (eval `(define-key ,map key ,func)))))

(defun evilified-state--configure-default-state (mode)
  "Configure default state for the passed mode."
  (evil-set-initial-state mode 'evilified))

(defun evilified-state--evilify-event (map map-symbol evil-map event evil-value
                                           &optional processed pending-funcs)
  "Evilify EVENT in MAP and return a list of PROCESSED events."
  (if (and event (or evil-value pending-funcs))
      (let* ((kbd-event (kbd (single-key-description event)))
             (map-value (lookup-key map kbd-event))
             (evil-value (or evil-value
                             (lookup-key evil-map kbd-event)
                             (car (pop pending-funcs)))))
        (when evil-value
          (evil-define-key 'evilified map kbd-event evil-value))
        (when map-value
          (add-to-list 'pending-funcs (cons map-value event) 'append))
        (push event processed)
        (setq processed (evilified-state--evilify-event
                         map map-symbol evil-map
                         (evilified-state--find-new-event event) nil
                         processed pending-funcs)))
    (when pending-funcs
      (message
       (concat (format (concat "Auto-evilification could not remap these "
                               "functions in map `%s':\n")
                       map-symbol)
               (mapconcat (lambda (x)
                            (format "   - `%s' originally mapped on `%s'"
                                    (car x) (single-key-description (cdr x))))
                          pending-funcs "\n")))))
  processed)

(defun evilified-state--find-new-event (event)
  "Return a new event for the evilified EVENT."
  (when event
    (cond
     ((equal event ?\a) nil) ; C-g (cannot remap C-g)
     ((equal event 32) ?')   ; space
     ((equal event ?/) ?\\)
     ((equal event ?:) ?|)
     ((and (numberp event) (<= ?a event) (<= event ?z)) (- event 32))
     ((equal event ?G) (+ (expt 2 25) ?\a)) ; G is mapped directly to C-S-g
     ((and (numberp event) (<= ?A event) (<= event ?Z)) (- event 64))
     ((and (numberp event) (<= 1 event) (<= event 26)) (+ (expt 2 25) event)))))

(defun evilified-state--sort-keymap (map)
  "Sort MAP following the order: `s' > `S' > `C-s' > `C-S-s'"
  (let (list)
    (map-keymap (lambda (a b) (push (cons a b) list)) map)
    (sort list
          (lambda (a b)
            (setq a (car a) b (car b))
            (if (integerp a)
                (if (integerp b)
                    (if (and (< a 256) (< b 256))
                        (> a b)
                      (< a b))
                  t)
              (if (integerp b) nil
                (string< a b)))))))

(defun evilified-state--mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(provide 'evil-evilified-state)

;;; core-evilified-state.el ends here
