;;; core-evilified-state.el --- A minimalistic evil state

;; Copyright (C) 2014, 2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil spacemacs
;; Created: 22 Mar 2015
;; Version: 1.0
;; Package-Requires: ((evil "1.0.9") (evil-leader "0.4.3"))

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

;;; Code:

(require 'core-dotspacemacs)
(require 'evil)
(require 'evil-leader)

(defvar spacemacs-core-evilified-state--modes nil
  "List of all evilified modes.")

(defvar spacemacs-core-evilified-state--visual-state-map evil-visual-state-map
  "Evil visual state map backup.")

(defvar spacemacs-core-evilified-state--evil-surround nil
  "Evil surround mode variable backup.")
(make-variable-buffer-local 'spacemacs-core-evilified-state--evil-surround)

(evil-define-state evilified
  "Evilified state.
 Hybrid `emacs state' with carrefully selected Vim key bindings.
 See spacemacs conventions for more info."
  :tag " <Ev> "
  :enable (emacs)
  :message "-- EVILIFIED BUFFER --"
  :cursor box)

(add-hook 'evil-evilified-state-entry-hook 'spacemacs//evilified-state-on-entry)
(add-hook 'evil-evilified-state-exit-hook 'spacemacs//evilified-state-on-exit)

(add-hook 'evil-visual-state-entry-hook 'spacemacs//visual-state-on-entry)
(add-hook 'evil-visual-state-exit-hook 'spacemacs//visual-state-on-exit)

(defun spacemacs//evilify-pre-command-hook ()
  (let ((map (get-char-property (point) 'keymap)))
    (when (and map (assq 'evilified-state map))
      (let* ((submap (cdr (assq 'evilified-state map)))
             (command (when (and submap (eq 1 (length (this-command-keys))))
                        (lookup-key submap (this-command-keys)))))
        (when command
          (setq this-command command))))))

(defun spacemacs//evilified-state-on-entry ()
  "Setup evilified state."
  (add-hook 'pre-command-hook 'spacemacs//evilify-pre-command-hook nil 'local)
  (when (bound-and-true-p evil-surround-mode)
    (make-local-variable 'evil-surround-mode)
    (evil-surround-mode -1))
  (setq-local evil-normal-state-map (cons 'keymap nil))
  (setq-local evil-visual-state-map (cons 'keymap (list (cons ?y 'evil-yank)))))

(defun spacemacs//evilified-state-on-exit ()
  "Clean evilified state"
  (remove-hook 'pre-command-hook 'spacemacs//evilify-pre-command-hook 'local))

(defun spacemacs//visual-state-on-entry ()
  "Setup visual state."
  (add-hook 'pre-command-hook 'spacemacs//evilify-pre-command-hook nil 'local))

(defun spacemacs//visual-state-on-exit ()
  "Clean visual state"
  (remove-hook 'pre-command-hook 'spacemacs//evilify-pre-command-hook 'local))

;; default key bindings for all evilified buffers
(define-key evil-evilified-state-map (kbd dotspacemacs-leader-key)
  evil-leader--default-map)
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
(define-key evil-evilified-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-evilified-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-evilified-state-map (kbd "C-z") 'evil-emacs-state)

;; old macro
(defmacro evilify (mode map &rest body)
  "Set `evilified state' as default for MODE.

BODY is a list of additional key bindings to apply for the given MAP in
`evilified state'."
  (let ((defkey (when body `(evil-define-key 'evilified ,map ,@body))))
    `(progn (unless ,(null mode)
              (unless (memq ',mode spacemacs-core-evilified-state--modes)
                (push ',mode spacemacs-core-evilified-state--modes))
              (unless (or (bound-and-true-p holy-mode)
                          (memq ',mode evil-evilified-state-modes))
                (delq ',mode evil-emacs-state-modes)
                (push ',mode evil-evilified-state-modes)))
            (unless ,(null defkey) (,@defkey)))))

;; new macro
(defmacro spacemacs|evilify-map (map &rest props)
  "Evilify MAP.

Avaiblabe PROPS:

`:mode SYMBOL'
A mode SYMBOL associated with MAP. Used to add SYMBOL to the list of modes
defaulting to `evilified-state'.

`:evilified-map SYMBOL'
A map SYMBOL of an alternate evilified map, if nil then
`evil-evilified-state-map' is used.

`:eval-after-load SYMBOL'
If specified the evilification of MAP is deferred to the loading of the feature
bound to SYMBOL. May be required for some lazy-loaded maps.

`:bindings EXPRESSIONS'
One or several EXPRESSIONS with the form `KEY FUNCTION':
   KEY1 FUNCTION1
   KEY2 FUNCTION2
   ...
Each pair KEYn FUNCTIONn is defined in MAP after the evilification of it."
  (declare (indent 1))
  (let* ((mode (plist-get props :mode))
         (evilified-map (plist-get props :evilified-map))
         (eval-after-load (plist-get props :eval-after-load))
         (bindings (spacemacs/mplist-get props :bindings))
         (defkey (when bindings `(evil-define-key 'evilified ,map ,@bindings)))
         (body
          `(progn
             (let ((sorted-map (spacemacs//evilify-sort-keymap
                                (or ,evilified-map evil-evilified-state-map)))
                   processed)
               (mapc (lambda (map-entry)
                       (unless (member (car map-entry) processed)
                         (setq processed (spacemacs//evilify-event
                                          ,map ',map
                                          (or ,evilified-map
                                              evil-evilified-state-map)
                                          (car map-entry) (cdr map-entry)))))
                     sorted-map))
             (unless ,(null defkey)
               (,@defkey))
             (unless ,(null mode)
               (spacemacs/evilify-configure-default-state ',mode)))))
    (if (null eval-after-load)
        `(,@body)
      `(eval-after-load ',eval-after-load '(,@body)))))

(defun spacemacs/evilify-configure-default-state (mode)
  "Configure default state for the passed mode."
  (add-to-list 'spacemacs-core-evilified-state--modes mode)
  (unless (bound-and-true-p holy-mode)
    (delq mode evil-emacs-state-modes)
    (add-to-list 'evil-evilified-state-modes mode)))

(defun spacemacs//evilify-event (map map-symbol evil-map event evil-value
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
        (setq processed (spacemacs//evilify-event
                         map map-symbol evil-map
                         (spacemacs//evilify-find-new-event event) nil
                         processed pending-funcs)))
    (when pending-funcs
      (spacemacs-buffer/warning
       (concat (format (concat "Auto-evilication could not remap these "
                               "functions in map `%s':\n")
                       map-symbol)
               (mapconcat (lambda (x)
                            (format "   - `%s' originally mapped on `%s'"
                                    (car x) (single-key-description (cdr x))))
                          pending-funcs "\n")))))
  processed)

(defun spacemacs//evilify-find-new-event (event)
  "Return a new event for the evilified EVENT."
  (when event
    (cond
     ((equal event ?\a) nil) ; C-g (cannot remap C-g)
     ((equal event 32) ?')   ; space
     ((equal event ?/) ?\\)
     ((equal event ?:) ?|)
     ((and (<= ?a event) (<= event ?z)) (- event 32))
     ((equal event ?G) (+ (expt 2 25) ?\a)) ; G is mapped directly to C-S-g
     ((and (<= ?A event) (<= event ?Z)) (- event 64))
     ((and (<= 1 event) (<= event 26)) (+ (expt 2 25) event)))))

(defun spacemacs//evilify-sort-keymap (map)
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

(provide 'core-evilified-state)

;;; core-evilified-state.el ends here
