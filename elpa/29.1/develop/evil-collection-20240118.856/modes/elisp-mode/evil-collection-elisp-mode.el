;;; evil-collection-elisp-mode.el --- Bindings for `elisp-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, elisp, lisp

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
;;; Bindings for `elisp-mode'.

;;; Code:
(require 'elisp-mode)
(require 'evil-collection)

(defconst evil-collection-elisp-mode-maps '(emacs-lisp-mode-map))

(defun evil-collection-elisp-mode-last-sexp-setup-props (beg end value alt1 alt2)
  "Set up text properties for the output of `elisp--eval-last-sexp'.
BEG and END are the start and end of the output in current-buffer.
VALUE is the Lisp value printed, ALT1 and ALT2 are strings for the
alternative printed representations that can be displayed."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'evil-collection-elisp-mode-return-or-last-sexp-toggle-display)
    (define-key map [down-mouse-2] 'mouse-set-point)
    (define-key map [mouse-2] 'elisp-last-sexp-toggle-display)
    (add-text-properties
     beg end
     `(printed-value (,value ,alt1 ,alt2)
                     mouse-face highlight
                     keymap ,map
                     help-echo "RET, mouse-2: toggle abbreviated display"
                     rear-nonsticky (mouse-face keymap help-echo
                                                printed-value)))))

(defun evil-collection-elisp-mode-return-or-last-sexp-toggle-display ()
  "Trigger RET or call `elisp-last-sexp-toggle-display'."
  (interactive)
  (if (eq evil-state 'insert)
      (call-interactively
       (lookup-key (current-global-map) (kbd "C-m")))
    (call-interactively 'elisp-last-sexp-toggle-display)))

(defun evil-collection-elisp-mode-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

(defun evil-collection-elisp-mode-ielm-repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

;;;###autoload
(defun evil-collection-elisp-mode-setup ()
  "Set up `evil' bindings for `elisp-mode'."
  (unless evil-move-beyond-eol
    (advice-add 'eval-print-last-sexp :around 'evil-collection-elisp-mode-last-sexp))
  (advice-add 'last-sexp-setup-props
              :override 'evil-collection-elisp-mode-last-sexp-setup-props)

  (evil-set-initial-state 'emacs-lisp-mode 'normal)
  (evil-collection-define-key 'normal 'emacs-lisp-mode-map
    "gz" 'evil-collection-elisp-mode-ielm-repl)

  (when evil-collection-want-find-usages-bindings
    (evil-collection-define-key 'normal 'emacs-lisp-mode-map
      "gr" 'xref-find-references)))

(provide 'evil-collection-elisp-mode)
;;; evil-collection-elisp-mode.el ends here
