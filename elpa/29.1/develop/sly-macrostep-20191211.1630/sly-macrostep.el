;;; sly-macrostep.el --- fancy macro-expansion via macrostep.el
;;
;; Version: 0.1
;; URL: https://github.com/capitaomorte/sly-macrostep
;; Keywords: languages, lisp, sly
;; Package-Requires: ((sly "1.0.0-beta2") (macrostep "0.9"))
;; Authors: Luís Oliveira <luismbo@gmail.com>, Jon Oddie <j.j.oddie@gmail.com, João Távora <joaotavora@gmail.com>
;;
;; Copyright (C) 2016 the authors
;; 
;; This file is free software; you can redistribute it and/or modify
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
;;; Description:
;;
;; This is the SLY port of a contrib originally written for SLIME,
;; with minimal changes, mostly "slime"->"sly" replacements.
;;
;; Fancier in-place macro-expansion using macrostep.el (originally
;; written for Emacs Lisp).  To use, position point before the
;; open-paren of the macro call in a SLY source or REPL buffer, and
;; type `C-c M-e' or `M-x macrostep-expand'.  The pretty-printed
;; result of `macroexpand-1' will be inserted inline in the current
;; buffer, which is temporarily read-only while macro expansions are
;; visible.  If the expansion is itself a macro call, expansion can be
;; continued by typing `e'.  Expansions are collapsed to their
;; original macro forms by typing `c' or `q'.  Other macro- and
;; compiler-macro calls in the expansion will be font-locked
;; differently, and point can be moved there quickly by typing `n' or
;; `p'.  For more details, see the documentation of
;; `macrostep-expand'.

;;; Code:

(require 'sly)
(require 'macrostep)
(require 'cl-lib)

(define-sly-contrib sly-macrostep
  "Interactive macro expansion via macrostep.el."
  (:authors "Luís Oliveira       <luismbo@gmail.com>"
            "Jon Oddie           <j.j.oddie@gmail.com>")
  (:license "GPL")
  (:slynk-dependencies slynk-macrostep)
  (:on-load
   (easy-menu-add-item sly-mode-map '(menu-bar SLY Debugging)
                       ["Macro stepper..." macrostep-expand (sly-connected-p)])
   (add-hook 'sly-editing-mode-hook #'sly-macrostep-mode-hook)
   (define-key sly-editing-mode-map (kbd "C-c M-e") #'macrostep-expand)
   (eval-after-load 'sly-mrepl
     '(progn
       (add-hook 'sly-mrepl-mode-hook #'sly-macrostep-mode-hook)
       (define-key sly-mrepl-mode-map (kbd "C-c M-e") #'macrostep-expand)))))

(defun sly-macrostep-mode-hook ()
  (setq macrostep-sexp-at-point-function #'sly-macrostep-sexp-at-point)
  (setq macrostep-environment-at-point-function #'sly-macrostep-context)
  (setq macrostep-expand-1-function #'sly-macrostep-expand-1)
  (setq macrostep-print-function #'sly-macrostep-insert)
  (setq macrostep-macro-form-p-function #'sly-macrostep-macro-form-p))

(defun sly-macrostep-sexp-at-point (&rest _ignore)
  (sly-sexp-at-point))

(defun sly-macrostep-context ()
  (let (defun-start defun-end)
    (save-excursion
      (while
          (condition-case nil
              (progn (backward-up-list) t)
            (scan-error nil)))
      (setq defun-start (point))
      (setq defun-end (scan-sexps (point) 1)))
    (list (buffer-substring-no-properties
           defun-start (point))
          (buffer-substring-no-properties
           (scan-sexps (point) 1) defun-end))))

(defun sly-macrostep-expand-1 (string context)
  (sly-dcase
      (sly-eval
       `(slynk-macrostep:macrostep-expand-1
         ,string ,macrostep-expand-compiler-macros ',context))
    ((:error error-message)
     (error "%s" error-message))
    ((:ok expansion positions)
     (list expansion positions))))

(defun sly-macrostep-insert (result _ignore)
  "Insert RESULT at point, indenting to match the current column."
  (cl-destructuring-bind (expansion positions) result
    (let ((start (point))
          (column-offset (current-column)))
      (insert expansion)
      (sly-macrostep--propertize-macros start positions)
      (indent-rigidly start (point) column-offset))))

(defun sly-macrostep--propertize-macros (start-offset positions)
  "Put text properties on macro forms."
  (dolist (position positions)
    (cl-destructuring-bind (operator type start)
        position
      (let ((open-paren-position
              (+ start-offset start)))
        (put-text-property open-paren-position
                           (1+ open-paren-position)
                           'macrostep-macro-start
                           t)
        ;; this assumes that the operator starts right next to the
        ;; opening parenthesis. We could probably be more robust.
        (let ((op-start (1+ open-paren-position)))
          (put-text-property op-start
                             (+ op-start (length operator))
                             'font-lock-face
                             (if (eq type :macro)
                                 'macrostep-macro-face
                                 'macrostep-compiler-macro-face)))))))

(defun sly-macrostep-macro-form-p (string context)
  (sly-dcase
      (sly-eval
       `(slynk-macrostep:macro-form-p
         ,string ,macrostep-expand-compiler-macros ',context))
    ((:error error-message)
     (error "%s" error-message))
    ((:ok result)
     result)))



;;; Automatically add ourselves to `sly-contribs' when this file is loaded
;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-macrostep 'append))

(provide 'sly-macrostep)
;;; sly-macrostep.el ends here
