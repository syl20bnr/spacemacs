;;; macrostep.el --- Interactive macro expander  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2015 Jon Oddie
;; Copyright (C) 2020-2023 Free Software Foundation, Inc.

;; Author: Jon Oddie <j.j.oddie@gmail.com>
;; Url: https://github.com/emacsorphanage/macrostep
;; Keywords: lisp, languages, macro, debugging

;; Package-Version: 0.9.2
;; Package-Requires: ((cl-lib "0.5"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `macrostep' is an Emacs minor mode for interactively stepping through
;; the expansion of macros in Emacs Lisp source code.  It lets you see
;; exactly what happens at each step of the expansion process by
;; pretty-printing the expanded forms inline in the source buffer, which is
;; temporarily read-only while macro expansions are visible.  You can
;; expand and collapse macro forms one step at a time, and evaluate or
;; instrument the expansions for debugging with Edebug as normal (but see
;; "Bugs and known limitations", below).  Single-stepping through the
;; expansion is particularly useful for debugging macros that expand into
;; another macro form.  These can be difficult to debug with Emacs'
;; built-in `macroexpand', which continues expansion until the top-level
;; form is no longer a macro call.

;; Both globally-visible macros as defined by `defmacro' and local macros
;; bound by `(cl-)macrolet' or another macro-defining form can be expanded.
;; Within macro expansions, calls to macros and compiler macros are
;; fontified specially: macro forms using `macrostep-macro-face', and
;; functions with compiler macros using `macrostep-compiler-macro-face'.
;; Uninterned symbols (gensyms) are fontified based on which step in the
;; expansion created them, to distinguish them both from normal symbols and
;; from other gensyms with the same print name.

;; As of version 0.9, it is also possible to extend `macrostep' to work
;; with other languages with macro systems in addition to Emacs Lisp.  An
;; extension for Common Lisp (via SLIME) is in the works; contributions for
;; other languages are welcome.  See "Extending macrostep" below for
;; details.


;; 1 Key-bindings and usage
;; ========================

;;   The standard keybindings in `macrostep-mode' are the following:

;;   e, =, RET : expand the macro form following point one step
;;   c, u, DEL : collapse the form following point
;;   q, C-c C-c: collapse all expanded forms and exit macrostep-mode
;;   n, TAB    : jump to the next macro form in the expansion
;;   p, M-TAB  : jump to the previous macro form in the expansion

;;   It's not very useful to enable and disable macrostep-mode directly.
;;   Instead, bind `macrostep-expand' to a key in `emacs-lisp-mode-map',
;;   for example C-c e:

;;   ,----
;;   | (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
;;   `----

;;   You can then enter macrostep-mode and expand a macro form completely
;;   by typing `C-c e e e ...' as many times as necessary.

;;   Exit macrostep-mode by typing `q' or `C-c C-c', or by successively
;;   typing `c' to collapse all surrounding expansions.


;; 2 Customization options
;; =======================

;;   Type `M-x customize-group RET macrostep RET' to customize options and
;;   faces.

;;   To display macro expansions in a separate window, instead of inline in
;;   the source buffer, customize `macrostep-expand-in-separate-buffer' to
;;   `t'.  The default is `nil'.  Whichever default behavior is selected,
;;   the alternative behavior can be obtained temporarily by giving a
;;   prefix argument to `macrostep-expand'.

;;   To have `macrostep' ignore compiler macros, customize
;;   `macrostep-expand-compiler-macros' to `nil'.  The default is `t'.

;;   Customize the faces `macrostep-macro-face',
;;   `macrostep-compiler-macro-face', and `macrostep-gensym-1' through
;;   `macrostep-gensym-5' to alter the appearance of macro expansions.


;; 3 Locally-bound macros
;; ======================

;;   As of version 0.9, `macrostep' can expand calls to a locally-bound
;;   macro, whether defined by a surrounding `(cl-)macrolet' form, or by
;;   another macro-defining macro.  In other words, it is possible to
;;   expand the inner `local-macro' forms in both the following examples,
;;   whether `local-macro' is defined by an enclosing `cl-macrolet' --

;;   ,----
;;   | (cl-macrolet ((local-macro (&rest args)
;;   |                 `(expansion of ,args)))
;;   |   (local-macro (do-something)))
;;   `----

;;   -- or by a macro which expands into `cl-macrolet', provided that its
;;   definition of macro is evaluated prior to calling `macrostep-expand':

;;   ,----
;;   | (defmacro with-local-macro (&rest body)
;;   |   `(cl-macrolet ((local-macro (&rest args)
;;   |                    `(expansion of ,args)))
;;   |      ,@body))
;;   |
;;   | (with-local-macro
;;   |     (local-macro (do something (else)))
;;   `----

;;   See the `with-js' macro in Emacs's `js.el' for a real example of the
;;   latter kind of macro.

;;   Expansion of locally-bound macros is implemented by instrumenting
;;   Emacs Lisp's macro-expander to capture the environment at point.  A
;;   similar trick is used to detect macro- and compiler-macro calls within
;;   expanded text so that they can be fontified accurately.


;; 4 Expanding sub-forms
;; =====================

;;   By moving point around in the macro expansion using
;;   `macrostep-next-macro' and `macrostep-prev-macro' (bound to the `n'
;;   and `p' keys), it is possible to expand other macro calls within the
;;   expansion before expanding the outermost form.  This can sometimes be
;;   useful, although it does not correspond to the real order of macro
;;   expansion in Emacs Lisp, which proceeds by fully expanding the outer
;;   form to a non-macro form before expanding sub-forms.

;;   The main reason to expand sub-forms out of order is to help with
;;   debugging macros which programmatically expand their arguments in
;;   order to rewrite them.  Expanding the arguments of such a macro lets
;;   you visualise what the macro definition would compute via
;;   `macroexpand-all'.


;; 5 Extending macrostep for other languages
;; =========================================

;;   Since version 0.9, it is possible to extend macrostep to work with
;;   other languages besides Emacs Lisp.  In typical Emacs fashion, this is
;;   implemented by setting buffer-local variables to different function
;;   values.  Six buffer-local variables define the language-specific part
;;   of the implementation:

;;   - `macrostep-sexp-bounds-function'
;;   - `macrostep-sexp-at-point-function'
;;   - `macrostep-environment-at-point-function'
;;   - `macrostep-expand-1-function'
;;   - `macrostep-print-function'
;;   - `macrostep-macro-form-p-function'

;;   Typically, an implementation for another language would set these
;;   variables in a major-mode hook.  See the docstrings of each variable
;;   for details on how each one is called and what it should return.  At a
;;   minimum, another language implementation needs to provide
;;   `macrostep-sexp-at-point-function', `macrostep-expand-1-function', and
;;   `macrostep-print-function'.  Lisp-like languages may be able to reuse
;;   the default `macrostep-sexp-bounds-function' if they provide another
;;   implementation of `macrostep-macro-form-p-function'.  Languages which
;;   do not implement locally-defined macros can set
;;   `macrostep-environment-at-point-function' to `ignore'.

;;   Note that the core `macrostep' machinery only interprets the return
;;   value of `macrostep-sexp-bounds-function', so implementations for
;;   other languages can use any internal representations of code and
;;   environments which is convenient.  Although the terminology is
;;   Lisp-specific, there is no reason that implementations could not be
;;   provided for non-Lisp languages with macro systems, provided there is
;;   some way of identifying macro calls and calling the compiler /
;;   preprocessor to obtain their expansions.


;; 6 Bugs and known limitations
;; ============================

;;   You can evaluate and edebug macro-expanded forms and step through the
;;   macro-expanded version, but the form that `eval-defun' and friends
;;   read from the buffer won't have the uninterned symbols of the real
;;   macro expansion.  This will probably work OK with CL-style gensyms,
;;   but may cause problems with `make-symbol' symbols if they have the
;;   same print name as another symbol in the expansion.  It's possible that
;;   using `print-circle' and `print-gensym' could get around this.

;;   Please send other bug reports and feature requests to the author.


;; 7 Acknowledgements
;; ==================

;;   Thanks to:
;;   - John Wiegley for fixing a bug with the face definitions under Emacs
;;     24 & for plugging macrostep in his [EmacsConf presentation]!
;;   - George Kettleborough for bug reports, and patches to highlight the
;;     expanded region and properly handle backquotes.
;;   - Nic Ferrier for suggesting support for local definitions within
;;     macrolet forms
;;   - Luís Oliveira for suggesting and implementing SLIME support

;;   `macrostep' was originally inspired by J. V. Toups's 'Deep Emacs Lisp'
;;   articles ([part 1], [part 2], [screencast]).

;;   [EmacsConf presentation] http://youtu.be/RvPFZL6NJNQ

;;   [part 1]
;;   http://dorophone.blogspot.co.uk/2011/04/deep-emacs-part-1.html

;;   [part 2]
;;   http://dorophone.blogspot.co.uk/2011/04/deep-emacs-lisp-part-2.html

;;   [screencast]
;;   http://dorophone.blogspot.co.uk/2011/05/monadic-parser-combinators-in-elisp.html


;; 8 Changelog
;; ===========

;;   - v0.9.2, 2023-05-12:
;;     - name the keymap macrostep-mode-map, fixing a regression in v0.9.1
;;   - v0.9.1, 2023-03-12:
;;     - bug fixes, cleanup and modernization
;;   - v0.9, 2015-10-01:
;;     - separate into Elisp-specific and generic components
;;     - highlight and expand compiler macros
;;     - improve local macro expansion and macro form identification by
;;       instrumenting `macroexpand(-all)'
;;   - v0.8, 2014-05-29: fix a bug with printing the first element of lists
;;   - v0.7, 2014-05-11: expand locally-defined macros within
;;     `(cl-)macrolet' forms
;;   - v0.6, 2013-05-04: better handling of quote and backquote
;;   - v0.5, 2013-04-16: highlight region, maintain cleaner buffer state
;;   - v0.4, 2013-04-07: only enter macrostep-mode on successful
;;     macro-expansion
;;   - v0.3, 2012-10-30: print dotted lists correctly.  autoload
;;     definitions.

;;; Code:

(require 'pp)
(require 'ring)
(require 'cl-lib)


;;; Constants and dynamically bound variables
(defvar macrostep-overlays nil
  "List of all macro stepper overlays in the current buffer.")
(make-variable-buffer-local 'macrostep-overlays)

(defvar macrostep-gensym-depth nil
  "Number of macro expansion levels that have introduced gensyms so far.")
(make-variable-buffer-local 'macrostep-gensym-depth)

(defvar macrostep-gensyms-this-level nil
  "t if gensyms have been encountered during current level of macro expansion.")
(make-variable-buffer-local 'macrostep-gensyms-this-level)

(defvar macrostep-saved-undo-list nil
  "Saved value of `buffer-undo-list' upon entering macrostep mode.")
(make-variable-buffer-local 'macrostep-saved-undo-list)

(defvar macrostep-saved-read-only nil
  "Saved value of `buffer-read-only' upon entering macrostep mode.")
(make-variable-buffer-local 'macrostep-saved-read-only)

(defvar macrostep-expansion-buffer nil
  "Non-nil if the current buffer is a macro-expansion buffer.")
(make-variable-buffer-local 'macrostep-expansion-buffer)

(defvar macrostep-outer-environment nil
  "Outermost macro-expansion environment to use in macro-expansion buffers.

This variable is used to save information about any enclosing
`cl-macrolet' context when a macro form is expanded in a separate
buffer.")
(make-variable-buffer-local 'macrostep-outer-environment)

;;; Customization options and faces
(defgroup macrostep nil
  "Interactive macro stepper for Emacs Lisp."
  :group 'lisp
  :link '(emacs-commentary-link :tag "commentary" "macrostep.el")
  :link '(emacs-library-link :tag "lisp file" "macrostep.el")
  :link '(url-link :tag "web page" "https://github.com/joddie/macrostep"))

(defface macrostep-gensym-1
  '((((min-colors 16581375)) :foreground "#8080c0" :box t :bold t)
    (((min-colors 8)) :background "cyan")
    (t :inverse-video t))
  "Face for gensyms created in the first level of macro expansion.")

(defface macrostep-gensym-2
  '((((min-colors 16581375)) :foreground "#8fbc8f" :box t :bold t)
    (((min-colors 8)) :background "#00cd00")
    (t :inverse-video t))
  "Face for gensyms created in the second level of macro expansion.")

(defface macrostep-gensym-3
  '((((min-colors 16581375)) :foreground "#daa520" :box t :bold t)
    (((min-colors 8)) :background "yellow")
    (t :inverse-video t))
  "Face for gensyms created in the third level of macro expansion.")

(defface macrostep-gensym-4
  '((((min-colors 16581375)) :foreground "#cd5c5c" :box t :bold t)
    (((min-colors 8)) :background "red")
    (t :inverse-video t))
  "Face for gensyms created in the fourth level of macro expansion.")

(defface macrostep-gensym-5
  '((((min-colors 16581375)) :foreground "#da70d6" :box t :bold t)
    (((min-colors 8)) :background "magenta")
    (t :inverse-video t))
  "Face for gensyms created in the fifth level of macro expansion.")

(defface macrostep-expansion-highlight-face
  `((((min-colors 16581375) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#eee8d5")
    (((min-colors 16581375) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "#222222"))
  "Face for macro-expansion highlight.")

(defface macrostep-macro-face
  '((t :underline t))
  "Face for macros in macro-expanded code.")

(defface macrostep-compiler-macro-face
  '((t :slant italic))
  "Face for compiler macros in macro-expanded code.")

(defcustom macrostep-expand-in-separate-buffer nil
  "When non-nil, show expansions in a separate buffer instead of inline."
  :type 'boolean)

(defcustom macrostep-expand-compiler-macros t
  "When non-nil, also expand compiler macros."
  :type 'boolean)

;; Need the following for making the ring of faces
(defun macrostep-make-ring (&rest items)
  "Make a ring containing all of ITEMS with no empty slots."
  (let ((ring (make-ring (length items))))
    (mapc (lambda (item) (ring-insert ring item)) (reverse items))
    ring))

(defvar macrostep-gensym-faces
  (macrostep-make-ring
   'macrostep-gensym-1 'macrostep-gensym-2 'macrostep-gensym-3
   'macrostep-gensym-4 'macrostep-gensym-5)
  "Ring of all macrostepper faces for fontifying gensyms.")

;; Other modes can enable macrostep by redefining these functions to
;; language-specific versions.
(defvar macrostep-sexp-bounds-function
  #'macrostep-sexp-bounds
  "Function to return the bounds of the macro form nearest point.

It will be called with no arguments and should return a cons of
buffer positions, (START . END).  It should use `save-excursion'
to avoid changing the position of point.

The default value, `macrostep-sexp-bounds', implements this for
Emacs Lisp, and may be suitable for other Lisp-like languages.")
(make-variable-buffer-local 'macrostep-sexp-bounds-function)

(defvar macrostep-sexp-at-point-function
  #'macrostep-sexp-at-point
  "Function to return the macro form at point for expansion.

It will be called with two arguments, the values of START and END
returned by `macrostep-sexp-bounds-function', and with point
positioned at START.  It should return a value suitable for
passing as the first argument to `macrostep-expand-1-function'.

The default value, `macrostep-sexp-at-point', implements this for
Emacs Lisp, and may be suitable for other Lisp-like languages.")
(make-variable-buffer-local 'macrostep-sexp-at-point-function)

(defvar macrostep-environment-at-point-function
  #'macrostep-environment-at-point
  "Function to return the local macro-expansion environment at point.

It will be called with no arguments, and should return a value
suitable for passing as the second argument to
`macrostep-expand-1-function'.

The default value, `macrostep-environment-at-point', is specific
to Emacs Lisp.  For languages which do not implement local
macro-expansion environments, this should be set to `ignore'
or `(lambda () nil)'.")
(make-variable-buffer-local 'macrostep-environment-at-point-function)

(defvar macrostep-expand-1-function
  #'macrostep-expand-1
  "Function to perform one step of macro-expansion.

It will be called with two arguments, FORM and ENVIRONMENT, the
return values of `macrostep-sexp-at-point-function' and
`macrostep-environment-at-point-function' respectively.  It
should return the result of expanding FORM by one step as a value
which is suitable for passing as the argument to
`macrostep-print-function'.

The default value, `macrostep-expand-1', is specific to Emacs Lisp.")
(make-variable-buffer-local 'macrostep-expand-1-function)

(defvar macrostep-print-function
  #'macrostep-pp
  "Function to pretty-print macro expansions.

It will be called with two arguments, FORM and ENVIRONMENT, the
return values of `macrostep-sexp-at-point-function' and
`macrostep-environment-at-point-function' respectively.  It
should insert a pretty-printed representation at point in the
current buffer, leaving point just after the inserted
representation, without altering any other text in the current
buffer.

The default value, `macrostep-pp', is specific to Emacs Lisp.")
(make-variable-buffer-local 'macrostep-print-function)

(defvar macrostep-macro-form-p-function
  #'macrostep-macro-form-p
  "Function to check whether a form is a macro call.

It will be called with two arguments, FORM and ENVIRONMENT -- the
return values of `macrostep-sexp-at-point-function' and
`macrostep-environment-at-point-function' respectively -- and
should return non-nil if FORM would undergo macro-expansion in
ENVIRONMENT.

This is called only from `macrostep-sexp-bounds', so it need not
be provided if a different value is used for
`macrostep-sexp-bounds-function'.

The default value, `macrostep-macro-form-p', is specific to Emacs Lisp.")
(make-variable-buffer-local 'macrostep-macro-form-p-function)


;;; Define keymap and minor mode
(define-obsolete-variable-alias 'macrostep-mode-keymap 'macrostep-mode-map "2023")
(define-obsolete-variable-alias 'macrostep-keymap 'macrostep-mode-map "2022")
(defvar macrostep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'macrostep-expand)
    (define-key map "=" #'macrostep-expand)
    (define-key map "e" #'macrostep-expand)

    (define-key map (kbd "DEL") #'macrostep-collapse)
    (define-key map "u" #'macrostep-collapse)
    (define-key map "c" #'macrostep-collapse)

    (define-key map (kbd "TAB") #'macrostep-next-macro)
    (define-key map "n" #'macrostep-next-macro)
    (define-key map (kbd "M-TAB") #'macrostep-prev-macro)
    (define-key map "p" #'macrostep-prev-macro)

    (define-key map "q" #'macrostep-collapse-all)
    (define-key map (kbd "C-c C-c") #'macrostep-collapse-all)
    map)
  "Keymap for `macrostep-mode'.")

;;;###autoload
(define-minor-mode macrostep-mode
  "Minor mode for inline expansion of macros in Emacs Lisp source buffers.

\\<macrostep-mode-map>Progressively expand macro forms with \
\\[macrostep-expand], collapse them with \\[macrostep-collapse],
and move back and forth with \\[macrostep-next-macro] and \
\\[macrostep-prev-macro].  Use \\[macrostep-collapse-all] or collapse all
visible expansions to quit and return to normal editing.

\\{macrostep-mode-map}"
  :lighter " Macro-Stepper"
  :group 'macrostep
  (if macrostep-mode
      (progn
        ;; Disable recording of undo information
        (setq macrostep-saved-undo-list buffer-undo-list
              buffer-undo-list t)
        ;; Remember whether buffer was read-only
        (setq macrostep-saved-read-only buffer-read-only
              buffer-read-only t)
        ;; Set up post-command hook to bail out on leaving read-only
        (add-hook 'post-command-hook #'macrostep-command-hook nil t)
        (message (substitute-command-keys "\
\\<macrostep-mode-map>Entering macro stepper mode. \
Use \\[macrostep-expand] to expand, \\[macrostep-collapse] to collapse, \
\\[macrostep-collapse-all] to exit.")))

    ;; Exiting mode
    (if macrostep-expansion-buffer
        ;; Kill dedicated expansion buffers
        (quit-window t)
      ;; Collapse any remaining overlays
      (when macrostep-overlays (macrostep-collapse-all))
      ;; Restore undo info & read-only state
      (setq buffer-undo-list macrostep-saved-undo-list
            buffer-read-only macrostep-saved-read-only
            macrostep-saved-undo-list nil)
      ;; Remove our post-command hook
      (remove-hook 'post-command-hook #'macrostep-command-hook t))))

;; Post-command hook: bail out of macrostep-mode if the user types C-x
;; C-q to make the buffer writable again.
(defun macrostep-command-hook ()
  (if (not buffer-read-only)
      (macrostep-mode 0)))


;;; Interactive functions
;;;###autoload
(defun macrostep-expand (&optional toggle-separate-buffer)
  "Expand the macro form following point by one step.

Enters `macrostep-mode' if it is not already active, making the
buffer temporarily read-only.  If `macrostep-mode' is active and the
form following point is not a macro form, search forward in the
buffer and expand the next macro form found, if any.

With a prefix argument, the expansion is displayed in a separate
buffer instead of inline in the current buffer.  Setting
`macrostep-expand-in-separate-buffer' to non-nil swaps these two
behaviors."
  (interactive "P")
  (cl-destructuring-bind (start . end)
      (funcall macrostep-sexp-bounds-function)
    (goto-char start)
    (let* ((sexp (funcall macrostep-sexp-at-point-function start end))
           (end (copy-marker end))
           (text (buffer-substring start end))
           (env (funcall macrostep-environment-at-point-function))
           (expansion (funcall macrostep-expand-1-function sexp env)))

      ;; Create a dedicated macro-expansion buffer and copy the text to
      ;; be expanded into it, if required
      (let ((separate-buffer-p
             (if toggle-separate-buffer
                 (not macrostep-expand-in-separate-buffer)
               macrostep-expand-in-separate-buffer)))
        (when (and separate-buffer-p (not macrostep-expansion-buffer))
          (let ((mode major-mode)
                (buffer
                 (get-buffer-create (generate-new-buffer-name "*macro expansion*"))))
            (set-buffer buffer)
            (funcall mode)
            (setq macrostep-expansion-buffer t)
            (setq macrostep-outer-environment env)
            (save-excursion
              (setq start (point))
              (insert text)
              (setq end (point-marker)))
            (pop-to-buffer buffer))))

      (unless macrostep-mode (macrostep-mode t))
      (let ((existing-overlay (macrostep-overlay-at-point))
            (macrostep-gensym-depth macrostep-gensym-depth)
            (macrostep-gensyms-this-level nil)
            priority)
        (if existing-overlay
            (progn        ; Expanding part of a previous macro-expansion
              (setq priority (1+ (overlay-get existing-overlay 'priority)))
              (setq macrostep-gensym-depth
                    (overlay-get existing-overlay 'macrostep-gensym-depth)))
          ;; Expanding source buffer text
          (setq priority 1)
          (setq macrostep-gensym-depth -1))

        (with-silent-modifications
          (atomic-change-group
            (let ((inhibit-read-only t))
              (save-excursion
                ;; Insert expansion
                (funcall macrostep-print-function expansion env)
                ;; Delete the original form
                (macrostep-collapse-overlays-in (point) end)
                (delete-region (point) end)
                ;; Create a new overlay
                (let* ((overlay
                        (make-overlay start
                                      (if (looking-at "\n")
                                          (1+ (point))
                                        (point))))
                       (highlight-overlay (unless macrostep-expansion-buffer
                                            (copy-overlay overlay))))
                  (unless macrostep-expansion-buffer
                    ;; Highlight the overlay in original source buffers only
                    (overlay-put highlight-overlay 'face 'macrostep-expansion-highlight-face)
                    (overlay-put highlight-overlay 'priority -1)
                    (overlay-put overlay 'macrostep-highlight-overlay highlight-overlay))
                  (overlay-put overlay 'priority priority)
                  (overlay-put overlay 'macrostep-original-text text)
                  (overlay-put overlay 'macrostep-gensym-depth macrostep-gensym-depth)
                  (push overlay macrostep-overlays))))))))))

(defun macrostep-collapse ()
  "Collapse the innermost macro expansion near point to its source text.

If no more macro expansions are visible after this, exit
`macrostep-mode'."
  (interactive)
  (let ((overlay (macrostep-overlay-at-point)))
    (when (not overlay) (error "No macro expansion at point"))
    (let ((inhibit-read-only t))
      (with-silent-modifications
        (atomic-change-group
          (macrostep-collapse-overlay overlay)))))
  (if (not macrostep-overlays)
      (macrostep-mode 0)))

(defun macrostep-collapse-all ()
  "Collapse all visible macro expansions and exit `macrostep-mode'."
  (interactive)
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (dolist (overlay macrostep-overlays)
        (let ((outermost (= (overlay-get overlay 'priority) 1)))
          ;; We only need restore the original text for the outermost
          ;; overlays
          (macrostep-collapse-overlay overlay (not outermost))))))
  (setq macrostep-overlays nil)
  (macrostep-mode 0))

(defun macrostep-next-macro ()
  "Move point forward to the next macro form in macro-expanded text."
  (interactive)
  (let* ((start (if (get-text-property (point) 'macrostep-macro-start)
                    (1+ (point))
                  (point)))
         (next (next-single-property-change start 'macrostep-macro-start)))
    (if next
        (goto-char next)
      (error "No more macro forms found"))))

(defun macrostep-prev-macro ()
  "Move point back to the previous macro form in macro-expanded text."
  (interactive)
  (let (prev)
    (save-excursion
      (while
          (progn
            (setq prev (previous-single-property-change
                        (point) 'macrostep-macro-start))
            (if (or (not prev)
                    (get-text-property (1- prev) 'macrostep-macro-start))
                nil
              (prog1 t (goto-char prev))))))
    (if prev
        (goto-char (1- prev))
      (error "No previous macro form found"))))


;;; Utility functions (not language-specific)

(defun macrostep-overlay-at-point ()
  "Return the innermost macro stepper overlay at point."
  (cdr (get-char-property-and-overlay (point) 'macrostep-original-text)))

(defun macrostep-collapse-overlay (overlay &optional no-restore-p)
  "Collapse a macro-expansion overlay and restore the unexpanded source text.

As a minor optimization, does not restore the original source
text if NO-RESTORE-P is non-nil.  This is safe to do when
collapsing all the sub-expansions of an outer overlay, since the
outer overlay will restore the original source itself.

Also removes the overlay from `macrostep-overlays'."
  (with-current-buffer (overlay-buffer overlay)
    ;; If we're cleaning up we don't need to bother restoring text
    ;; or checking for inner overlays to delete
    (unless no-restore-p
      (let* ((start (overlay-start overlay))
             (end (overlay-end overlay))
             (text (overlay-get overlay 'macrostep-original-text))
             (sexp-end
              (copy-marker
               (if (equal (char-before end) ?\n) (1- end) end))))
        (macrostep-collapse-overlays-in start end)
        (goto-char (overlay-start overlay))
        (save-excursion
          (insert text)
          (delete-region (point) sexp-end))))
    ;; Remove overlay from the list and delete it
    (setq macrostep-overlays
          (delq overlay macrostep-overlays))
    (let ((highlight-overlay (overlay-get overlay 'macrostep-highlight-overlay)))
      (when highlight-overlay (delete-overlay highlight-overlay)))
    (delete-overlay overlay)))

(defun macrostep-collapse-overlays-in (start end)
  "Collapse all macrostepper overlays that are strictly between START and END.

Will not collapse overlays that begin at START and end at END."
  (dolist (ol (overlays-in start end))
    (when (and (overlay-buffer ol)        ; collapsing may delete other overlays
               (> (overlay-start ol) start)
               (< (overlay-end ol) end)
               (overlay-get ol 'macrostep-original-text))
      (macrostep-collapse-overlay ol t))))


;;; Emacs Lisp implementation

(defun macrostep-sexp-bounds ()
  "Find the bounds of the macro form nearest point.

If point is not before an open-paren, moves up to the nearest
enclosing list.  If the form at point is not a macro call,
attempts to move forward to the next macro form as determined by
`macrostep-macro-form-p-function'.

Returns a cons of buffer positions, (START . END)."
  (save-excursion
    (if (not (looking-at "[(`]"))
        (backward-up-list 1))
    (if (equal (char-before) ?`)
        (backward-char))
    (let ((sexp (funcall macrostep-sexp-at-point-function))
          (env (funcall macrostep-environment-at-point-function)))
      ;; If this isn't a macro form, try to find the next one in the buffer
      (unless (funcall macrostep-macro-form-p-function sexp env)
        (condition-case nil
            (macrostep-next-macro)
          (error
           (if (consp sexp)
               (error "(%s ...) is not a macro form" (car sexp))
             (error "Text at point is not a macro form"))))))
    (cons (point) (scan-sexps (point) 1))))

(defun macrostep-sexp-at-point (&rest _ignore)
  "Return the sexp near point for purposes of macro-stepper expansion.

If the sexp near point is part of a macro expansion, returns the
saved text of the macro expansion, and does not read from the
buffer.  This preserves uninterned symbols in the macro
expansion, so that they can be fontified consistently.  (See
`macrostep-print-sexp'.)"
  (or (get-text-property (point) 'macrostep-expanded-text)
      (sexp-at-point)))

(defun macrostep-macro-form-p (form environment)
  "Return non-nil if FORM would be evaluated via macro expansion.

If FORM is an invocation of a macro defined by `defmacro' or an
enclosing `cl-macrolet' form, return the symbol `macro'.

If `macrostep-expand-compiler-macros' is non-nil and FORM is a
call to a function with a compiler macro, return the symbol
`compiler-macro'.

Otherwise, return nil."
  (car (macrostep--macro-form-info form environment t)))

(defun macrostep--macro-form-info (form environment &optional inhibit-autoload)
  "Return information about macro definitions that apply to FORM.

If no macros are involved in the evaluation of FORM within
ENVIRONMENT, returns nil.  Otherwise, returns a cons (TYPE
. DEFINITION).

If FORM would be evaluated by a macro defined by `defmacro',
`cl-macrolet', etc., TYPE is the symbol `macro' and DEFINITION is
the macro definition, as a function.

If `macrostep-expand-compiler-macros' is non-nil and FORM would
be compiled using a compiler macro, TYPE is the symbol
`compiler-macro' and DEFINITION is the function that implements
the compiler macro.

If FORM is an invocation of an autoloaded macro, the behavior
depends on the value of INHIBIT-AUTOLOAD.  If INHIBIT-AUTOLOAD is
nil, the file containing the macro definition will be loaded
using `load-library' and the macro definition returned as normal.
If INHIBIT-AUTOLOAD is non-nil, no files will be loaded, and the
value of DEFINITION in the result will be nil."
  (if (not (and (consp form)
                (symbolp (car form))))
      `(nil . nil)
    (let* ((head (car form))
           (local-definition (assoc-default head environment #'eq)))
      (if local-definition
          `(macro . ,local-definition)
        (let ((compiler-macro-definition
               (and macrostep-expand-compiler-macros
                    (or (get head 'compiler-macro)
                        (get head 'cl-compiler-macro)))))
          (if (and compiler-macro-definition
                   (not (eq form
                            (apply compiler-macro-definition form (cdr form)))))
              `(compiler-macro . ,compiler-macro-definition)
            (condition-case nil
                (let ((fun (indirect-function head)))
                  (cl-case (car-safe fun)
                    ((macro)
                     `(macro . ,(cdr fun)))
                    ((autoload)
                     (when (memq (nth 4 fun) '(macro t))
                       (if inhibit-autoload
                           `(macro . nil)
                         (load-library (nth 1 fun))
                         (macrostep--macro-form-info form nil))))
                    (t
                     `(nil . nil))))
              (void-function nil))))))))

(defun macrostep-expand-1 (form environment)
  "Return result of macro-expanding the top level of FORM by exactly one step.
Unlike `macroexpand', this function does not continue macro
expansion until a non-macro-call results."
  (cl-destructuring-bind (type . definition)
      (macrostep--macro-form-info form environment)
    (cl-ecase type
      ((nil)
       form)
      ((macro)
       (apply definition (cdr form)))
      ((compiler-macro)
       (let ((expansion (apply definition form (cdr form))))
         (if (equal form expansion)
             (error "Form left unchanged by compiler macro")
           expansion))))))

(put 'macrostep-grab-environment-failed 'error-conditions
     '(macrostep-grab-environment-failed error))

(defun macrostep-environment-at-point ()
  "Return the local macro-expansion environment at point, if any.

The local environment includes macros declared by any `macrolet'
or `cl-macrolet' forms surrounding point, as well as by any macro
forms which expand into a `macrolet'.

The return value is an alist of elements (NAME . FUNCTION), where
NAME is the symbol locally bound to the macro and FUNCTION is the
lambda expression that returns its expansion."
  ;; If point is on a macro form within an expansion inserted by
  ;; `macrostep-print-sexp', a local environment may have been
  ;; previously saved as a text property.
  (let ((saved-environment
         (get-text-property (point) 'macrostep-environment)))
    (if saved-environment
        saved-environment
      ;; Otherwise, we (ab)use the macro-expander to return the
      ;; environment at point.  If point is not at an evaluated
      ;; position in the containing form,
      ;; `macrostep-environment-at-point-1' will raise an error, and
      ;; we back up progressively through the containing forms until
      ;; it succeeds.
      (save-excursion
        (catch 'done
          (while t
            (condition-case nil
                (throw 'done (macrostep-environment-at-point-1))
              (macrostep-grab-environment-failed
               (condition-case nil
                   (backward-sexp)
                 (scan-error (backward-up-list)))))))))))

(defun macrostep-environment-at-point-1 ()
  "Attempt to extract the macro environment that would be active at point.

If point is not at an evaluated position within the containing
form, raise an error."
  ;; Macro environments are extracted using Emacs Lisp's builtin
  ;; macro-expansion machinery.  The form containing point is copied
  ;; to a temporary buffer, and a call to
  ;; `--macrostep-grab-environment--' is inserted at point.  This
  ;; altered form is then fully macro-expanded, in an environment
  ;; where `--macrostep-grab-environment--' is defined as a macro
  ;; which throws the environment to a uniquely-generated tag.
  (let* ((point-at-top-level
          (save-excursion
            (while (ignore-errors (backward-up-list) t))
            (point)))
         (enclosing-form
          (buffer-substring point-at-top-level
                            (scan-sexps point-at-top-level 1)))
         (position (- (point) point-at-top-level))
         (tag (make-symbol "macrostep-grab-environment-tag"))
         (grab-environment '--macrostep-grab-environment--))
    (if (= position 0)
        nil
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert enclosing-form)
        (goto-char (+ (point-min) position))
        (prin1 `(,grab-environment) (current-buffer))
        (let ((form (read (copy-marker (point-min)))))
          (catch tag
            (cl-letf (((symbol-function #'message) (symbol-function #'format)))
              (with-no-warnings
                (ignore-errors
                  (macroexpand-all
                   `(cl-macrolet ((,grab-environment (&environment env)
                                    (throw ',tag env)))
                      ,form)))))
            (signal 'macrostep-grab-environment-failed nil)))))))

(defun macrostep-collect-macro-forms (form &optional environment)
  "Identify sub-forms of FORM which undergo macro-expansion.

FORM is an Emacs Lisp form.  ENVIRONMENT is a local environment of
macro definitions.

The return value is a list of two elements, (MACRO-FORM-ALIST
COMPILER-MACRO-FORMS).

MACRO-FORM-ALIST is an alist of elements of the form (SUBFORM
. ENVIRONMENT), where SUBFORM is a form which undergoes
macro-expansion in the course of expanding FORM, and ENVIRONMENT
is the local macro environment in force when it is expanded.

COMPILER-MACRO-FORMS is a list of subforms which would be
compiled using a compiler macro.  Since there is no standard way
to provide a local compiler-macro definition in Emacs Lisp, no
corresponding local environments are collected for these.

Forms and environments are extracted from FORM by instrumenting
Emacs's builtin `macroexpand' function and calling
`macroexpand-all'."
  (let* ((macro-form-alist '())
         (compiler-macro-forms '())
         (override (lambda (real-macroexpand form environment &rest args)
                     (let ((expansion
                            (apply real-macroexpand form environment args)))
                       (cond ((not (eq expansion form))
                              (setq macro-form-alist
                                    (cons (cons form environment)
                                          macro-form-alist)))
                             ((and (consp form)
                                   (symbolp (car form))
                                   macrostep-expand-compiler-macros
                                   (not (eq form
                                            (cl-compiler-macroexpand form))))
                              (setq compiler-macro-forms
                                    (cons form compiler-macro-forms))))
                       expansion))))
    (cl-macrolet ((with-override (fn &rest body)
                    `(cl-letf (((symbol-function ,fn)
                                (apply-partially override (indirect-function ,fn))))
                       ,@body))
                  (with-macroexpand-1 (&rest body)
                    (if (< emacs-major-version 30)
                        `(progn ,@body) `(with-override #'macroexpand-1 ,@body)))
                  (with-macroexpand (&rest body)
                    `(with-override #'macroexpand ,@body)))
      (with-macroexpand-1
       (with-macroexpand
        (ignore-errors
          (macroexpand-all form environment)))))
    (list macro-form-alist compiler-macro-forms)))

(defvar macrostep-collected-macro-form-alist nil
  "An alist of macro forms and environments.
Controls the printing of sub-forms in `macrostep-print-sexp'.")

(defvar macrostep-collected-compiler-macro-forms nil
  "A list of compiler-macro forms to be highlighted in `macrostep-print-sexp'.")

(defun macrostep-pp (sexp environment)
  "Pretty-print SEXP, fontifying macro forms and uninterned symbols."
  (cl-destructuring-bind
        (macrostep-collected-macro-form-alist
         macrostep-collected-compiler-macro-forms)
      (macrostep-collect-macro-forms sexp environment)
    (let ((print-quoted t))
      (macrostep-print-sexp sexp)
      ;; Point is now after the expanded form; pretty-print it
      (save-restriction
        (narrow-to-region (scan-sexps (point) -1) (point))
        (save-excursion
          (pp-buffer)
          ;; Remove the extra newline inserted by pp-buffer
          (goto-char (point-max))
          (delete-region
           (point)
           (save-excursion (skip-chars-backward " \t\n") (point))))
        ;; Indent the newly-inserted form in context
        (widen)
        (save-excursion
          (backward-sexp)
          (indent-sexp))))))

;; This must be defined before `macrostep-print-sexp':
(defmacro macrostep-propertize (form &rest plist)
  "Evaluate FORM, applying syntax properties in PLIST to any inserted text."
  (declare (indent 1)
           (debug (&rest form)))
  (let ((start (make-symbol "start")))
    `(let ((,start (point)))
       (prog1
           ,form
         ,@(cl-loop for (key value) on plist by #'cddr
                    collect `(put-text-property ,start (point)
                                                ,key ,value))))))

(defun macrostep-print-sexp (sexp)
  "Insert SEXP like `print', fontifying macro forms and uninterned symbols.

Fontifies uninterned symbols and macro forms using
`font-lock-face' property, and saves the actual text of SEXP's
sub-forms as the `macrostep-expanded-text' text property so that
any uninterned symbols can be reused in macro expansions of the
sub-forms.  See also `macrostep-sexp-at-point'.

Macro and compiler-macro forms within SEXP are identified by
comparison with the `macrostep-collected-macro-form-alist' and
`macrostep-collected-compiler-macro-forms' variables, which
should be dynamically let-bound around calls to this function."
  (cond
   ((symbolp sexp)
    ;; Fontify gensyms
    (if (not (eq sexp (intern-soft (symbol-name sexp))))
        (macrostep-propertize
            (prin1 sexp (current-buffer))
          'font-lock-face (macrostep-get-gensym-face sexp))
      ;; Print other symbols as normal
      (prin1 sexp (current-buffer))))

   ((listp sexp)
    ;; Print quoted and quasiquoted forms nicely.
    (let ((head (car sexp)))
      (cond ((and (eq head 'quote)      ; quote
                  (= (length sexp) 2))
             (insert "'")
             (macrostep-print-sexp (cadr sexp)))

            ((and (eq head '\`)         ; backquote
                  (= (length sexp) 2))
             (if (assq sexp macrostep-collected-macro-form-alist)
                 (macrostep-propertize
                     (insert "`")
                   'macrostep-expanded-text sexp
                   'macrostep-macro-start t
                   'font-lock-face 'macrostep-macro-face)
               (insert "`"))
             (macrostep-print-sexp (cadr sexp)))

            ((and (memq head '(\, \,@)) ; unquote
                  (= (length sexp) 2))
             (princ head (current-buffer))
             (macrostep-print-sexp (cadr sexp)))

            (t                          ; other list form
             (cl-destructuring-bind (macro? . environment)
                 (or (assq sexp macrostep-collected-macro-form-alist)
                     '(nil . nil))
               (let
                   ((compiler-macro?
                     (memq sexp macrostep-collected-compiler-macro-forms)))
                 (if (or macro? compiler-macro?)
                     (progn
                       ;; Save the real expansion as a text property on the
                       ;; opening paren
                       (macrostep-propertize
                        (insert "(")
                        'macrostep-macro-start t
                        'macrostep-expanded-text sexp
                        'macrostep-environment environment)
                       ;; Fontify the head of the macro
                       (macrostep-propertize
                        (macrostep-print-sexp head)
                        'font-lock-face
                        (if macro?
                            'macrostep-macro-face
                          'macrostep-compiler-macro-face)))
                   ;; Not a macro form
                   (insert "(")
                   (macrostep-print-sexp head))))

             ;; Print remaining list elements
             (setq sexp (cdr sexp))
             (when sexp (insert " "))
             (while sexp
               (if (listp sexp)
                   (progn
                     (macrostep-print-sexp (car sexp))
                     (when (cdr sexp) (insert " "))
                     (setq sexp (cdr sexp)))
                 ;; Print tail of dotted list
                 (insert ". ")
                 (macrostep-print-sexp sexp)
                 (setq sexp nil)))
             (insert ")")))))

   ;; Print everything except symbols and lists as normal
   (t (prin1 sexp (current-buffer)))))

(defun macrostep-get-gensym-face (symbol)
  "Return the face to use in fontifying SYMBOL in printed macro expansions.

All symbols introduced in the same level of macro expansion are
fontified using the same face (modulo the number of faces; see
`macrostep-gensym-faces')."
  (or (get symbol 'macrostep-gensym-face)
      (progn
        (if (not macrostep-gensyms-this-level)
            (setq macrostep-gensym-depth (1+ macrostep-gensym-depth)
                  macrostep-gensyms-this-level t))
        (let ((face (ring-ref macrostep-gensym-faces macrostep-gensym-depth)))
          (put symbol 'macrostep-gensym-face face)
          face))))


(provide 'macrostep)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; macrostep.el ends here
