;;; funcs.el --- Space-macs Bootstrap Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3




(defun space-macs/state-color-face (state)
  "Return the symbol of the face for the given STATE."
  (intern (format "space-macs-%s-face" (symbol-name state))))

(defun space-macs/state-color (state)
  "Return the color string associated to STATE."
  (face-background (space-macs/state-color-face state)))

(defun space-macs/current-state-color ()
  "Return the color string associated to the current state."
  (face-background (space-macs/state-color-face evil-state)))

(defun space-macs/state-face (state)
  "Return the face associated to the STATE."
  (space-macs/state-color-face state))

(defun space-macs/current-state-face ()
  "Return the face associated to the current state."
  (let ((state (if (eq evil-state 'operator)
                    evil-previous-state
                  evil-state)))
    (space-macs/state-color-face state)))

(defun space-macs/add-evil-cursor (state color shape)
  "Define a cursor and face for a new evil state.
An appropriate entry is added to `space-macs-evil-cursors', as well.

For evil states that do not need an evil cursor use
`space-macs/define-evil-state-face' instead."
  (add-to-list 'space-macs-evil-cursors (list state color shape))
  (space-macs/define-evil-state-face state color)
  (set (intern (format "evil-%s-state-cursor" state))
       (list (when dotspace-macs-colorize-cursor-according-to-state color)
             shape)))

(defun space-macs/define-evil-state-face (state color)
  "Define a face for an evil state.
For evil states that also need an entry to `space-macs-evil-cursors' use
`space-macs/add-evil-cursor' instead."
  ;; this function and `space-macs/add-evil-cursor' need to be separate because
  ;; some states must explicitly *not* have their own evil space-macs cursor
  ;; for example tree-macs: it needs no cursor since it solely uses hl-line-mode
  ;; and having an evil cursor defined anyway leads to the cursor sometimes
  ;; visibly flashing in tree-macs buffers
  (eval `(defface ,(intern (format "space-macs-%s-face" state))
           `((t (:background ,color
                             :foreground ,(face-background 'mode-line)
                             :inherit 'mode-line)))
           (format "%s state face." state)
           :group 'space-macs)))

(defun space-macs/set-state-faces ()
  (cl-loop for (state color cursor) in space-macs-evil-cursors
           do
           (set-face-attribute (intern (format "space-macs-%s-face" state))
                               nil
                               :foreground (face-background 'mode-line))))

(defun evil-insert-state-cursor-hide ()
  (setq evil-insert-state-cursor '((hbar . 0))))

(defun space-macs/set-evil-search-module (style)
  "Set the evil search module depending on STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             (bound-and-true-p hybrid-style-use-evil-search-module)))
    ;; if Evil is loaded already, just setting `evil-search-module' isn't
    ;; enough, we need to call `evil-select-search-module' as well (this is done
    ;; automatically when `evil-search-module' is changed via customize)
    (if (featurep 'evil-search)
        (evil-select-search-module 'evil-search-module 'evil-search)
      (setq-default evil-search-module 'evil-search)))
   (t
    (if (featurep 'evil-search)
        (evil-select-search-module 'evil-search-module 'isearch)
      (setq-default evil-search-module 'isearch)))))

(defun space-macs/evil-search-clear-highlight ()
  "Clear evil-search or evil-ex-search persistent highlights."
  (interactive)
  (evil-ex-nohighlight)) ; `/' highlights

(defun space-macs/evil-smart-doc-lookup ()
  "Run documentation lookup command specific to the major mode.
Use command bound to `SPC m h h` if defined, otherwise fall back
to `evil-lookup'"
  (interactive)
  (let ((binding (key-binding (kbd (concat dotspace-macs-leader-key " mhh")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-lookup))))

(defun space-macs//set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the
current major mode."
  (let ((shift-width
         (catch 'break
           (dolist (test space-macs--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

(defmacro space-macs|define-text-object (key name start end)
  "Define a text object and a surround pair.
START and END are strings (not regular expressions) that define
the boundaries of the text object."
  `(progn
     (space-macs|define-text-object-regexp ,key ,name
                                          ,(regexp-quote start)
                                          ,(regexp-quote end))
     (with-eval-after-load 'evil-surround
       (add-to-list 'evil-surround-pairs-alist
                    (cons (string-to-char ,key)
                          (if ,end
                              (cons ,start ,end)
                            ,start))))))

(defmacro space-macs|define-text-object-regexp (key name start-regexp end-regexp)
  "Define a text object.
START-REGEXP and END-REGEXP are the boundaries of the text object."
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; need to delay this macro since it relies on evil key maps to be defined
(with-eval-after-load 'evil
  (defmacro evil-map (state key seq)
    "Map for a given STATE a KEY to a sequence SEQ of keys.

Can handle recursive definition only if KEY is the first key of SEQ.
Example: (evil-map visual \"<\" \"<gv\")"
    (let ((map (intern (format "evil-%S-state-map" state))))
      `(define-key ,map ,key
         (lambda ()
           (interactive)
           ,(if (string-equal key (substring seq 0 1))
                `(progn
                   (call-interactively ',(lookup-key evil-normal-state-map key))
                   (execute-kbd-macro ,(substring seq 1)))
              (execute-kbd-macro ,seq)))))))

(defun space-macs/diminish-hook (_)
  "Display diminished lighter in vanilla e-macs mode-line."
  (let ((unicodep (dotspace-macs|symbol-value
                   dotspace-macs-mode-line-unicode-symbols)))
    (cl-loop for (mode uni nouni) in space-macs--diminished-minor-modes
             do (diminish mode (if unicodep uni nouni)))))



(defun space-macs//hydra-key-doc-function (key key-width doc doc-width)
  "Custom hint documentation format for keys."
  (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
          key doc))



(defvar space-macs--scroll-ts-full-hint-toggle t
  "Toggle the state of the scroll transient state documentation.

Initial value is t so full hint will be shown by default. This is
to preserve the behavior before hint toggle was implemented for
the scroll transient state.")

(defvar space-macs--scroll-ts-full-hint nil
  "Display full scroll transient state documentation.")

(defun space-macs//scroll-ts-toggle-hint ()
  "Toggle the full hint docstring for the scroll transient state."
  (interactive)
  (setq space-macs--scroll-ts-full-hint-toggle
        (not space-macs--scroll-ts-full-hint-toggle)))

(defun space-macs//scroll-ts-hint ()
  "Return a condensed/full hint for the scroll transient state"
  (concat
   " "
   (if space-macs--scroll-ts-full-hint-toggle
       space-macs--scroll-ts-full-hint
     (concat "[" (propertize "?" 'face 'hydra-face-red) "] toggle help"))))



(defun use-package-normalize/:spacebind (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (if (and (listp arg) (keywordp (car arg)))
          arg
        (use-package-error
         ":spacebind wants an arg list compatible with `spacebind' macro")))))

(defun use-package-handler/:spacebind (name-symbol keyword args rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (if (null args)
        body
      (use-package-concat
       body
       `((space-macs|spacebind ,@args))))))



;; As suggested in the e-macs wiki https://www.e-macswiki.org/e-macs/HideShow#toc5
(defun space-macs/toggle-selective-display (column)
  "Toggle selective display by column, a.k.a. folding by indentation.

Invokes `set-selective-display', but if a prefix argument is not supplied and
`selective-display' is not already true, source the prefix argument from the
column."
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))


