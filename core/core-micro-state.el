;;; -*- lexical-binding: t -*-
;;; core-micro-state.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'lv)

(defun spacemacs//defface-micro-state-faces ()
  "Define faces for micro-states."
  (let* ((hname 'spacemacs-micro-state-header-face)
         (bname 'spacemacs-micro-state-binding-face)
         (box '(:line-width -1 :color (plist-get (face-attribute
                                                  'mode-line :box) :color)))
         (err (face-attribute 'error :foreground)))
    (eval `(defface ,hname '((t ()))
             "Face for micro-state header in echo area.
The header is the name of the micro-state."
             :group 'spacemacs))
    (set-face-attribute hname nil
                        :background "DarkGoldenrod2"
                        :foreground "black"
                        :bold t
                        :box box)
    (eval `(defface ,bname '((t ()))
             "Face for micro-state key binding in echo area.
Characters enclosed in `[]' will have this face applied to them."
             :group 'spacemacs))
    (set-face-attribute bname nil
                        :foreground err
                        :bold t)))
(spacemacs//defface-micro-state-faces)

(defmacro spacemacs|define-micro-state (name &rest props)
  "Define a micro-state called NAME.

NAME is a symbol.

Available PROPS:

`:on-enter SEXP'
    Evaluate SEXP when the micro-state is switched on.

`:on-exit SEXP'
    Evaluate SEXP when leaving the micro-state.

`:doc STRING or SEXP'
    A STRING or a SEXP that evaluates to a string

`:persistent BOOLEAN'
    If BOOLEAN in non nil then the micro-state never exits. A binding
    with an explicitly set `exit t' property is required.

`:bindings EXPRESSIONS'
    One or several EXPRESSIONS with the form
    (STRING1 SYMBOL1 :doc STRING
                     :pre SEXP
                     :post SEXP
                     :exit SYMBOL)
    where:
    - STRING1 is a key to bound to the function SYMBOL1.
    - :doc STRING or SEXP is a STRING or an SEXP that evalutes
      to a string
    - :pre is an SEXP evaluated before the bound action
    - :post is an SEXP evaluated after the bound action
    - :exit SYMBOL is either `:exit t' or `:exit nil', if non nil then
      pressing this key will leave the micro-state (default is nil).

All properties supported by `spacemacs//create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((func (spacemacs//micro-state-func-name name))
         (doc (spacemacs/mplist-get props :doc))
         (persistent (plist-get props :persistent))
         (exec-binding (plist-get props :execute-binding-on-enter))
         (on-enter (spacemacs/mplist-get props :on-enter))
         (on-exit (spacemacs/mplist-get props :on-exit))
         (bindings (spacemacs/mplist-get props :bindings))
         (wrappers (spacemacs//micro-state-create-wrappers name doc bindings))
         (keymap-body (spacemacs//micro-state-fill-map-sexps wrappers))
         (bindkeys (spacemacs//create-key-binding-form props func)))
    `(progn (defun ,func ()
              ,(format "%S micro-state." name)
              (interactive)
              (let ((doc ,@doc))
                (when doc
                  (lv-message (spacemacs//micro-state-propertize-doc
                               (format "%S: %s" ',name doc)))))
              ,(when exec-binding
                 (spacemacs//micro-state-auto-execute bindings))
              ,@on-enter
              (,(if (version< emacs-version "24.4")
                    'set-temporary-overlay-map
                  'set-transient-map)
               (let ((map (make-sparse-keymap)))
                 ,@keymap-body map) ',(spacemacs//micro-state-create-exit-func
                                       name wrappers persistent on-exit)))
            ,@bindkeys)))

(defun spacemacs//micro-state-func-name (name)
  "Return the name of the micro-state function."
  (intern (format "spacemacs/%S-micro-state" name)))

(defun spacemacs//micro-state-auto-execute (bindings)
  "Auto execute the binding corresponding to `this-command-keys'."
  `(let* ((key (substring (this-command-keys)
                          (1- (length (this-command-keys)))))
          (binding (assoc key ',bindings)))
     (when binding
       (call-interactively (cadr binding)))))

(defun spacemacs//micro-state-create-wrappers (name doc bindings)
  "Return an alist (key wrapper) for each binding in BINDINGS."
  (mapcar (lambda (x) (spacemacs//micro-state-create-wrapper name doc x))
          (append bindings
                  ;; force SPC to quit the micro-state to avoid a edge case
                  ;; with evil-leader
                  (list '("SPC" nil :exit t)))))

(defun spacemacs//micro-state-create-wrapper (name default-doc binding)
  "Create a wrapper of FUNC and return a tuple (key wrapper BINDING)."
  (let* ((key (car binding))
         (wrapped (cadr binding))
         (binding-doc (spacemacs/mplist-get binding :doc))
         (binding-pre (spacemacs/mplist-get binding :pre))
         (binding-post (spacemacs/mplist-get binding :post))
         (wrapper-name (intern (format "spacemacs//%S-%S-%s" name wrapped key)))
         (doc-body `((let ((bdoc ,@binding-doc)
                           (defdoc ,@default-doc))
                       (if bdoc
                           (lv-message (spacemacs//micro-state-propertize-doc
                                        (format "%S: %s" ',name bdoc)))
                         (when (and defdoc
                                    ',wrapped (not (plist-get ',binding :exit)))
                           (lv-message (spacemacs//micro-state-propertize-doc
                                        (format "%S: %s" ',name defdoc))))))))
         (wrapper-func
          (eval `(defun ,wrapper-name ()
                   "Auto-generated function"
                   (interactive)
                   ,@binding-pre
                   (let ((throwp t))
                     (catch 'exit
                       (when ',wrapped
                         (call-interactively ',wrapped))
                       (setq throwp nil))
                     ,@binding-post
                     (when throwp (throw 'exit nil)))
                   ,@doc-body
                   ))))
    (append (list (car binding) wrapper-func) binding)))

(defun spacemacs//micro-state-fill-map-sexps (wrappers)
  "Return a list of `define-key' sexp to fill the micro-state temporary map."
  (mapcar (lambda (x) `(define-key map ,(kbd (car x)) ',(cadr x)))
          wrappers))

(defun spacemacs//micro-state-create-exit-func
    (name wrappers persistent on-exit)
  "Return a function to execute when leaving the micro-state.

The returned function returns nil if the executed command exits the
micro-state."
  (let ((func (intern (format "spacemacs//%s-on-exit" name))))
    (eval `(defun ,func ()
             "Function executed after each micro-state command."
             (let* ((cur-wrapper (spacemacs//get-current-wrapper
                                  ',name ',wrappers))
                    (exitp (if cur-wrapper (plist-get cur-wrapper :exit)
                             ,(not persistent))))
               (when exitp ,@on-exit (spacemacs//micro-state-close-window))
               (not exitp))))))

(defun spacemacs//get-current-wrapper (name wrappers)
  "Return the wrapper being executed.
Returns nil if no wrapper is being executed (i.e. an unbound key has been
pressed)."
  (let ((micro-state-fun (spacemacs//micro-state-func-name name)))
    (catch 'found
      (dolist (wrapper wrappers)
        (let ((key (car wrapper))
              (func (cadr wrapper)))
          (if (and (or (eq this-command micro-state-fun)
                       (eq this-command func))
                   (equal (this-command-keys) (kbd key)))
              (throw 'found wrapper))))
      nil)))

(defun spacemacs//micro-state-propertize-doc (doc)
  "Return a propertized doc string from DOC."
  (when (string-match "^\\(.+?\\):\\([[:ascii:]]*\\)$" doc)
    (let* ((header (match-string 1 doc))
           (pheader (when header
                      (propertize (concat " " header " ")
                                  'face 'spacemacs-micro-state-header-face)))
           (tail (spacemacs//micro-state-propertize-doc-1
                  (match-string 2 doc))))
      (concat pheader tail))))

(defun spacemacs//micro-state-propertize-doc-1 (doc)
  "Recursively propertize keys"
  (if (string-match "^\\([[:ascii:]]*?\\)\\(\\[.+?\\]\\)\\([[:ascii:]]*\\)$" doc)
      (let* ((head (match-string 1 doc))
             (key (match-string 2 doc))
             (pkey (when key
                     (propertize key 'face 'spacemacs-micro-state-binding-face)))
             (tail (spacemacs//micro-state-propertize-doc-1
                    (match-string 3 doc))))
        (concat head pkey tail))
    doc))

(defun spacemacs//micro-state-close-window ()
  "Close micro-state help window."
  (when (window-live-p lv-wnd)
    (let ((buf (window-buffer lv-wnd)))
      (delete-window lv-wnd)
      (kill-buffer buf))))

(provide 'core-micro-state)
