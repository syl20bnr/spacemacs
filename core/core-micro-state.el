;;; -*- lexical-binding: t -*-
;;; core-micro-state.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/defface-micro-state-faces ()
  "Define faces for micro-states."
  (let* ((hname 'space-macs-micro-state-header-face)
         (bname 'space-macs-micro-state-binding-face)
         (box '(:line-width -1 :color (plist-get (face-attribute
                                                  'mode-line :box) :color)))
         (err (face-attribute 'error :foreground)))
    (eval `(defface ,hname '((t ()))
             "Face for micro-state header in echo area.
The header is the name of the micro-state."
             :group 'space-macs))
    (set-face-attribute hname nil
                        :background "DarkGoldenrod2"
                        :foreground "black"
                        :bold t
                        :box box)
    (eval `(defface ,bname '((t ()))
             "Face for micro-state key binding in echo area.
Characters enclosed in `[]' will have this face applied to them."
             :group 'space-macs))
    (set-face-attribute bname nil
                        :foreground err
                        :bold t)))
(space-macs/defface-micro-state-faces)
(add-hook 'space-macs-post-theme-change-hook
          'space-macs/defface-micro-state-faces)

(defun space-macs//micro-state-set-minibuffer-height (str)
  "Set the max mini windows size given a string STR."
  (let ((line-count (1+ (space-macs/how-many-str "\n" str))))
    (when (and (> line-count max-mini-window-height)
               (> line-count 10))
      (setq max-mini-window-height line-count))))

(defmacro space-macs|define-micro-state (name &rest props)
  "Define a micro-state called NAME.

NAME is a symbol.

Available PROPS:

`:on-enter SEXP'
    Evaluate SEXP when the micro-state is switched on.

`:on-exit SEXP'
    Evaluate SEXP when leaving the micro-state.

`:doc STRING or SEXP'
    A STRING or a SEXP that evaluates to a string.

`:use-minibuffer BOOLEAN'
    If non nil then the minibuffer is used to display the documentation
    strings. Default is nil.

`:disable-evil-leader BOOLEAN'
    If non nil then the evil leader has no effect when the micro state
    is active. Default to nil.

`:persistent BOOLEAN'
    If BOOLEAN is non nil then the micro-state never exits. A binding
    with an explicitly set `exit t' property is required. Default is nil.

`:execute-binding-on-enter BOOLEAN'
    If BOOLEAN is non nil then execute the micro-state command bound to
    to the pressed key that started the micro-state.

`:bindings EXPRESSIONS'
    One or several EXPRESSIONS with the form
    (STRING1 SYMBOL1 :doc STRING
                     :pre SEXP
                     :post SEXP
                     :exit SYMBOL)
    where:
    - STRING1 is a key to be bound to the function or key map SYMBOL1.
    - :doc STRING or SEXP is a STRING or an SEXP that evalutes
      to a string
    - :pre is an SEXP evaluated before the bound action
    - :post is an SEXP evaluated after the bound action
    - :exit SYMBOL or SEXP, if non nil then pressing this key will
      leave the micro-state (default is nil).
      Important note: due to inner working of transient-maps in e-macs
      the `:exit' keyword is evaluate *before* the actual execution
      of the bound command.

All properties supported by `space-macs//create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((func (space-macs//micro-state-func-name name))
         (doc (space-macs/mplist-get-values props :doc))
         (persistent (plist-get props :persistent))
         (disable-leader (plist-get props :disable-evil-leader))
         (msg-func (if (plist-get props :use-minibuffer)
                       'message
                     'lv-message))
         (exec-binding (plist-get props :execute-binding-on-enter))
         (on-enter (space-macs/mplist-get-values props :on-enter))
         (on-exit (space-macs/mplist-get-values props :on-exit))
         (bindings (space-macs/mplist-get-values props :bindings))
         (wrappers (space-macs//micro-state-create-wrappers
                    name doc msg-func disable-leader bindings))
         (keymap-body (space-macs//micro-state-fill-map-sexps wrappers))
         (bindkeys (space-macs//create-key-binding-form props func)))
    `(progn (defun ,func ()
              ,(format "%S micro-state." name)
              (interactive)
              ,@on-enter
              ,(when exec-binding
                 (space-macs//micro-state-auto-execute bindings))
              (let ((doc ,@doc))
                (when doc
                  (space-macs//micro-state-set-minibuffer-height doc)
                  (apply ',msg-func (list (space-macs//micro-state-propertize-doc
                                           (format "%S: %s" ',name doc))))))
              (,(if (version< e-macs-version "24.4")
                    'set-temporary-overlay-map
                  'set-transient-map)
               (let ((map (make-sparse-keymap)))
                 ,@keymap-body map) ',(space-macs//micro-state-create-exit-func
                                       name wrappers persistent on-exit)))
            ,@bindkeys)))

(defun space-macs//micro-state-func-name (name)
  "Return the name of the micro-state function."
  (intern (format "space-macs/%S-micro-state" name)))

(defun space-macs//micro-state-auto-execute (bindings)
  "Auto execute the binding corresponding to `this-command-keys'."
  `(let* ((key (substring (this-command-keys)
                          (1- (length (this-command-keys)))))
          (binding (assoc key ',bindings)))
     (when binding
       (call-interactively (cadr binding)))))

(defun space-macs//micro-state-create-wrappers
    (name doc msg-func disable-leader bindings)
  "Return an alist (key wrapper) for each binding in BINDINGS."
  (mapcar (lambda (x) (space-macs//micro-state-create-wrapper
                       name doc msg-func x))
          (append bindings
                  ;; force SPC to quit the micro-state to avoid a edge case
                  ;; with evil-leader
                  (list `(,dotspace-macs-leader-key
                          ,(unless disable-leader 'space-macs-default-map)
                          :exit t)))))

(defun space-macs//micro-state-create-wrapper (name default-doc msg-func binding)
  "Create a wrapper of FUNC and return a tuple (key wrapper BINDING)."
  (let* ((key (car binding))
         (wrapped (cadr binding))
         (binding-doc (space-macs/mplist-get-values binding :doc))
         (binding-pre (space-macs/mplist-get-values binding :pre))
         (binding-post (space-macs/mplist-get-values binding :post))
         (wrapper-name (intern (format "space-macs//%S-%S-%s" name wrapped key)))
         (doc-body
          `((let ((bdoc ,@binding-doc)
                  (defdoc ,@default-doc))
              (cond
               (bdoc
                (apply ',msg-func
                       (list (space-macs//micro-state-propertize-doc
                              (format "%S: %s" ',name bdoc))))
                bdoc)
               ((and defdoc
                     ',wrapped
                     (not (plist-get ',binding :exit)))
                (space-macs//micro-state-set-minibuffer-height defdoc)
                (apply ',msg-func
                       (list (space-macs//micro-state-propertize-doc
                              (format "%S: %s" ',name defdoc))))
                defdoc)))))
         (wrapper-func
          (if (and (boundp wrapped)
                   (eval `(keymapp ,wrapped)))
              wrapped
            `(defun ,wrapper-name ()
               "Auto-generated function"
               (interactive)
               ,@binding-pre
               (let ((throwp t))
                 (catch 'exit
                   (when (fboundp ',wrapped)
                     (setq this-command ',wrapped)
                     (call-interactively ',wrapped)
                     (setq last-command ',wrapped))
                   (setq throwp nil))
                 ,@binding-post
                 (when throwp (throw 'exit nil)))
               (when ,@doc-body
                 (space-macs//micro-state-set-minibuffer-height ,@doc-body)
                 ,@doc-body)))))
    (append (list (car binding) (eval wrapper-func)) binding)))

(defun space-macs//micro-state-fill-map-sexps (wrappers)
  "Return a list of `define-key' sexp to fill the micro-state temporary map."
  (mapcar (lambda (x) `(define-key map ,(kbd (car x)) ',(cadr x)))
          wrappers))

(defun space-macs//micro-state-create-exit-func
    (name wrappers persistent on-exit)
  "Return a function to execute when leaving the micro-state.

The returned function returns nil if the executed command exits the
micro-state."
  (let ((func (intern (format "space-macs//%s-on-exit" name))))
    (eval `(defun ,func ()
             "Function executed after each micro-state command."
             (let* ((cur-wrapper (space-macs//get-current-wrapper
                                  ',name ',wrappers))
                    (exitp (if cur-wrapper (plist-get cur-wrapper :exit)
                             ,(not persistent))))
               (when (listp exitp) (setq exitp (eval exitp)))
               (when exitp ,@on-exit (space-macs//micro-state-close-window))
               (not exitp))))))

(defun space-macs//get-current-wrapper (name wrappers)
  "Return the wrapper being executed.
Return nil if no wrapper is being executed (i.e. an unbound key has been
pressed)."
  (let ((micro-state-fun (space-macs//micro-state-func-name name)))
    (catch 'found
      (dolist (wrapper wrappers)
        (let ((key (car wrapper))
              (func (cadr wrapper)))
          (if (and (or (eq this-command micro-state-fun)
                       (eq this-command func))
                   (equal (this-command-keys) (kbd key)))
              (throw 'found wrapper))))
      nil)))

(defun space-macs//micro-state-propertize-doc (doc)
  "Return a propertized doc string from DOC."
  (when (string-match "^\\(.+?\\):\\([[:ascii:]]*\\)$" doc)
    (let* ((header (match-string 1 doc))
           (pheader (when header
                      (propertize (concat " " header " ")
                                  'face 'space-macs-micro-state-header-face)))
           (tail (space-macs//micro-state-propertize-doc-rec
                  (match-string 2 doc))))
      (concat pheader tail))))

(defun space-macs//micro-state-propertize-doc-rec (doc)
  "Recursively propertize keys"
  (if (string-match "^\\([[:ascii:]]*?\\)\\(\\[.+?\\]\\)\\([[:ascii:]]*\\)$" doc)
      (let* ((head (match-string 1 doc))
             (key (match-string 2 doc))
             (pkey (when key
                     (propertize key 'face 'space-macs-micro-state-binding-face)))
             (tail (space-macs//micro-state-propertize-doc-rec
                    (match-string 3 doc))))
        (concat head pkey tail))
    doc))

(defun space-macs//micro-state-close-window ()
  "Close micro-state help window."
  (when (window-live-p lv-wnd)
    (let ((buf (window-buffer lv-wnd)))
      (delete-window lv-wnd)
      (kill-buffer buf))))

(provide 'core-micro-state)


