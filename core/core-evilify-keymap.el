;;; core-evilify-keymap.el --- Spacemacs Core File
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
     ;; space
     ((equal event 32) nil)
     ((equal event ?/) nil)
     ((equal event ?:) nil)
     ;; C-g (cannot remap C-g)
     ((equal event ?\a) nil)
     ((and (<= ?a event) (<= event ?z)) (- event 32))
     ;; don't shadow C-g, G is mapped directly to C-S-g
     ((equal event ?G) (+ (expt 2 25) ?\a))
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

(provide 'core-evilify-keymap)
