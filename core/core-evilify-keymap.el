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

;; (defvar spacemacs--evilified-state-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "/" 'evil-search-forward)
;;     (define-key map ":" 'evil-ex)
;;     (define-key map "h" 'evil-backward-char)
;;     (define-key map "j" 'evil-next-visual-line)
;;     (define-key map "k" 'evil-previous-visual-line)
;;     (define-key map "l" 'evil-forward-char)
;;     (define-key map "n" 'evil-search-next)
;;     (define-key map "N" 'evil-search-previous)
;;     (define-key map "v" 'evil-visual-char)
;;     (define-key map "V" 'evil-visual-line)
;;     (define-key map "y" 'evil-yank)
;;     map)
;;   "Evilifed state keymap.")

(defun spacemacs/evilify-map (map &optional mode)
  "Evilify the keymap MAP."
  (let* ((evilified-keys (mapcar 'car (cdr evil-evilified-state-map))))
    (map-keymap (lambda (event value)
                  (when (member event evilified-keys)
                    (spacemacs//evilify-remap-binding map event value)))
                (symbol-value map)))
  ;; keep a list of all evilified modes
  (when mode
    (add-to-list 'evil-evilified-state--modes mode)
    (unless (bound-and-true-p holy-mode)
      (delq mode evil-emacs-state-modes)
      (add-to-list 'evil-evilified-state-modes mode))))

(defun spacemacs//evilify-remap-binding (map event value)
  "Remap VALUE binding in MAP."
  (when (and (characterp event)
             ;; do not remap lambda wrapper
             (not (eq 'lambda (when (listp value) (car value))))
             ;; do not remap already remapped bindings
             (not (string-match "spacemacs" (if (keymapp value)
                                                ""
                                              (symbol-name value)))))
    (let ((new-event (spacemacs//evilify-next-event (symbol-value map) event))
          (wrapper (spacemacs//evilify-make-wrapper map event value)))
      (if (null new-event)
          (message "Warning: Could not rebind event \"%s\" (map %S)"
                   (char-to-string event) map)
        (when (assoc new-event (cdr (symbol-value map)))
          ;; new-event is already bound in MAP so we process it before
          ;; for instance if MAP has 'k' and 'K', then we move 'K' first
          ;; to 'C-k' and we will be able to move 'k' on 'K'.
          (message "new event: %s" new-event)
          (spacemacs//evilify-remap-binding
           map new-event (lookup-key (symbol-value map)
                                     (kbd (char-to-string new-event)))))
        ;; remap event
        (if (keymapp value)
            (progn
              (eval `(define-key ,map ,(char-to-string event) ',wrapper)))
          (eval `(define-key ,map [remap ,value] ',wrapper)))
        ;; move original command or keymap on a new event
        (if new-event
            (if (keymapp value)
                (eval `(define-key ,map ,(char-to-string new-event) ',value))
              (eval `(define-key ,map ,(char-to-string new-event)
                       (lambda ()
                         (interactive)
                         (call-interactively ',value))))))))))

(defun spacemacs//evilify-wrapper-name (map event value)
  "Return the name of the wrapper function."
  (intern (format "spacemacs/evilified-%s"
                  (if (keymapp value)
                      (format "%s-keymap-%s"
                              (symbol-name map)
                              (char-to-string event))
                    value))))

(defun spacemacs//evilify-make-wrapper (map event value)
  "Wrap VALUE in a function which takes care of the evilified state."
  (let ((wrapper-func (spacemacs//evilify-wrapper-name map event value)))
    (eval `(defun ,wrapper-func ()
             ,(format "Wrap %s to support evilified state."
                      (if (keymapp value)
                          "keymap"
                        (format "command %s" value)))
             (interactive)
             (if (eq 'evilified evil-state)
                 (call-interactively ',(lookup-key evil-evilified-state-map
                                                   (char-to-string event)))
               (if ,(keymapp value)
                   (progn
                     (message "%s-" ,(char-to-string event))
                     (,(if (version< emacs-version "24.4")
                           'set-temporary-overlay-map
                         'set-transient-map)
                      ',value))
                 (call-interactively ',value)))))
    wrapper-func))

(defun spacemacs//evilify-next-event (map event)
  "Return a new event for the evilified EVENT in MAP."
  (cond
   ;; space
   ((char-equal event 32)
    (message "unsupported for now") nil)
   ((char-equal event ?/)
    (message "unsupported for now") nil)
   ((char-equal event ?:)
    (message "unsupported for now") nil)
   ((member event (mapcar (lambda (x)
                            (when (listp x) (car x))) (cdr map)))
    (cond
     ((and (<= ?a event) (<= event ?z)) (- event 32))
     ((and (<= ?A event) (<= event ?Z)) (- event 64))
     ((and (<= 1 event) (<= event 26)) (+ (expt 2 25) event))
     (t (message "No next event left.") nil)))))

(provide 'core-evilify-keymap)
