;;; evil-cleverparens-text-objects.el --- Text Objects for evil-cleverparens.
;;
;; Copyright (C) 2015 Olli Piepponen
;;
;; Author: Olli Piepponen <opieppo@gmail.com>
;; URL: https://github.com/emacs-evil/evil-cleverparens
;; Keywords: convenience, emulations
;; Version: 0.1.0
;; Package-Requires: ((evil "1.0") (paredit "1") (smartparens "1.6.1") (emacs "24.4") (dash "2.12.0"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software (MIT License)

;;; Commentary:

;; Text objects for forms, comments and defuns, used in evil-cleverparens, but
;; potentially useful in non-lisp-like modes as well. In such a case,
;; key-bindings are left open for the user to decide.

;;; Code:

(require 'evil-common)
(require 'smartparens)
(require 'evil-cleverparens-util)


;;;###autoload (autoload 'evil-cp-a-WORD "evil-cleverparens-text-objects" nil t)
(evil-define-text-object evil-cp-a-WORD (count &optional beg end type)
  "Select a symbol"
  (evil-select-an-object (if evil-cleverparens-swap-move-by-word-and-symbol
                             'evil-word 'evil-symbol)
                         beg end type count))

;;;###autoload (autoload 'evil-cp-inner-WORD "evil-cleverparens-text-objects" nil t)
(evil-define-text-object evil-cp-inner-WORD (count &optional beg end type)
  "Select inner symbol"
  (evil-select-inner-object (if evil-cleverparens-swap-move-by-word-and-symbol
                                'evil-word 'evil-symbol)
                            beg end type count))

;;;###autoload (autoload 'evil-cp-a-form "evil-cleverparens-text-objects" nil t)
(evil-define-text-object evil-cp-a-form (count &optional beg end type)
  "Smartparens sexp object."
  (let* ((bounds (evil-cp--get-enclosing-bounds t)))
    (if (not bounds)
        (error "No surrounding form found.")
      ;; I'm not sure what difference 'inclusive / 'exclusive makes here
      (evil-range (car bounds) (cdr bounds) 'inclusive :expanded t))))

;;;###autoload (autoload 'evil-cp-inner-form "evil-cleverparens-text-objects" nil t)
(evil-define-text-object evil-cp-inner-form (count &optional beg end type)
  "Smartparens inner sexp object."
  (let ((range (evil-cp--get-enclosing-bounds)))
    (if (not range)
        (error "No surrounding form found.")
      (let ((beg (car range))
            (end (cdr range)))
        (evil-range (1+ beg) (1- end) 'inclusive :expanded t)))))

;;;###autoload (autoload 'evil-cp-a-comment "evil-cleverparens-text-objects" nil t)
(evil-define-text-object evil-cp-a-comment (count &optional beg end type)
  "An outer comment text object as defined by `sp-get-comment-bounds'."
  (let ((bounds (sp-get-comment-bounds)))
    (if (not bounds)
        (error "Not inside a comment.")
      (let ((beg (car bounds))
            (end (cdr bounds)))
        (evil-range beg end 'exclusive :expanded t)))))

;;;###autoload (autoload 'evil-cp-inner-comment "evil-cleverparens-text-objects" nil t)
(evil-define-text-object evil-cp-inner-comment (count &optional beg end type)
  "An inner comment text object as defined by `sp-get-comment-bounds'."
  (let ((bounds (sp-get-comment-bounds)))
    (if (not bounds)
        (error "Not inside a comment.")
      (let ((beg (save-excursion
                   (goto-char (car bounds))
                   (forward-word 1)
                   (forward-word -1)
                   (point)))
            (end (save-excursion
                   (goto-char (cdr bounds))
                   (evil-end-of-line)
                   (point))))
        (evil-range beg end 'block :expanded t)))))

;;;###autoload (autoload 'evil-cp-a-defun "evil-cleverparens-text-objects" nil t)
(evil-define-text-object evil-cp-a-defun (count &optional beg end type)
  "An outer text object for a top level sexp (defun)."
  (if (evil-cp--inside-form-p)
      (let ((bounds (evil-cp--top-level-bounds)))
        (evil-range (car bounds) (cdr bounds) 'inclusive :expanded t))
    (error "Not inside a sexp.")))

;;;###autoload (autoload 'evil-cp-inner-defun "evil-cleverparens-text-objects" nil t)
(evil-define-text-object evil-cp-inner-defun (count &optional beg end type)
  "An inner text object for a top level sexp (defun)."
  (if (evil-cp--inside-form-p)
      (let ((bounds (evil-cp--top-level-bounds)))
        (evil-range (1+ (car bounds)) (1- (cdr bounds)) 'inclusive :expanded t))
    (error "Not inside a sexp.")))

(provide 'evil-cleverparens-text-objects)
;;; evil-cleverparens-text-objects.el ends here
