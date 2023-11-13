;; -*- lexical-binding: t; -*-
(require 'sly)

(define-sly-contrib sly-retro
  "Enable SLIME to connect to a SLY-started SLYNK"
  (:slynk-dependencies slynk/retro)
  (:on-load (setq sly-net-send-translator #'sly-retro-slynk-to-swank))
  (:on-unload (setq sly-net-send-translator nil)))

(defun sly-retro-slynk-to-swank (sexp)
  (cond ((and sexp
              (symbolp sexp)
              (string-match "^slynk\\(.*\\)$" (symbol-name sexp)))
         (intern (format "swank%s" (match-string 1 (symbol-name sexp)))))
        ((and sexp (listp sexp))
         (cl-loop for (x . rest) on sexp
                  append (list (sly-retro-slynk-to-swank x)) into foo
                  finally (return (append foo (sly-retro-slynk-to-swank rest)))))
        (t
         sexp)))

(provide 'sly-retro)
