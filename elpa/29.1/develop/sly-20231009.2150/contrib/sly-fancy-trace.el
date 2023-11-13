;; -*- lexical-binding: t; -*-
(require 'sly)
(require 'sly-parse "lib/sly-parse")

(define-sly-contrib sly-fancy-trace
  "Enhanced version of sly-trace capable of tracing local functions,
methods, setf functions, and other entities supported by specific
slynk:slynk-toggle-trace backends. Invoke via C-u C-t."
  (:authors "Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>"
            "Tobias C. Rittweiler <tcr@freebits.de>")
  (:license "GPL"))

(defun sly-trace-query (spec)
  "Ask the user which function to trace; SPEC is the default.
The result is a string."
  (cond ((null spec)
         (sly-read-from-minibuffer "(Un)trace: "))
        ((stringp spec)
         (sly-read-from-minibuffer "(Un)trace: " spec))
        ((symbolp spec)    ; `sly-extract-context' can return symbols.
         (sly-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
        (t
         (sly-dcase spec
           ((setf n)
            (sly-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:defun n)
            (sly-read-from-minibuffer "(Un)trace: " (prin1-to-string n)))
           ((:defgeneric n)
            (let* ((name (prin1-to-string n))
                   (answer (sly-read-from-minibuffer "(Un)trace: " name)))
              (cond ((and (string= name answer)
                          (y-or-n-p (concat "(Un)trace also all "
                                            "methods implementing "
                                            name "? ")))
                     (prin1-to-string `(:defgeneric ,n)))
                    (t
                     answer))))
           ((:defmethod &rest _)
            (sly-read-from-minibuffer "(Un)trace: " (prin1-to-string spec)))
           ((:call caller callee)
            (let* ((callerstr (prin1-to-string caller))
                   (calleestr (prin1-to-string callee))
                   (answer (sly-read-from-minibuffer "(Un)trace: "
                                                       calleestr)))
              (cond ((and (string= calleestr answer)
                          (y-or-n-p (concat "(Un)trace only when " calleestr
                                            " is called by " callerstr "? ")))
                     (prin1-to-string `(:call ,caller ,callee)))
                    (t
                     answer))))
           (((:labels :flet) &rest _)
            (sly-read-from-minibuffer "(Un)trace local function: "
                                        (prin1-to-string spec)))
           (t (error "Don't know how to trace the spec %S" spec))))))

(defun sly-toggle-fancy-trace (&optional using-context-p)
  "Toggle trace."
  (interactive "P")
  (let* ((spec (if using-context-p
                   (sly-extract-context)
                   (sly-symbol-at-point)))
         (spec (sly-trace-query spec)))
    (sly-message "%s" (sly-eval `(slynk:slynk-toggle-trace ,spec)))))

;; override sly-toggle-trace-fdefinition
(define-key sly-prefix-map "\C-t" 'sly-toggle-fancy-trace)

(provide 'sly-fancy-trace)
