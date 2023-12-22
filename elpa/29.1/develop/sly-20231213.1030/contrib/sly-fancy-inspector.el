;; -*- lexical-binding: t; -*-
(require 'sly)
(require 'sly-parse "lib/sly-parse")

(define-sly-contrib sly-fancy-inspector
  "Fancy inspector for CLOS objects."
  (:authors "Marco Baringer <mb@bese.it> and others")
  (:license "GPL")
  (:slynk-dependencies slynk/fancy-inspector))

(defun sly-inspect-definition ()
  "Inspect definition at point"
  (interactive)
  (sly-inspect (sly-definition-at-point)))

(defun sly-disassemble-definition ()
  "Disassemble definition at point"
  (interactive)
  (sly-eval-describe `(slynk:disassemble-form
                         ,(sly-definition-at-point t))))

(provide 'sly-fancy-inspector)
