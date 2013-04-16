(require 'fill-column-indicator)

(defun turn-on-fill-column-indicator ()
  (fci-mode 1)
  (setq fci-rule-column 80)
  (setq fci-rule-width 2))

(let ((supported-modes
       '(
         clojure-mode-hook
         elixir-mode-hook
         emacs-lisp-mode-hook
         erlang-mode-hook
         java-mode-hook
         javascript-mode-hook
         lisp-mode-hook
         org-mode-hook
         python-mode-hook
         )))
  (dolist (hook supported-modes)
    (add-hook hook 'turn-on-fill-column-indicator)))
