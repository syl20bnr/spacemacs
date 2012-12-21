(require 'fill-column-indicator)

(defun turn-on-fill-column-indicator ()
  (fci-mode 1)
  (setq fci-rule-column 80)
  (setq fci-rule-width 2)
  (setq fci-rule-color "#073642"))

(let ((supported-modes '(emacs-lisp-mode-hook
                         clojure-mode-hook
                         javascript-mode-hook
                         lisp-mode-hook
                         python-mode-hook
                         erlang-mode-hook)))
  (dolist (hook supported-modes)
    (add-hook hook 'turn-on-fill-column-indicator)))

