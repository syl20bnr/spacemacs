(require 'autopair)

(defvar autopair-modes
  '(python-mode
    emacs-lisp-mode
    erlang-mode
    ruby-mode))
(defun turn-on-autopair-mode () (autopair-mode 1))
(dolist (mode autopair-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))
