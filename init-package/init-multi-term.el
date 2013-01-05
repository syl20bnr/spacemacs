(require 'multi-term)
;; zsh
(setq multi-term-program "/usr/bin/zsh")
;; for solarized dark theme
(custom-set-variables
   '(term-default-bg-color "#002b36")
   '(term-default-fg-color "#93a1a1"))
;; enable evil
(evil-set-initial-state 'term-mode 'emacs)
;; don't switch to other multi-term when closing
;; the current one
(setq multi-term-switch-after-close nil)

;; Following code was take from:
;; http://emacswiki.org/emacs/MultiTerm
(defun last-multi-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (zoo/last-term-buffer (cdr l)))))

(defun last-used-multi-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-multi-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))
