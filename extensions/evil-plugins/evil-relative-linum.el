(require 'linum+) ;; See http://github.com/tarao/elisp/raw/master/linum+.el
(require 'evil)

(defgroup evil-relative-linum nil
  "Relative line numbers when operators are activated."
  :prefix "evil-relative-linum-"
  :group 'evil)

(defcustom evil-relative-linum-delay 0.3
  "Delay in showing line numbers after the operator is activated."
  :group 'evil-relative-linum
  :type 'float)

(defvar evil-relative-linum-timer nil)
(defvar evil-relative-linum-activated nil)

(define-minor-mode evil-relative-linum-mode
  "Show relative line numbers when operators are activated."
  :group 'evil-relative-linum
  (let ((exit-cmd `(lambda ()
                     (interactive)
                     (save-excursion (set-buffer ,(current-buffer))
                                     (evil-relative-linum-off)))))
    (if evil-relative-linum-mode
        (progn
          (add-hook 'pre-command-hook exit-cmd)
          (add-hook 'post-command-hook exit-cmd)
          (setq evil-relative-linum-timer
                (run-with-idle-timer evil-relative-linum-delay nil
                                     'evil-relative-linum-activate)))
      (cancel-timer evil-relative-linum-timer)
      (when evil-relative-linum-activated
        (relative-linum-mode 0)
        (setq evil-relative-linum-activated nil))
      (remove-hook 'pre-command-hook exit-cmd)
      (remove-hook 'post-command-hook exit-cmd))))

(defun evil-relative-linum-off ()
  (interactive)
  (evil-relative-linum-mode 0))

(defun evil-relative-linum-on ()
  (interactive)
  (evil-relative-linum-mode 1))

(defun evil-relative-linum-activate ()
  (setq evil-relative-linum-activated t)
  (relative-linum-mode 1))

(add-hook 'evil-operator-state-entry-hook 'evil-relative-linum-on)

(provide 'evil-relative-linum)
