(setq fci-rule-width 1)
(setq fci-enabled 0)

(defun toggle-fill-column-indicator ()
  (interactive)
  (make-local-variable 'fci-enabled)
  (if (> fci-enabled 0) (deactivate-fci) (activate-fci)))

(defun activate-fci ()
  (setq fci-rule-column 79)
  (setq fci-enabled 1)
  (fci-mode 1))

(defun deactivate-fci ()
  (setq fci-enabled 0)
  (fci-mode 0))

(use-package fill-column-indicator
  :commands toggle-fill-column-indicator)

