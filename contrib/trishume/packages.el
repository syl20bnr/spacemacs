(defvar trishume-packages
  '(
    auctex
    cdlatex
    ))

(defun trishume/init-auctex ()
  (use-package tex
    :defer t
    :config
    (progn
      (setq-default TeX-auto-save t)
      (setq-default TeX-parse-self t)
      (setq-default TeX-master nil)
      (setq-default TeX-PDF-mode t))))
