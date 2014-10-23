(defvar trishume-packages
  '(
    auctex
    cdlatex
    smooth-scrolling
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

(defun trishume/init-smooth-scrolling ()
  (use-package smooth-scrolling
    :init
    (setq scroll-margin 5
          scroll-conservatively 9999
          scroll-step 1)))
