(defvar trishume-packages
  '(
    auctex
    cdlatex
    smooth-scrolling
    helm-ag
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

(defun trishume/init-helm-ag ()
  (use-package helm-ag
    :init
    (progn
      (defun trishume-helm-ag ()
        (interactive)
        (helm-ag (projectile-project-root)))
      (evil-leader/set-key
        "pa" 'trishume-helm-ag))
    :config
    (progn
      ;; (setq helm-ag-command-option "--all-text")
      (setq helm-ag-insert-at-point 'symbol))))
