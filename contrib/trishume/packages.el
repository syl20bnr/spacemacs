(defvar trishume-packages
  '(
    auctex
    cdlatex
    elscreen
    evil-tabs
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

(defun trishume/init-elscreen ()
  (use-package elscreen
    :config
    (progn
      (use-package evil-tabs
        :config (global-evil-tabs-mode 1))
      (require 'elscreen-color-theme)
      (evil-leader/set-key "ho" 'helm-elscreen)
      (elscreen-start))))
