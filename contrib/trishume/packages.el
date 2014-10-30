(defvar trishume-packages
  '(
    auctex
    cdlatex
    smooth-scrolling
    idris-mode
    arduino-mode
    scad-mode
    qml-mode
    julia-mode
    helm-ag
    lua-mode
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

(defun trishume/init-arduino-mode ()
  (use-package arduino-mode :defer t))

(defun trishume/init-idris-mode ()
  (use-package idris-mode :defer t))

(defun trishume/init-scad-mode ()
  (use-package scad-mode :defer t))

(defun trishume/init-qml-mode ()
  (use-package qml-mode :defer t))

(defun trishume/init-julia-mode ()
  (use-package julia-mode :defer t))

(defun trishume/init-helm-ag ()
  (use-package helm-ag
    :init
    (progn
      (defun trishume-helm-ag ()
        (interactive)
        (helm-ag (projectile-project-root)))
      (evil-leader/set-key
        "pa" 'trishume-helm-ag))))

(defun trishume/init-lua-mode ()
  (use-package lua-mode
    :defer t))
