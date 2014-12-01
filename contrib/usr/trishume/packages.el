(defvar trishume-packages
  '(
    idris-mode
    arduino-mode
    scad-mode
    qml-mode
    julia-mode
    helm-ag
    lua-mode
    racket-mode
    go-mode
    yaml-mode
    ag
    aggressive-indent
    hungry-delete
    ))

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
        "oa" 'trishume-helm-ag))))

(defun trishume/init-lua-mode ()
  (use-package lua-mode
    :defer t))

(defun trishume/init-go-mode ()
  (use-package go-mode
    :defer t))

(defun trishume/init-yaml-mode ()
  (use-package yaml-mode
    :defer t))

(defun trishume/init-racket-mode ()
  (use-package racket-mode
    :defer t
    :config
    (progn
      (add-to-list 'sp--lisp-modes 'racket-mode)
      (sp-local-pair 'racket-mode "'" nil :actions nil)
      (evil-leader/set-key-for-mode 'racket-mode
        "ml" 'evil-lisp-state
        "mt" 'racket-test
        "mg" 'racket-visit-definition
        "md" 'racket-doc)
      (add-hook 'racket-mode-hook
                '(lambda ()
                   (define-key racket-mode-map (kbd "H-r") 'racket-run))))))

(defun trishume/init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :init
    (add-to-hooks #'aggressive-indent-mode '(emacs-lisp-mode-hook
                                             racket-mode-hook
                                             css-mode-hook))
    :config
    (spacemacs//hide-lighter aggressive-indent-mode)))

(defun trishume/init-hungry-delete ()
  (use-package hungry-delete
    :init (global-hungry-delete-mode)
    :config
    (progn
      (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
      (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
      (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char))))
