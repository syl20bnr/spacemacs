(defvar trishume-packages
  '(
    auctex
    smooth-scrolling
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
    ))

(when (member 'trishume dotspacemacs-configuration-layers)
  (add-to-list 'trishume-packages 'company-auctex))

(defun trishume/init-auctex ()
  (defun load-auctex-on-demand ()
    (interactive)
    (use-package tex
      :config
      (progn
        (use-package smartparens
          :config (require 'smartparens-latex))

        (when (member 'company-mode dotspacemacs-configuration-layers)
          (use-package company-auctex
            :init (company-auctex-init)))

        (defun build-view ()
          (interactive)
          (if (buffer-modified-p)
              (progn
                (let ((TeX-save-query nil))
                  (TeX-save-document (TeX-master-file)))
                (setq build-proc (TeX-command "LaTeX" 'TeX-master-file -1))
                (set-process-sentinel  build-proc  'build-sentinel))
            (TeX-view)))

        (defun build-sentinel (process event)
          (if (string= event "finished\n")
              (TeX-view)
            (message "Errors! Check with C-`")))

        (add-hook 'LaTeX-mode-hook '(lambda () (local-set-key (kbd "H-r") 'build-view)))
        (add-hook 'LaTeX-mode-hook 'flyspell-mode)
        (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
        (add-hook 'LaTeX-mode-hook 'spacemacs/load-yasnippet)

        (evil-leader/set-key
          "oe" 'LaTeX-environment
          "oc" 'LaTeX-close-environment)

        (setq-default TeX-auto-save t)
        (setq-default TeX-parse-self t)
        (setq-default TeX-master nil)
        (setq-default TeX-PDF-mode t))))
  (evil-leader/set-key
    "el" 'load-auctex-on-demand))

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
    (add-hook 'racket-mode-hook
          '(lambda ()
             (define-key racket-mode-map (kbd "H-r") 'racket-run)))))
