(defvar extra-langs-packages
  '(
    idris-mode
    arduino-mode
    scad-mode
    qml-mode
    julia-mode
    nix-mode
    racket-mode
    yaml-mode
    rust-mode
    wolfram-mode
    ))

(defun extra-langs/init-arduino-mode ()
  (use-package arduino-mode :defer t))

(defun extra-langs/init-idris-mode ()
  (use-package idris-mode :defer t))

(defun extra-langs/init-scad-mode ()
  (use-package scad-mode :defer t))

(defun extra-langs/init-nix-mode ()
  (use-package nix-mode :defer t))

(defun extra-langs/init-qml-mode ()
  (use-package qml-mode :defer t))

(defun extra-langs/init-julia-mode ()
  (use-package julia-mode :defer t))

(defun extra-langs/init-yaml-mode ()
  (use-package yaml-mode :defer t))

(defun extra-langs/init-rust-mode ()
  (use-package rust-mode :defer t))

;; no associated extension because conflicts with more common Objective-C, manually invoke for .m files.
(defun extra-langs/init-wolfram-mode ()
  (use-package wolfram-mode
    :defer t
    :interpreter "\\(Wolfram\\|Mathematica\\)Script\\( -script\\)?"))

(defun extra-langs/init-racket-mode ()
  (use-package racket-mode
    :defer t
    :config
    (progn
      (use-package smartparens :config
        (progn (add-to-list 'sp--lisp-modes 'racket-mode)
               (when (fboundp 'sp-local-pair) (sp-local-pair 'racket-mode "`" nil :actions nil))))
      (sp-local-pair 'racket-mode "'" nil :actions nil)
      (evil-leader/set-key-for-mode 'racket-mode
        "ml" 'evil-lisp-state
        "mt" 'racket-test
        "mg" 'racket-visit-definition
        "mhd" 'racket-doc)
      (add-hook 'racket-mode-hook
                '(lambda ()
                   (define-key racket-mode-map (kbd "H-r") 'racket-run))))))
