(defvar extra-langs-packages
  '(
    arduino-mode
    idris-mode
    julia-mode
    nim-mode
    nix-mode
    qml-mode
    racket-mode
    rust-mode
    scad-mode
    wolfram-mode
    yaml-mode
    ))

(defun extra-langs/init-arduino-mode ()
  (use-package arduino-mode :defer t))

(defun extra-langs/init-idris-mode ()
  (use-package idris-mode :defer t))

(defun extra-langs/init-scad-mode ()
  (use-package scad-mode :defer t))

(defun extra-langs/init-nix-mode ()
  (use-package nix-mode :defer t))

(defun extra-langs/init-nim-mode ()
  (use-package nim-mode :defer t))

(defun extra-langs/init-qml-mode ()
  (use-package qml-mode :defer t))

(defun extra-langs/init-julia-mode ()
  (use-package julia-mode :defer t))

(defun extra-langs/init-yaml-mode ()
  (use-package yaml-mode :defer t))

(defun extra-langs/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :config
    (when (fboundp 'sp-local-pair) ; Don't pair lifetime specifiers
      (sp-local-pair 'rust-mode "'" nil :actions nil))))

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
      ;; Bug exists in Racket company backend that opens docs in new window when
      ;; company-quickhelp calls it. Note hook is appendended for proper ordering.
      (when (configuration-layer/package-declaredp 'company-quickhelp)
        (add-hook 'company-mode-hook
                  '(lambda () (when (equal major-mode 'racket-mode) (company-quickhelp-mode -1))) t))
      (add-hook 'racket-mode-hook
                '(lambda ()
                   (define-key racket-mode-map (kbd "H-r") 'racket-run))))))
