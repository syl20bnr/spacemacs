(setq extra-langs-packages
  '(
    arduino-mode
    idris-mode
    julia-mode
    matlab-mode
    nim-mode
    nix-mode
    qml-mode
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
  (use-package qml-mode :defer t :mode "\\.qml\\'"))

(defun extra-langs/init-julia-mode ()
  (use-package julia-mode :defer t))

(defun extra-langs/init-matlab-mode ()
  (use-package matlab-mode :defer t))

(defun extra-langs/init-yaml-mode ()
  (use-package yaml-mode :defer t))

;; no associated extension because conflicts with more common Objective-C, manually invoke for .m files.
(defun extra-langs/init-wolfram-mode ()
  (use-package wolfram-mode
    :defer t
    :interpreter "\\(Wolfram\\|Mathematica\\)Script\\( -script\\)?"))
