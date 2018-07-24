(setq extra-langs-packages
  '(
    arduino-mode
    julia-mode
    matlab-mode
    qml-mode
    scad-mode
    stan-mode
    thrift
    ;; removed from MELPA (https://github.com/syl20bnr/spacemacs/issues/9795)
    ;; TODO re-enable this mode when it is added back to MELPA
    ;; wolfram-mode
    ))

(defun extra-langs/init-arduino-mode ()
  (use-package arduino-mode :defer t))

(defun extra-langs/init-scad-mode ()
  (use-package scad-mode :defer t))

(defun extra-langs/init-qml-mode ()
  (use-package qml-mode :defer t :mode "\\.qml\\'"))

(defun extra-langs/init-julia-mode ()
  (use-package julia-mode :defer t))

(defun extra-langs/init-matlab-mode ()
  (use-package matlab-mode
    :defer t
    :init
    ;; Explicitly run prog-mode hooks since matlab-mode does not derive from
    ;; prog-mode major-mode
    (add-hook 'matlab-mode-hook 'spacemacs/run-prog-mode-hooks)))

(defun extra-langs/init-stan-mode ()
  (use-package stan-mode :defer t))

(defun extra-langs/init-thrift ()
  (use-package thrift :defer t))

;; .m files are not associated because conflict with more common Objective-C and
;; MATLAB/Octave, manually invoke for .m files.
(defun extra-langs/init-wolfram-mode ()
  (use-package wolfram-mode
    :defer t
    :interpreter "\\(Wolfram\\|Mathematica\\)Script\\( -script\\)?"
    :mode "\\.wl\\'"))
