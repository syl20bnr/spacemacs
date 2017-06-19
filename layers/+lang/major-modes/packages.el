(setq major-modes-packages
      '(
        arduino-mode
        julia-mode
        (logcat :location (recipe :fetcher github :repo "dcolascione/logcat-mode"))
        matlab-mode
        qml-mode
        scad-mode
        stan-mode
        thrift
        wolfram-mode
        ))

(defun major-modes/init-arduino-mode ())

(defun major-modes/init-logcat ()
  (use-package logcat
    :commands (logcat logcat-mode)))

(defun major-modes/init-julia-mode ())

(defun major-modes/init-matlab-mode ()
  (use-package matlab-mode
    :defer t
    :init
    ;; Explicitly run prog-mode hooks since matlab-mode does not derive from
    ;; prog-mode major-mode
    (add-hook 'matlab-mode-hook 'spacemacs/run-prog-mode-hooks)))

(defun major-modes/init-qml-mode ()
  (use-package qml-mode
    :defer t
    :mode "\\.qml\\'"))

(defun major-modes/init-scad-mode ())

(defun major-modes/init-stan-mode ())

(defun major-modes/init-thrift ())

;; .m files are not associated because conflict with more common Objective-C and
;; MATLAB/Octave, manually invoke for .m files.
(defun major-modes/init-wolfram-mode ()
  (use-package wolfram-mode
    :defer t
    :interpreter "\\(Wolfram\\|Mathematica\\)Script\\( -script\\)?"
    :mode "\\.wl\\'"))
