(setq major-modes-packages
      '(
        arduino-mode
        (ebuild-mode :location (recipe :fetcher github :repo "emacsmirror/ebuild-mode"))
        (hoon-mode :location (recipe :fetcher github :repo "urbit/hoon-mode.el"))
        kivy-mode
        (logcat :location (recipe :fetcher github :repo "dcolascione/logcat-mode"))
        matlab-mode
        pkgbuild-mode
        qml-mode
        scad-mode
        stan-mode
        thrift
        vala-mode
        (vala-snippets :requires yasnippet)
        wolfram-mode
        ))

(defun major-modes/init-arduino-mode ())

(defun major-modes/init-ebuild-mode ()
  (use-package ebuild-mode
    :mode ("\\.\\(ebuild\\|eclass\\)" . ebuild-mode)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'ebuild-mode
        "n" 'ebuild-mode-insert-skeleton
        "k" 'ebuild-mode-keyword
        "e" 'ebuild-run-command
        "a" 'ebuild-run-echangelog))))

(defun major-modes/init-hoon-mode ())

(defun major-modes/init-logcat ()
  (use-package logcat
    :commands (logcat logcat-mode)))

(defun major-modes/init-kivy-mode ())

(defun major-modes/init-matlab-mode ()
  (use-package matlab-mode
    :defer t
    :init
    ;; Explicitly run prog-mode hooks since matlab-mode does not derive from
    ;; prog-mode major-mode
    (add-hook 'matlab-mode-hook 'spacemacs/run-prog-mode-hooks)))

(defun major-modes/init-pkgbuild-mode ()
  (use-package pkgbuild-mode
    :mode ("\\`PKGBUILD\\'" . pkgbuild-mode)
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'pkgbuild-mode
        "r" 'pkgbuild-increase-release-tag
        "b" 'pkgbuild-makepkg
        "a" 'pkgbuild-tar
        "u" 'pkgbuild-browse-url
        "m" 'pkgbuild-update-sums-line
        "e" 'pkgbuild-etags))))

(defun major-modes/init-qml-mode ()
  (use-package qml-mode
    :defer t
    :mode "\\.qml\\'"))

(defun major-modes/init-scad-mode ())

(defun major-modes/init-stan-mode ())

(defun major-modes/init-thrift ())

(defun major-modes/init-vala-mode ()
  (use-package vala
    :defer t
    :init
    ;; Explicitly run prog-mode hooks since vala-mode does not derive from
    ;; prog-mode major-mode
    (add-hook 'vala-mode-hook 'spacemacs/run-prog-mode-hooks)))

(defun major-modes/init-vala-snippets ())

;; .m files are not associated because conflict with more common Objective-C and
;; MATLAB/Octave, manually invoke for .m files.
(defun major-modes/init-wolfram-mode ()
  (use-package wolfram-mode
    :defer t
    :interpreter "\\(Wolfram\\|Mathematica\\)Script\\( -script\\)?"
    :mode "\\.wl\\'"))
