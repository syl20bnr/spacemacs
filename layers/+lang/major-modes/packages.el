;;; packages.el --- Major modes Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(setq major-modes-packages
      '(
        arduino-mode
        (ebuild-mode :location (recipe :fetcher github :repo "emacsmirror/ebuild-mode"))
        evil-matchit
        gemini-mode
        (hoon-mode :location (recipe :fetcher github :repo "urbit/hoon-mode.el"))
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

(defun major-modes/init-gemini-mode ()
  (use-package gemini-mode
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'gemini-mode
        "l" 'gemini-insert-link
        "o" 'gemini-open-link-at-point
        "RET" 'gemini-insert-list-item
        "t" 'gemini-insert-time-stamp
        "n" 'gemini-insert-tinylog-header))))

(defun major-modes/init-hoon-mode ())

(defun major-modes/init-logcat ()
  (use-package logcat
    :commands (logcat logcat-mode)))

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
        "s" 'pkgbuild-update-srcinfo
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

(defun major-modes/post-init-evil-matchit ()
  (add-hook 'matlab-mode-hook 'turn-on-evil-matchit-mode))
