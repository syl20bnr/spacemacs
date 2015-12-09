;;; packages.el --- Octave Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq octave-packages
  '(
    octave
    ))

(defun octave/init-octave ()
  (use-package octave
    :mode ("\\.m\\'" . octave-mode)
    :commands (run-octave)
    :config (evil-leader/set-key-for-mode 'octave-mode
              ;; helpers
              "mhd" 'octave-help
              "mhi" 'octave-lookfor
              ;; REPL
              "msb" 'octave-send-buffer
              "msf" 'octave-send-defun
              "msi" 'run-octave
              "msl" 'octave-send-line
              "msr" 'octave-send-region)))
