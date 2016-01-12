;;; packages.el --- Octave Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    :config (spacemacs/set-leader-keys-for-major-mode 'octave-mode
              ;; helpers
              "hh" 'octave-help
              "hi" 'octave-lookfor
              ;; REPL
              "sb" 'octave-send-buffer
              "sf" 'octave-send-defun
              "si" 'run-octave
              "sl" 'octave-send-line
              "sr" 'octave-send-region)))
