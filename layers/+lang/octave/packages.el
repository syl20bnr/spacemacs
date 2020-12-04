;;; packages.el --- Octave Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq octave-packages
      '(
        ggtags
        counsel-gtags
        helm-gtags
        (octave :location built-in)
        ))

(defun octave/init-octave ()
  (use-package octave
    :mode ("\\.m\\'" . octave-mode)
    :commands (run-octave)
    :init (space-macs/register-repl 'octave 'run-octave "octave")
    :config (space-macs/set-leader-keys-for-major-mode 'octave-mode
              ;; helpers
              "hh" 'octave-help
              "hi" 'octave-lookfor
              ;; REPL
              "'" 'run-octave
              "sb" 'octave-send-buffer
              "sf" 'octave-send-defun
              "si" 'run-octave
              "sl" 'octave-send-line
              "sr" 'octave-send-region)))

(defun octave/post-init-ggtags ()
  (add-hook 'octave-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun octave/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'octave-mode))

(defun octave/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'octave-mode))


