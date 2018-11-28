;;; packages.el --- Octave Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
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
    :init (spacemacs/register-repl 'octave 'run-octave "octave")
    :config (spacemacs/set-leader-keys-for-major-mode 'octave-mode
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
  (add-hook 'octave-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun octave/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'octave-mode))

(defun octave/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'octave-mode))
