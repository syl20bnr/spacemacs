;;; packages.el --- Octave Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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
