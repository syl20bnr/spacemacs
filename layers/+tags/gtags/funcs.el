;;; funcs.el --- gtags functions File
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


(defun spacemacs/counsel-gtags-maybe-dwim ()
  "Runs `counsel-gtags-dwim' if `gtags-enable-by-default' is on.
Otherwise does nothing."
  (interactive)
  (when gtags-enable-by-default
    (setq spacemacs--counsel-gtags-dwim-success nil)
    (setq spacemacs--counsel-gtags-dwim-success
          (call-interactively 'counsel-gtags-dwim))))

(defun spacemacs//counsel-gtags-dwim-success ()
  "Returns whether or not the last invocation of
  `spacemacs/counsel-gtags-maybe-dwim' was a success"
  spacemacs--counsel-gtags-dwim-success)

(defun spacemacs/counsel-gtags-define-keys-for-mode (mode)
  "Obsolete, does nothing."
  (message "spacemacs/counsel-gtags-define-keys-for-mode does nothing! %s doesn't have to call it anymore."
           mode))

(defun helm-gtags-dwim-other-window ()
  "helm-gtags-dwim in the other window"
  (interactive)
  (let ((helm-gtags--use-otherwin t)
        (split-height-threshold nil)
        (split-width-threshold 140))
    (helm-gtags-dwim)))

(defun spacemacs/helm-gtags-maybe-dwim ()
  "helm-gtags-dwim in same window"
  (interactive)
  (call-interactively 'helm-gtags-dwim))

(defun spacemacs/helm-gtags-define-keys-for-mode (mode)
  "Obsolete, does nothing."
  (message "spacemacs/helm-gtags-define-keys-for-mode does nothing! %s doesn't have to call it anymore."
           mode))

(defun spacemacs/helm-ggtags-set-jump-handler ()
  (add-to-list 'spacemacs-jump-handlers 'spacemacs/helm-gtags-maybe-dwim))

(defun spacemacs/counsel-ggtags-set-jump-handler ()
  (add-to-list 'spacemacs-jump-handlers 'spacemacs/counsel-gtags-maybe-dwim))

(defun spacemacs/ggtags-mode-enable ()
  "Enable ggtags and eldoc mode.

For eldoc, ggtags advises the eldoc function at the lowest priority
so that if the major mode has better support it will use it first."
  (when gtags-enable-by-default
    (ggtags-mode 1)
    (eldoc-mode 1)))
