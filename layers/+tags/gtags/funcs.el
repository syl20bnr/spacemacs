;;; funcs.el --- gtags functions File
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
  "Define key bindings for the specific MODE."
  ;; `counsel-gtags-dwim' is added to the end of the mode-specific jump handlers
  ;; Some modes have more sophisticated jump handlers that go to the beginning
  ;; It might be possible to add `counsel-gtags-dwim' instead to the default
  ;; handlers, if it does a reasonable job in ALL modes.
  (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" mode)))
        (handler '(spacemacs/counsel-gtags-maybe-dwim
                   :async spacemacs//counsel-gtags-dwim-success)))
    (when (boundp jumpl) (add-to-list jumpl handler 'append)))

  ;; TODO: Add missing keybindings when new functions get added
  (spacemacs/set-leader-keys-for-major-mode mode
    "gC" 'counsel-gtags-create-tags
    "gd" 'counsel-gtags-dwim
    ;; "gD" 'helm-gtags-find-tag-other-window
    "gf" 'counsel-gtags-find-file
    ;; "gG" 'helm-gtags-dwim-other-window
    ;; "gi" 'helm-gtags-tags-in-this-function
    ;; "gl" 'helm-gtags-parse-file
    "gn" 'counsel-gtags-go-forward
    "gp" 'counsel-gtags-go-backward
    "gr" 'counsel-gtags-find-reference
    ;; "gR" 'helm-gtags-resume
    ;; "gs" 'helm-gtags-select
    ;; "gS" 'helm-gtags-show-stack
    "gy" 'counsel-gtags-find-symbol
    "gu" 'counsel-gtags-update-tags))

(defun helm-gtags-dwim-other-window ()
  "helm-gtags-dwim in the other window"
  (interactive)
  (let ((helm-gtags--use-otherwin t)
        (split-height-threshold nil)
        (split-width-threshold 140))
    (helm-gtags-dwim)))

(defun spacemacs/helm-gtags-maybe-dwim ()
  "Runs `helm-gtags-dwim' if `gtags-enable-by-default' is on.
Otherwise does nothing."
  (interactive)
  (when gtags-enable-by-default
    (call-interactively 'helm-gtags-dwim)))

(defun spacemacs/helm-gtags-define-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  ;; The functionality of `helm-gtags-mode' is pretty much entirely superseded
  ;; by `ggtags-mode', so we don't add this hook
  ;; (let ((hook (intern (format "%S-hook" mode))))
  ;;   (add-hook hook 'helm-gtags-mode))

  ;; `helm-gtags-dwim' is added to the end of the mode-specific jump handlers
  ;; Some modes have more sophisticated jump handlers that go to the beginning
  ;; It might be possible to add `helm-gtags-dwim' instead to the default
  ;; handlers, if it does a reasonable job in ALL modes.
  (let ((jumpl (intern (format "spacemacs-jump-handlers-%S" mode))))
    (when (boundp jumpl)
      (add-to-list jumpl 'spacemacs/helm-gtags-maybe-dwim 'append)))

  (spacemacs/set-leader-keys-for-major-mode mode
    "gC" 'helm-gtags-create-tags
    "gd" 'helm-gtags-find-tag
    "gD" 'helm-gtags-find-tag-other-window
    "gf" 'helm-gtags-select-path
    "gG" 'helm-gtags-dwim-other-window
    "gi" 'helm-gtags-tags-in-this-function
    "gl" 'helm-gtags-parse-file
    "gn" 'helm-gtags-next-history
    "gp" 'helm-gtags-previous-history
    "gr" 'helm-gtags-find-rtag
    "gR" 'helm-gtags-resume
    "gs" 'helm-gtags-select
    "gS" 'helm-gtags-show-stack
    "gy" 'helm-gtags-find-symbol
    "gu" 'helm-gtags-update-tags))

(defun spacemacs/ggtags-mode-enable ()
  "Enable ggtags and eldoc mode.

For eldoc, ggtags advises the eldoc function at the lowest priority
so that if the major mode has better support it will use it first."
  (when gtags-enable-by-default
    (ggtags-mode 1)
    (eldoc-mode 1)))
