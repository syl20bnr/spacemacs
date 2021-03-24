;;; packages.el --- Windows Scripts Layer packages File for Spacemacs
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


(defconst windows-scripts-packages
  '(
    (dos :location local)
    ggtags
    counsel-gtags
    helm-gtags
    powershell))

(defun windows-scripts/init-dos ()
  (use-package dos
    :commands dos-mode
    :mode (("\\.bat\\'" . dos-mode)
           ("\\.cmd\\'" . dos-mode))
    :init
    (progn
      (defun windows-scripts/dos-outline-hook ()
        (defun outline-mouse-select ()
          "Select position and return to `dos-mode'."
          (interactive)
          (dos-mode)
          (beginning-of-line)))
      (defun windows-scripts/dos-outline ()
        "Set a local binding to be able to return easily in dos-mode."
        (interactive)
        (dos-outline)
        (define-key evil-normal-state-local-map (kbd "SPC m z") 'dos-mode))
      (add-hook 'outline-mode-hook 'windows-scripts/dos-outline-hook))
    :config
    (spacemacs/set-leader-keys-for-major-mode 'dos-mode
      "hD" 'dos-help-cmd
      "eb" 'dos-run
      "eB" 'dos-run-args
      "s"  'dos-sep
      "t"  'dos-template-mini
      "T"  'dos-template
      "z"  'windows-scripts/dos-outline)))

(defun windows-scripts/post-init-ggtags ()
  (add-hook 'dos-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun windows-scripts/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'dos-mode))

(defun windows-scripts/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'dos-mode))

(defun windows-scripts/init-powershell ()
  (use-package powershell
    :mode (("\\.ps1\\'"  . powershell-mode)
           ("\\.psm1\\'" . powershell-mode))
    :defer t
    :init
    (progn
      (defun powershell/define-text-objects ()
        (spacemacs|define-text-object "$" "dollarparen" "$(" ")"))
      (add-hook 'powershell-mode-hook 'powershell/define-text-objects)
      (spacemacs/set-leader-keys
        "atsp" 'powershell)
      (spacemacs/set-leader-keys-for-major-mode 'powershell-mode
        "rr" 'powershell-regexp-to-regex)
      ;; TODO
      ;; - split out powershell
      ;; - get help output with mgg (Get-Help) or Get-Help -online
      ;; -
      )))
