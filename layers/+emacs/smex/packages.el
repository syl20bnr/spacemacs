;;; packages.el --- smex Layer packages File for Spacemacs
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


(setq smex-packages '(smex))

(defun smex/init-smex ()
  (use-package smex
    :defer t
    :init
    (progn
      (setq-default smex-history-length 32
                    smex-save-file (concat spacemacs-cache-directory
                                           ".smex-items"))
      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'emacs-startup-hook
                (lambda () (spacemacs/set-leader-keys
                             dotspacemacs-emacs-command-key 'spacemacs/smex)))
      (spacemacs/set-leader-keys "m:" 'spacemacs/smex-major-mode-commands)
      (global-set-key (kbd "M-x") 'spacemacs/smex))))
