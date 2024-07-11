;;; packages.el --- Windows Scripts Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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
    (bat-mode :location built-in)
    bmx-mode
    (counsel-gtags :if (configuration-layer/package-used-p 'counsel))
    ggtags
    powershell))

(defun windows-scripts/init-bat-mode()
  (use-package bat-mode
    :commands (bat-cmd-help bat-run bat-run-args bat-template)
    :mode (("\\.bat\\'" . bat-mode)
           ("\\.cmd\\'" . bat-mode))
    :init
    (spacemacs/declare-prefix-for-mode 'bat-mode "me" "eval")
    (spacemacs/declare-prefix-for-mode 'bat-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'bat-mode "mi" "insert")
    :spacebind
    (:major
     (bat-mode
      ("e" "eval"
       ("b" bat-run "eval buffer")
       ("B" bat-run-args "eval buffer with args"))
      ("h" "help"
       ("h" bat-cmd-help "show help of cmd"))
      ("i" "insert"
       ("t" bat-template "insert minimal template"))
      ("z" windows-scripts/bat-outline "batch file outline")))))

(defun windows-scripts/init-bmx-mode()
  (use-package bmx-mode
    :commands (bmx-insert-colon-and-complete
               bmx-fixup-labels
               bmx-insert-percentage-and-complete
               bmx-fixup-variable
               bmx-find-references-at-point
               bmx-navigate-to-symbol-at-point
               bmx-rename-symbol-at-point
               bmx-fixup-labels-and-variables)
    :hook bat-mode
    :spacebind
    (:minor
     (bmx-mode
      ("g" "goto"
       ("d" bmx-navigate-to-symbol-at-point "go to definition")
       ("r" bmx-find-references-at-point "find references"))
      ("r" "refactor"
       ("r" bmx-rename-symbol-at-point "rename symbol")
       ("f" bmx-fixup-labels-and-variables ("ensure all symbols has consistent casing/syntax"
                                            :label "bmx-fixup")))))))

(defun windows-scripts/post-init-company ()
  (when (configuration-layer/package-used-p 'company)
    (add-hook 'company-completion-finished-hook #'bmx--company-completion-finished-hook)
    (spacemacs|add-company-backends
      :backends (bmx--company-label-backend bmx--company-variable-backend company-files company-capf)
      :modes bat-mode)))

(defun windows-scripts/post-init-ggtags ()
  (add-hook 'bat-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun windows-scripts/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'bat-mode))

(defun windows-scripts/init-powershell ()
  (use-package powershell
    :mode (("\\.ps1\\'"  . powershell-mode)
           ("\\.psm1\\'" . powershell-mode))
    :defer t
    :init
    (defun powershell/define-text-objects ()
      (spacemacs|define-text-object "$" "dollarparen" "$(" ")"))
    (add-hook 'powershell-mode-hook 'powershell/define-text-objects)
    (spacemacs/set-leader-keys
      "atsp" 'powershell)
    (spacemacs/set-leader-keys-for-major-mode 'powershell-mode
      "rr" 'powershell-regexp-to-regex)))
;; TODO
;; - split out powershell
;; - get help output with mgg (Get-Help) or Get-Help -online
;; -
