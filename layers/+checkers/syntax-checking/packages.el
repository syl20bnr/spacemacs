;;; packages.el --- Syntax Checking Layer packages File for Spacemacs
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

;;; Commentary:

;;; Code:


(defconst syntax-checking-packages
  '(
    flycheck
    flycheck-pos-tip
    popwin))

(defun syntax-checking/init-flycheck ()
  (use-package flycheck
    :defer t
    :spacediminish (" ⓢ" " s")
    :init
    (spacemacs|add-transient-hook prog-mode-hook
      (lambda () (when syntax-checking-enable-by-default
                   (global-flycheck-mode 1)))
      lazy-load-flycheck)
    (setq flycheck-standard-error-navigation syntax-checking-use-standard-error-navigation
          flycheck-display-errors-delay (or syntax-checking-tooltips-delay 0.9)
          flycheck-global-modes nil)
    ;; key bindings
    (spacemacs/set-leader-keys
      "eb" #'flycheck-buffer
      "ec" #'flycheck-clear
      "ed" #'flycheck-disable-checker
      "eh" #'flycheck-describe-checker
      "el" #'spacemacs/toggle-flycheck-error-list
      "eL" #'spacemacs/goto-flycheck-error-list
      "es" #'flycheck-select-checker
      "eS" #'flycheck-set-checker-executable
      "ev" #'flycheck-verify-setup
      "ey" #'flycheck-copy-errors-as-kill
      "ex" #'flycheck-explain-error-at-point)
    (spacemacs|add-toggle syntax-checking
      :mode flycheck-mode
      :documentation "Enable error and syntax checking."
      :evil-leader "ts")
    :config
    ;; Custom fringe/margin indicator
    (pcase-let ((`(,bitmap . ,margin-str) syntax-checking-indication-symbol))
      (when (booleanp syntax-checking-use-original-bitmaps)
        (warn "`syntax-checking-use-original-bitmaps' is deprecated. Use `syntax-checking-indication-symbol' instead.")
        (setq bitmap (unless syntax-checking-use-original-bitmaps
                       'syntax-checking--fringe-indicator)))
      (flycheck-redefine-standard-error-levels margin-str bitmap))

    (evilified-state-evilify-map flycheck-error-list-mode-map
      :mode flycheck-error-list-mode
      :bindings
      "j" #'flycheck-error-list-next-error
      "k" #'flycheck-error-list-previous-error
      "J" #'next-line
      "K" #'previous-line)))

(defun syntax-checking/init-flycheck-pos-tip ()
  (use-package flycheck-pos-tip
    :if syntax-checking-enable-tooltips
    :after (flycheck)
    :init
    (flycheck-pos-tip-mode)
    (setq flycheck-pos-tip-timeout (or syntax-checking-auto-hide-tooltips 0))))

(defun syntax-checking/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push syntax-checking--buffer-config
          popwin:special-display-config)))
