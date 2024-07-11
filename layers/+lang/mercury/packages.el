;;; packages.el --- mercury layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Ludvig Böklin <ludvig.boklin@protonmail.com>
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


(defconst mercury-packages
  '((metal-mercury-mode :location (recipe
                                   :fetcher github
                                   :repo "ahungry/metal-mercury-mode"
                                   :commit "99e2d8fb7177cae3bfa2dec2910fc28216d5f5a8"))
    flycheck
    (flycheck-mercury :requires flycheck)
    smartparens))

(defun mercury/post-init-flycheck ()
  (add-hook 'metal-mercury-mode-hook 'flycheck-mode))

(defun mercury/init-flycheck-mercury ()
  "Initialize flycheck-mercury"
  (use-package flycheck-mercury))

(defun mercury/init-metal-mercury-mode ()
  "Initialize metal-mercury-mode"
  (use-package metal-mercury-mode
    :defer t
    :init
    :mode ("\\.m\\'" . metal-mercury-mode)
    :config
    (dolist (x '(
                 ;; ("m=" . "format")
                 ("mc" . "mercury/compile")
                 ;; ("mh" . "help")
                 ))
      (spacemacs/declare-prefix-for-mode 'metal-mercury-mode (car x) (cdr x)))

    (spacemacs/set-leader-keys-for-major-mode 'metal-mercury-mode
      ;; make
      "cb" 'metal-mercury-mode-compile
      "cr" 'metal-mercury-mode-runner)))

(defun mercury/post-init-smartparens ()
  (add-hook 'metal-mercury-mode-hook #'spacemacs//activate-smartparens))
