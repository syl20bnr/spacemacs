;;; packages.el --- rest layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author:  <wwguo@hiGDP>
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


(defconst restructuredtext-packages
  '(
    auto-complete
    ;; Disabled due to package is not longer maintained
    ;; (auto-complete-rst :requires auto-complete)

    ;; Linum is deprecated, use nlinum layer or native line numbers
    ;; linum
    (rst :location built-in)
    (rst-directives :location local)
    (rst-lists :location local)
    flyspell
    smartparens
    yasnippet))

(defun restructuredtext/post-init-auto-complete ()
  (add-hook 'rst-mode-hook 'auto-complete-mode))

;; (defun restructuredtext/init-auto-complete-rst ()
;;   (use-package auto-complete-rst
;;     :commands (auto-complete-rst-add-sources
;;                auto-complete-rst-init)
;;     :init (spacemacs/add-to-hook 'rst-mode-hook '(auto-complete-rst-init
;;                                                   auto-complete-rst-add-sources))))

;; (defun restructuredtext/post-init-linum ()
;;   ;; important auto-complete work-around to be applied to make both linum
;;   ;; and auto-complete to work together
;;   (when (configuration-layer/package-used-p 'auto-complete)
;;     (add-hook 'rst-mode-hook 'ac-linum-workaround t)))

(defun restructuredtext/init-rst-directives ()
  (use-package rst-directives))

(defun restructuredtext/init-rst-lists ()
  (use-package rst-lists))

(defun restructuredtext/init-rst ()
  (use-package rst
    :defer t
    :config (add-hook 'rst-adjust-hook 'rst-toc-update)))

(defun restructuredtext/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'rst-mode-hook)
  ;; important auto-complete work-around to be applied to make both flyspell
  ;; and auto-complete to work together
  (when (configuration-layer/package-used-p 'auto-complete)
    (add-hook 'rst-mode-hook 'ac-flyspell-workaround t)))

(defun restructuredtext/post-init-yasnippet ()
  (add-hook 'rst-mode-hook 'spacemacs/load-yasnippet))

(defun restructuredtext/post-init-smartparens ()
  (add-hook 'rst-mode-hook #'spacemacs//activate-smartparens))
