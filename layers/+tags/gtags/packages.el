;;; packages.el --- gtags Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;;    and: Christian E. Hopps <chopps@gmail.com>
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


(defconst gtags-packages
  '(
    ggtags
    (counsel-gtags :requires ivy)
    (helm-gtags :requires helm)
    ))

(defun gtags/init-counsel-gtags ()
  (use-package counsel-gtags
    :defer t
    :init
    (setq counsel-gtags-ignore-case t
          counsel-gtags-auto-update t)
    (add-hook 'ggtags-mode-hook 'counsel-gtags-mode)
    (add-hook 'ggtags-mode-hook #'spacemacs/counsel-ggtags-set-jump-handler)
    :config
    ;; TODO add mixing commands
    (spacemacs/set-leader-keys-for-minor-mode 'counsel-gtags-mode
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
      "gu" 'counsel-gtags-update-tags)))

(defun gtags/init-ggtags ()
  (use-package ggtags
    :defer t
    :init
    (spacemacs|add-toggle ggtags-mode
      :status ggtags-mode
      :on (ggtags-mode nil)
      :off (ggtags-mode -1)
      :documentation "Toggle GNU Global source code tagging system."
      ;; TODO make a list of all supported major modes by ggtags, check
      ;; how it is done with evil-cleverparens
      ;; TODO update the documentation in each supported layers like it
      ;; has been done with evil-cleverparens and lisp dialects
      :evil-leader-for-mode
      (asm-mode . "Tg")
      (awk-mode . "Tg")
      (c-mode . "Tg")
      (c++-mode . "Tg")
      (clojure-mode . "Tg")
      (common-lisp-mode . "Tg")
      (compilation-mode . "Tg")
      (csharp-mode . "Tg")
      (d-mode . "Tg")
      (dired-mode . "Tg")
      (dos-mode . "Tg")
      (elixir-mode . "Tg")
      (emacs-lisp-mode . "Tg")
      (erlang-mode . "Tg")
      (fsharp-mode . "Tg")
      (go-mode . "Tg")
      (haskell-mode . "Tg")
      (java-mode . "Tg")
      (js2-mode . "Tg")
      (latex-mode . "Tg")
      (lua-mode . "Tg")
      (ocaml-mode . "Tg")
      (octave-mode . "Tg")
      (php-mode . "Tg")
      (python-mode . "Tg")
      (racket-mode . "Tg")
      (ruby-mode . "Tg")
      (rust-mode . "Tg")
      (scala-mode . "Tg")
      (scheme-mode . "Tg")
      (sh-mode . "Tg")
      (shell-mode . "Tg")
      (tcl-mode . "Tg")
      (vhdl-mode . "Tg")
      (vimrc-mode . "Tg"))
    :config
    (when (configuration-layer/package-used-p 'helm-gtags)
      ;; If anyone uses helm-gtags, they would want to use these key bindings.
      ;; These are bound in `ggtags-mode-map', since the functionality of
      ;; `helm-gtags-mode' is basically entirely contained within
      ;; `ggtags-mode-map' --- this way we don't have to enable both.
      ;; Note: all of these functions are autoloadable.
      (define-key ggtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      (define-key ggtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
      (define-key ggtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
      (define-key ggtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack))
    (spacemacs|diminish ggtags-mode " 🅶" " [g]")))

(defun gtags/init-helm-gtags ()
  (use-package helm-gtags
    :defer t
    :init
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t)
    (add-hook 'ggtags-mode-hook 'helm-gtags-mode)
    (add-hook 'ggtags-mode-hook #'spacemacs/helm-ggtags-set-jump-handler)
    :config
    (spacemacs/set-leader-keys-for-minor-mode 'helm-gtags-mode
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
       "gu" 'helm-gtags-update-tags)))
