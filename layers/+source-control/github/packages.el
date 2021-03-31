;;; packages.el --- Github Layer packages File for Spacemacs
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


(setq github-packages
      '(
        gist
        github-clone
        github-search
        grip-mode
        ;; this package does not exits, we need it to wrap
        ;; the call to spacemacs/declare-prefix.
        (spacemacs-github :location built-in)
        ))

(defun github/init-gist ()
  (use-package gist
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "gg" "github gist")
      (spacemacs/set-leader-keys
        "ggb" 'gist-buffer
        "ggB" 'gist-buffer-private
        "ggl" 'gist-list
        "ggr" 'gist-region
        "ggR" 'gist-region-private))
    :config
    (progn
      (evilified-state-evilify-map gist-list-menu-mode-map
        :mode gist-list-mode
        :bindings
        "f" 'gist-fetch-current
        "K" 'gist-kill-current
        "o" 'gist-browse-current-url)
      (evilified-state-evilify-map gist-list-mode-map
        :mode gist-list-mode
        :bindings
        (kbd "gr") 'gist-list-reload))))

(defun github/init-github-clone ()
  (use-package github-clone
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "ghc" "clone")
      (spacemacs/set-leader-keys
        "ghcc" 'github-clone
        "ghcr" 'github-clone-add-existing-remote
        "ghcf" 'github-clone-fork-remote
        "ghcu" 'github-clone-add-source-remote))))

(defun github/init-github-search ()
  (use-package github-search
    :commands (github-search-clone-repo github-search-user-clone-repo)
    :init (spacemacs/set-leader-keys "ghc/" 'github-search-clone-repo)))

(defun github/init-grip-mode ()
  (use-package grip-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "ghp" 'grip-mode))))

(defun github/init-spacemacs-github ()
  (spacemacs/declare-prefix "gh" "github"))
