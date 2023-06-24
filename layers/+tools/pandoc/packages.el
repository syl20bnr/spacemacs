;;; packages.el --- pandoc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Christoph Paulik <cpaulik@gmail.com>
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


(defconst pandoc-packages
  '(pandoc-mode
    ox-pandoc))

(defun pandoc/init-pandoc-mode ()
  "Initialize my package"
  (use-package pandoc-mode
    :defer t
    :commands spacemacs/run-pandoc
    :init
    (spacemacs/declare-prefix "P" "pandoc")
    (spacemacs/set-leader-keys "P/" 'spacemacs/run-pandoc)
    :config
    (setq pandoc-data-dir (concat spacemacs-cache-directory "pandoc/"))
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)))

(defun pandoc/init-ox-pandoc ()
  (use-package ox-pandoc
    :defer t
    :init
    (with-eval-after-load 'org (require 'ox-pandoc))))
