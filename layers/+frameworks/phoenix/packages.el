;;; packages.el --- phoenix layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Lyuben Petrov <lyuben.y.petrov@gmail.com>
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


(defconst phoenix-packages '(alchemist))

(defun phoenix/post-init-alchemist ()
  (progn
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mf" "phoenix")
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mff" "find")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
      "ffw" 'alchemist-phoenix-find-web
      "ffv" 'alchemist-phoenix-find-views
      "ffc" 'alchemist-phoenix-find-controllers
      "ffC" 'alchemist-phoenix-find-channels
      "fft" 'alchemist-phoenix-find-templates
      "ffm" 'alchemist-phoenix-find-models
      "ffs" 'alchemist-phoenix-find-static
      "ffr" 'alchemist-phoenix-router
      "fr" 'alchemist-phoenix-routes)))
