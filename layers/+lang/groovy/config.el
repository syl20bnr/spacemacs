;;; config.el --- Groovy layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Maxi Wolff <smile13241324@gmail.com>
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


;; Variables

(spacemacs|defc groovy-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'company-groovy)
  "The backend to use for IDE features.
Possible values are `lsp' and `company-groovy'.
If not set then 'company-groovy` is the default backend unless `lsp' layer is used"
  '(choice (const lsp) (const company-groovy)) nil t)

(spacemacs|defc groovy-lsp-jar-path "~/groovy-lsp-all.jar"
  "The path to the lsp jar file"
  '(file :must-match t) nil t)
