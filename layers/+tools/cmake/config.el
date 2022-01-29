;;; config.el --- CMake layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Alexander Dalshov <dalshov@gmail.com>
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


;; variables

(defconst cmake-modes '(c-mode c++-mode cmake-mode)
  "Primary major modes where `cmake-ide' could be used.")

(defvar cmake-enable-cmake-ide-support nil
  "If non nil CMake related packages and configuration are enabled.")

(defvar cmake-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'company-cmake)
  "The backend to use for IDE features.
Possible values are `lsp' and `company-cmake'.
If `nil' then 'company-cmake` is the default backend unless `lsp' layer is used")
