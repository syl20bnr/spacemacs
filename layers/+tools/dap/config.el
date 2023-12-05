;;; config.el --- DAP mode functions File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Ivan Yonchovski (yyoncho@gmail.com)
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



(defvar spacemacs--dap-supported-modes nil
  "List of modes supported by DAP.")

(defvar dap-enable-mouse-support t
  "If non-nil, enable `dap-mode''s mouse support.")

(defvar dap-enable-ui-controls t
  "If non-nil, enable `dap-mode''s UI controls.")

;; Force dap-mode's default for debugpy which is the new
;; engine. `ptvsd` is now deprecated, but their default is
;; hasn't yet been updated. `ptvsd` is currently broken
;; with spacemacs.
(setq dap-python-debugger 'debugpy)
