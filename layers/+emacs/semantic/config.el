;;; config.el --- semantic Layer configuration
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com
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


(setq srecode-map-save-file (concat spacemacs-cache-directory
                                    "srecode-map.el"))
(setq semanticdb-default-save-directory (concat spacemacs-cache-directory
                                                "semanticdb/"))
(setq semanticdb-search-system-databases nil)
(setq semanticdb-project-root-functions #'projectile-project-root)
(unless (file-exists-p semanticdb-default-save-directory)
  (make-directory semanticdb-default-save-directory))
