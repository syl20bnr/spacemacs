;;; core-local-variables.el --- Spacemacs Core File
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

(require 'core-spacemacs-buffer)

(defvar spacemacs-available-project-types nil
  "List of available project types.

Project types are registered using the function `spacemacs/add-available-project-type'
in the `layers.el' files of layer.")

(defvar spacemacs-project-types nil
  "List of enabled project types for the current project.

This variable should a buffer local variable.")
(put 'spacemacs-project-types 'safe-local-variable #'listp)

(defun spacemacs/add-available-project-type (type)
  "Add project TYPE to `configuration-layer-project-types'."
  (add-to-list 'spacemacs-available-project-types type))

(defun spacemacs/add-local-var-hook (func &rest props)
  ""
  (let ((project-type (plist-get props :project-type))
        (major-mode (plist-get props :major-mode)))
    (when (and (not project-type) (not major-mode))
      (spacemacs-buffer/warning
       "No project type or major mode provided with local vars function: %S"
       func))
    (when (and project-type major-mode)
      (spacemacs-buffer/warning
       "Both project type and major mode scopes provided with local vars "
       "function: %S" func))
    (when project-type
      (add-hook (intern (format "spacemacs-%S-local-vars-project-type-hook"
                                project-type))
                func))
    (when major-mode
      (add-hook (intern (format "spacemacs-%S-local-vars-major-mode-hook"
                                major-mode))
                func))))

(defun spacemacs//hack-local-variables-hook-func ()
  "Run major mode hook configuration and enable projet minor modes.

This function is run after the local variables have been processed."
  (when spacemacs-project-types
    (dolist (type spacemacs-project-types)
      (run-hooks (intern (format "spacemacs-%S-local-vars-project-type-hook"
                                 type)))))
  (run-hooks (intern (format "spacemacs-%S-local-vars-major-mode-hook"
                             major-mode))))

;; install spacemacs hack local variables system
(add-hook 'hack-local-variables-hook
          #'spacemacs//hack-local-variables-hook-func)

(provide 'core-local-variables)
