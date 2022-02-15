;;; core-use-package-ext.el --- Spacemacs Core File
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

(defconst spacemacs--use-package-add-hook-keywords '(:pre-init
                                                     :post-init
                                                     :pre-config
                                                     :post-config))

(defmacro spacemacs|use-package-add-hook (name &rest plist)
  "Add post hooks to `:init' or `:config' arguments of an existing
configuration.

In order to use this macro the variable `use-package-inject-hooks'
must be non-nil. If it is not a warning will be issued.

This is useful in the dotfile to override the default configuration
of a package.

Usage:

  (spacemacs|use-package-add-hook package-name
     [:keyword [option]]...)

:pre-init      Code to run before the default `:init' configuration.
:post-init     Code to run after the default `:init' configuration.
:pre-config    Code to run before the default `:config' configuration.
:post-config   Code to run after the default `:config' configuration.

In practice the most useful hook is the `:post-config' where you can
override lazy-loaded settings."
  (declare (indent 1))
  (let ((name-symbol (if (stringp name) (intern name) name))
        (expanded-forms '()))
    (dolist (keyword spacemacs--use-package-add-hook-keywords)
      (let ((body (spacemacs/mplist-get-values plist keyword)))
        (when body
          (let ((hook (intern (format "use-package--%S--%s-hook"
                                      name-symbol
                                      (substring (format "%s" keyword) 1)))))
            (push `(add-hook ',hook (lambda nil ,@body t)) expanded-forms)))))
    `(progn ,@expanded-forms)))

(provide 'core-use-package-ext)
