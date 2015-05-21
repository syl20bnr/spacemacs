;;; core-use-package-ext.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst spacemacs--use-package-add-hook-keywords '(:pre-init
                                                     :post-init
                                                     :pre-config
                                                     :post-config))

(defmacro spacemacs|use-package-add-hook (name &rest plist)
  "Add post hooks to `:init' or `:config' arguments of an existing
configuration.

In order to use this macro the variable `use-package-inject-hooks'
must be non-nil.

This is useful in the dotfile to override the default configuration
of a package.

Usage:

  (use-package-add-hook package-name
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
      (let ((body (spacemacs/mplist-get plist keyword)))
        (when body
          (let ((hook (intern (format "use-package--%S--%s-hook"
                                      name-symbol
                                      (substring (format "%s" keyword) 1)))))
            (push `(add-hook ',hook (lambda nil ,@body)) expanded-forms)))))
    `(progn ,@expanded-forms)))

;; Temporary fix until #213 is merged upstream
(eval-after-load 'use-package
  '(defun use-package-hook-injector (name-string keyword body)
  "Wrap pre/post hook injections around a given keyword form.
ARGS is a list of forms, so `((foo))' if only `foo' is being called."
  (if (not use-package-inject-hooks)
      (use-package-expand name-string (format "%s" keyword) body)
    (let ((keyword-name (substring (format "%s" keyword) 1)))
      (when body
        `((when ,(macroexp-progn
                  (use-package-expand name-string (format "pre-%s hook" keyword)
                    `((run-hook-with-args-until-failure
                       ',(intern (concat "use-package--" name-string
                                         "--pre-" keyword-name "-hook"))))))
            ,(macroexp-progn
              (use-package-expand name-string (format "%s" keyword) body))
            ,(macroexp-progn
              (use-package-expand name-string (format "post-%s hook" keyword)
                `((run-hooks
                  ',(intern (concat "use-package--" name-string
                                    "--post-" keyword-name "-hook")))))))))))))

(provide 'core-use-package-ext)
