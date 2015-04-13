(defvar puppet-packages
  '(
    ;; package puppets go here
    puppet-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar puppet-excluded-packages '())

;; For each package, define a function puppet-mode/init-<package-puppet-mode>
;;
;; (defun puppet-mode/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun puppet/init-puppet-mode ()
  "Initialize Puppet mode"
  (use-package puppet-mode
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'puppet-mode
        "m{" 'beginning-of-defun
        "m}" 'end-of-defun
        "m$" 'puppet-interpolate
        "ma" 'puppet-align-block
        "m'" 'puppet-toggle-string-quotes
        "m;" 'puppet-clear-string
        "mj" 'imenu
        "mc" 'puppet-apply
        "mv" 'puppet-validate
        "ml" 'puppet-lint
      ))))
