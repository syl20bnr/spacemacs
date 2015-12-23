(setq puppet-packages
  '(
    puppet-mode
    company
    flycheck
    ))

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
      (spacemacs/set-leader-keys-for-major-mode 'puppet-mode
        "{" 'beginning-of-defun
        "}" 'end-of-defun
        "$" 'puppet-interpolate
        "a" 'puppet-align-block
        "'" 'puppet-toggle-string-quotes
        ";" 'puppet-clear-string
        "j" 'imenu
        "c" 'puppet-apply
        "v" 'puppet-validate
        "l" 'puppet-lint
      ))))

(defun puppet/post-init-company ()
  (spacemacs|add-company-hook puppet-mode))

(defun puppet/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'puppet-mode-hook))
