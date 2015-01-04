(defvar fasd-packages
  '(
    fasd
    ;; package fasds go here
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar fasd-excluded-packages '()
  "List of packages to exclude.")

(defun fasd/init-fasd ()
  "initializes fasd-emacs and adds a key binding to <SPC f z>"
  (use-package fasd
    :init
    (progn
      (global-fasd-mode 1)
      (evil-leader/set-key "fz" 'fasd-find-file))))
;; For each package, define a function fasd/init-<package-fasd>
;;
;; (defun fasd/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
