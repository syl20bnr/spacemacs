(defvar paradox-packages
  '(
    paradox
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar paradox-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function paradox/init-<package-paradox>
;;
(defun paradox/init-paradox ()
  (use-package paradox
    :commands paradox-list-packages
    :defer t
    :init
    (progn
      (defun spacemacs/paradox-list-packages ()
        (interactive)
        (require 'epa-file)
        (require 'auth-source)
        (when (and (not (boundp 'paradox-github-token)) 
                   (file-exists-p "~/.authinfo.gpg"))
          (let ((authinfo-result (car (auth-source-search 
                                       :max 1
                                       :host "github.com"
                                       :port "paradox" 
                                       :user "paradox"
                                       :require '(:secret)))))
            (let ((paradox-token (plist-get authinfo-result :secret))) 
              (setq paradox-github-token (if (functionp paradox-token)
                                             (funcall paradox-token)
                                           paradox-token)))))
        (paradox-list-packages nil))
      
        (eval-after-load "evil-leader"
          (evil-leader/set-key
            "aP" 'spacemacs/paradox-list-packages))
        )
    ))
      

