;;; This is a spacemacs layer for the password-store package which provides
;;; bindings for the `pass` password manager.

(defconst password-store-packages
  '(
    password-store
    (password-store :location (recipe
                              :fetcher github
                              :repo "zx2c4/password-store"
                              :files ("contrib/emacs/*.el")))
   ))

(defun password-store/init-password-store ()
                                           (use-package password-store
                                                       :defer t
                                                       :init
                                                       (progn
                                                         (spacemacs/declare-prefix "P" "Password Store")
                                                         (evil-leader/set-key
                                                           "Pc" 'password-store-copy
                                                           "PC" 'password-store-clear
                                                           "Pg" 'password-store-get
                                                           "Pi" 'password-store-insert
                                                           "Pg" 'password-store-generate
                                                           "Pr" 'password-store-remove
                                                           "PR" 'password-store-rename))
                                                       ))
