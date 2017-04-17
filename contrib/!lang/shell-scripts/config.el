;;; packages.el --- Shell Scripts Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: James Conroy-Finn <james@logi.cl>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
(dolist (pattern '("\\.zsh\\'"
                   "zlogin\\'"
                   "zlogout\\'"
                   "zpreztorc\\'"
                   "zprofile\\'"
                   "zshenv\\'"
                   "zshrc\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))

;; When we open a `.zsh' file set the shell to ZSH.
(add-hook
 'sh-mode-hook
 (lambda ()
   (if (string-match "\\.zsh$" buffer-file-name)
       (sh-set-shell "zsh"))))
