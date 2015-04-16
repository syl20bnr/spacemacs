;;; config.el --- Eshell configuration File for Spacemacs
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

;; START: Code based on https://github.com/technomancy/emacs-starter-kit
(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-hist-ignoredups t
      eshell-buffer-shorthand t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
               '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
     (setq eshell-cmpl-cycle-completions nil)

     ;; TODO: submit these via M-x report-emacs-bug
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))
;; END

;; Move point to end of current prompt line if we were scrolling around in normal mode but now want to type.
(defun spacemacs//eshell-auto-end ()
  (when (and (eq major-mode 'eshell-mode)
             ;; Not on last line, we might want to edit within it.
             (not (eq (line-end-position) (point-max))))
    (end-of-buffer)))
(add-hook 'evil-insert-state-entry-hook 'spacemacs//eshell-auto-end)

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun spacemacs//eshell-setup-company ()
    ;; This completion backend is a bit laggy, I think it searches the PATH constantly.
    (setq-local company-idle-delay 0.2)
    ;; The default frontend screws everything up in short windows like terminal often are
    (setq-local company-frontends '(company-preview-frontend)))
  (add-hook 'eshell-mode-hook 'spacemacs//eshell-setup-company)
  (spacemacs|defvar-company-backends eshell-mode))
