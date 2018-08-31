;;; packages.el --- Pact Layer packages File for Spacemacs
;;
;; Copyright (c) 2018 Kadena LLC
;;
;; Author: Colin Woodbury <colin@kadena.io>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst pact-packages
  '(
    ;; evil-cleverparens
    ;; flycheck
    ;; (flycheck-pact :requires flycheck)
    ;; pact-mode
    (pact-mode :location (recipe
                          :fetcher github
                          :repo "fosskers/pact-mode"))
    ))

;; (defun pact/post-init-flycheck ()
;;   (spacemacs/enable-flycheck 'pact-mode))

(defun pact/init-pact-mode ()
  (use-package pact-mode
    :defer t

    ;; TODO Enable repl
    ;; :init
    ;; (progn
    ;;   (spacemacs/register-repl 'racket-mode 'racket-repl "racket"))

    ;; :config
    ;; (progn

    ;;   ;; TODO is this actually visible to the user?
    ;;   (defun pact/load-file ()
    ;;     "Call `pact-load-file', loading the current buffer into an inferior process."
    ;;     (interactive)
    ;;     (pact-load-file))

    ;;   (dolist (prefix '(("ms" . "repl")))
    ;;     (spacemacs/declare-prefix-for-mode 'pact-mode (car prefix) (cdr prefix)))

    ;;   (spacemacs/set-leader-keys-for-major-mode 'pact-mode
    ;;     ;; REPL
    ;;     "sb" 'pact-load-file))))
    ))
