;;; packages.el --- Pact Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Colin Woodbury <colin@kadena.io>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst pact-packages
  '(
    flycheck
    flycheck-pact
    pact-mode
    ))

(defun pact/post-init-flycheck ()
  "Initialize flycheck."
  (spacemacs/enable-flycheck 'pact-mode))

(defun pact/init-flycheck-pact ()
  "Initialize flycheck-pact."
  (use-package flycheck-pact
    :commands flycheck-pact-interactive-buffer
    :init (add-hook 'flycheck-mode-hook 'flycheck-pact-interactive-buffer)))

(defun pact/init-pact-mode ()
  "Initialize pact-mode."
  (use-package pact-mode
    :defer t

    :init
    (progn
      (spacemacs/register-repl 'pact-mode 'spacemacs/pact-repl "pact"))

    :config
    (progn
      (defun spacemacs/pact-repl ()
        "Open a pact repl in a side frame."
        (interactive)
        (if (get-buffer-process inferior-lisp-buffer)
            ;; Borrowed from `switch-to-lisp':
            (let ((pop-up-frames
                   ;; Be willing to use another frame
                   ;; that already has the window in it.
                   (or pop-up-frames
                       (get-buffer-window inferior-lisp-buffer t))))
              (pop-to-buffer inferior-lisp-buffer))
          (progn
            (spacemacs/new-empty-buffer-right)
            (pact-mode)
            (run-lisp inferior-lisp-program))))

      (defun spacemacs/pact-load-file ()
        "Load the current buffer into the Pact repl, optionally starting
the repl if it hasn't yet been."
        (interactive)
        (let ((curr (current-buffer)))
          (progn
            (unless (get-buffer-process inferior-lisp-buffer)
              (spacemacs/pact-repl)
              (pop-to-buffer curr))
            (pact-load-file nil)
            (pop-to-buffer curr))))

      (dolist (prefix '(("ms" . "repl")))
        (spacemacs/declare-prefix-for-mode 'pact-mode (car prefix) (cdr prefix)))

      (spacemacs/set-leader-keys-for-major-mode 'pact-mode
        ;; REPL
        "s'" 'spacemacs/pact-repl
        "sb" 'spacemacs/pact-load-file))))

;;; packages.el ends here
