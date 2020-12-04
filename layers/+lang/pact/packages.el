;;; packages.el --- Pact Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Colin Woodbury <colin@kadena.io>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
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
  (space-macs/enable-flycheck 'pact-mode))

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
      (space-macs/register-repl 'pact-mode 'space-macs/pact-repl "pact"))

    :config
    (progn
      (defun space-macs/pact-repl ()
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
            (space-macs/new-empty-buffer-right)
            (pact-mode)
            (run-lisp inferior-lisp-program))))

      (defun space-macs/pact-load-file ()
        "Load the current buffer into the Pact repl, optionally starting
the repl if it hasn't yet been."
        (interactive)
        (let ((curr (current-buffer)))
          (progn
            (unless (get-buffer-process inferior-lisp-buffer)
              (space-macs/pact-repl)
              (pop-to-buffer curr))
            (pact-load-file nil)
            (pop-to-buffer curr))))

      (dolist (prefix '(("ms" . "repl")))
        (space-macs/declare-prefix-for-mode 'pact-mode (car prefix) (cdr prefix)))

      (space-macs/set-leader-keys-for-major-mode 'pact-mode
        ;; REPL
        "s'" 'space-macs/pact-repl
        "sb" 'space-macs/pact-load-file))))

;;; packages.el ends here


