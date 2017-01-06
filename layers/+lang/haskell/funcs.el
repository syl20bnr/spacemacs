;;; funcs.el --- Haskell Layer funcs File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs-haskell//setup-completion-backend ()
  "Conditionally setup haskell completion backend."
  (unless (eq haskell-completion-backend 'ghc-mod)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
  (when (configuration-layer/package-usedp 'company)
    (pcase haskell-completion-backend
      (`ghci (spacemacs-haskell//setup-ghci))
      (`ghc-mod (spacemacs-haskell//setup-ghc-mod))
      (`intero (spacemacs-haskell//setup-intero)))))

(defun spacemacs-haskell//setup-ghci ()
  (add-to-list 'company-backends-haskell-mode
               '(company-ghci company-dabbrev-code company-yasnippet)))

(defun spacemacs-haskell//setup-ghc-mod ()
  (add-to-list 'company-backends-haskell-mode
               '(company-ghc company-dabbrev-code company-yasnippet))
  (ghc-init)
  (dolist (mode haskell-modes)
    (spacemacs/declare-prefix-for-mode mode "mm" "haskell/ghc-mod")
    (spacemacs/set-leader-keys-for-major-mode mode
      "mt" 'ghc-insert-template-or-signature
      "mu" 'ghc-initial-code-from-signature
      "ma" 'ghc-auto
      "mf" 'ghc-refine
      "me" 'ghc-expand-th
      "mn" 'ghc-goto-next-hole
      "mp" 'ghc-goto-prev-hole
      "m>" 'ghc-make-indent-deeper
      "m<" 'ghc-make-indent-shallower
      "hi" 'ghc-show-info
      "ht" 'ghc-show-type))
  (when (configuration-layer/package-usedp 'flycheck)
    ;; remove overlays from ghc-check.el if flycheck is enabled
    (set-face-attribute 'ghc-face-error nil :underline nil)
    (set-face-attribute 'ghc-face-warn nil :underline nil)))

(defun spacemacs-haskell//setup-intero ()
  (add-to-list 'company-backends-haskell-mode
               '(company-intero company-dabbrev-code company-yasnippet))
  (push 'intero-goto-definition spacemacs-jump-handlers)
  (intero-mode)
  (dolist (mode haskell-modes)
    (spacemacs/set-leader-keys-for-major-mode mode
      "hi" 'intero-info
      "ht" 'intero-type-at
      "hT" 'haskell-intero/insert-type
      "rs" 'intero-apply-suggestions
      "sb" 'intero-repl-load))

  (dolist (mode (cons 'haskell-cabal-mode haskell-modes))
    (spacemacs/set-leader-keys-for-major-mode mode
      "sc"  nil
      "ss"  'haskell-intero/display-repl
      "sS"  'haskell-intero/pop-to-repl))

  (dolist (mode (append haskell-modes '(haskell-cabal-mode intero-repl-mode)))
    (spacemacs/declare-prefix-for-mode mode "mi" "haskell/intero")
    (spacemacs/set-leader-keys-for-major-mode mode
      "ic"  'intero-cd
      "id"  'intero-devel-reload
      "ik"  'intero-destroy
      "il"  'intero-list-buffers
      "ir"  'intero-restart
      "it"  'intero-targets))

  (evil-define-key '(insert normal) intero-mode-map
    (kbd "M-.") 'intero-goto-definition))


;; Intero functions

(defun haskell-intero/insert-type ()
  (interactive)
  (intero-type-at :insert))

(defun haskell-intero/display-repl (&optional prompt-options)
  (interactive "P")
  (let ((buffer (intero-repl-buffer prompt-options)))
    (unless (get-buffer-window buffer 'visible)
      (display-buffer buffer))))

(defun haskell-intero/pop-to-repl (&optional prompt-options)
  (interactive "P")
  (pop-to-buffer (intero-repl-buffer prompt-options)))

(defun haskell-intero//preserve-focus (f &rest args)
  (let ((buffer (current-buffer)))
    (apply f args)
    (pop-to-buffer buffer)))
