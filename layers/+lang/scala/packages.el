;;; packages.el --- Scala Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq scala-packages
  '(
    eldoc
    ensime
    flycheck
    flyspell
    ggtags
    counsel-gtags
    helm-gtags
    noflet
    org
    scala-mode
    sbt-mode
    ))

(defun scala/post-init-eldoc ()
  (when scala-enable-eldoc
    (add-hook 'scala-mode-hook #'spacemacs//java-setup-ensime-eldoc)))

(defun scala/pre-init-ensime ()
  (spacemacs|use-package-add-hook ensime
    :pre-config (add-to-list 'java--ensime-modes 'scala-mode)))

(defun scala/post-init-ensime ()
  (use-package ensime
    :defer t
    :init
    (progn
      (add-hook 'scala-mode-hook #'spacemacs//scala-setup-ensime)
      (when scala-auto-start-ensime
        (add-hook 'scala-mode-hook 'spacemacs//ensime-maybe-start)))
    :config
    (progn
      ;; Enable Expand Region integration from Ensime.  Ignore load errors to
      ;; handle older Ensime versions gracefully.
      (when (configuration-layer/package-used-p 'expand-region)
        (require 'ensime-expand-region nil 'noerror)))))

(defun scala/post-init-flycheck ()
  (spacemacs/enable-flycheck 'scala-mode)
  ;; Don't use scala checker if ensime mode is active, since it provides
  ;; better error checking.
  (with-eval-after-load 'flycheck
    (add-hook 'ensime-mode-hook 'spacemacs//scala-disable-flycheck-scala)))

(defun scala/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'scala-mode)
  (add-hook 'scala-mode-hook #'spacemacs//java-setup-ensime-flyspell))

(defun scala/init-noflet ()
  (use-package noflet))

(defun scala/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(scala . t))))

(defun scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'scala-mode
            "b." 'sbt-hydra
            "bb" 'sbt-command)))

(defun scala/init-scala-mode ()
  (use-package scala-mode
    :defer t
    :init
    (progn
      (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
        (add-to-list 'completion-ignored-extensions ext)))
    :config
    (progn
      ;; Automatically insert asterisk in a comment when enabled
      (defun scala/newline-and-indent-with-asterisk ()
        (interactive)
        (newline-and-indent)
        (when scala-auto-insert-asterisk-in-comments
          (scala-indent:insert-asterisk-on-multiline-comment)))

      (evil-define-key 'insert scala-mode-map
        (kbd "RET") 'scala/newline-and-indent-with-asterisk)

      ;; Automatically replace arrows with unicode ones when enabled
      (defconst scala-unicode-arrows-alist
        '(("=>" . "⇒")
          ("->" . "→")
          ("<-" . "←")))

      (defun scala/replace-arrow-at-point ()
        "Replace the arrow before the point (if any) with unicode ones.
An undo boundary is inserted before doing the replacement so that
it can be undone."
        (let* ((end (point))
               (start (max (- end 2) (point-min)))
               (x (buffer-substring start end))
               (arrow (assoc x scala-unicode-arrows-alist)))
          (when arrow
            (undo-boundary)
            (backward-delete-char 2)
            (insert (cdr arrow)))))

      (defun scala/gt ()
        "Insert a `>' to the buffer.
If it's part of a right arrow (`->' or `=>'),replace it with the corresponding
unicode arrow."
        (interactive)
        (insert ">")
        (scala/replace-arrow-at-point))

      (defun scala/hyphen ()
        "Insert a `-' to the buffer.
If it's part of a left arrow (`<-'),replace it with the unicode arrow."
        (interactive)
        (insert "-")
        (scala/replace-arrow-at-point))

      (when scala-use-unicode-arrows
        (define-key scala-mode-map
          (kbd ">") 'scala/gt)
        (define-key scala-mode-map
          (kbd "-") 'scala/hyphen))

      (evil-define-key 'normal scala-mode-map "J" 'spacemacs/scala-join-line)

      ;; Compatibility with `aggressive-indent'
      (setq scala-indent:align-forms t
            scala-indent:align-parameters t
            scala-indent:default-run-on-strategy
            scala-indent:operator-strategy))))

(defun scala/post-init-ggtags ()
  (add-hook 'scala-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun scala/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'scala-mode))

(defun scala/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'scala-mode))
