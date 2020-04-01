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
        lsp-mode
        dap-mode
        eldoc
        flycheck
        flyspell
        lsp-treemacs
        counsel-gtags
        ggtags
        helm-gtags
        (ensime :toggle (spacemacs//scala-backend-ensime-p))
        sbt-mode
        scala-mode
        ))

(defun scala/post-init-eldoc ()
  (when (and scala-enable-eldoc (spacemacs//scala-backend-ensime-p))
    (add-hook 'scala-mode-hook #'spacemacs//scala-setup-ensime-eldoc)))

(defun scala/init-ensime ()
  (use-package ensime
    :defer t
    :if (spacemacs//scala-backend-ensime-p)
    :init
    (progn
      (setq ensime-startup-dirname (concat spacemacs-cache-directory "ensime/"))
      (spacemacs/register-repl 'ensime 'ensime-inf-switch "ensime")
      (add-hook 'scala-mode-hook #'spacemacs//scala-setup-ensime)
      (when scala-auto-start-backend
        (add-hook 'scala-mode-hook 'spacemacs//ensime-maybe-start)))
    :config
    (progn
      ;; This function was renamed in ensime. Usually we don't need to do this,
      ;; but documentation recommends the stable version of ensime, so we must
      ;; try to support it, too.
      (unless (fboundp 'ensime-type-at-point)
        (defalias 'ensime-type-at-point 'ensime-print-type-at-point))

      ;; key bindings
      (dolist (mode '(scala-mode))
        (dolist (prefix '(("mb" . "build")
                          ("mc" . "check")
                          ("md" . "debug")
                          ("mD" . "daemon")
                          ("mE" . "errors")
                          ("mg" . "goto")
                          ("mh" . "docs")
                          ("mi" . "inspect")
                          ("mr" . "refactor")
                          ("mt" . "test")
                          ("ms" . "repl")
                          ("my" . "yank")))
          (spacemacs/declare-prefix-for-mode mode (car prefix) (cdr prefix)))
        (spacemacs/set-leader-keys-for-major-mode mode
          "/"      'ensime-search
          "'"      'ensime-inf-switch

          "bc"     'ensime-sbt-do-compile
          "bC"     'ensime-sbt-do-clean
          "bi"     'ensime-sbt-switch
          "bp"     'ensime-sbt-do-package
          "br"     'ensime-sbt-do-run

          "ct"     'ensime-typecheck-current-buffer

          "dA"     'ensime-db-attach
          "db"     'ensime-db-set-break
          "dB"     'ensime-db-clear-break
          "dC"     'ensime-db-clear-all-breaks
          "dc"     'ensime-db-continue
          "di"     'ensime-db-inspect-value-at-point
          "dn"     'ensime-db-next
          "do"     'ensime-db-step-out
          "dq"     'ensime-db-quit
          "dr"     'ensime-db-run
          "ds"     'ensime-db-step
          "dt"     'ensime-db-backtrace

          "Df"     'ensime-reload-open-files
          "Dr"     'spacemacs/ensime-gen-and-restart
          "Ds"     'ensime

          "Ee"     'ensime-print-errors-at-point
          "Es"     'ensime-stacktrace-switch

          "gp"     'ensime-pop-find-definition-stack

          "hh"     'ensime-show-doc-for-symbol-at-point
          "hT"     'ensime-type-at-point-full-name
          "ht"     'ensime-type-at-point
          "hu"     'ensime-show-uses-of-symbol-at-point

          "ra"     'ensime-refactor-add-type-annotation
          "rd"     'ensime-refactor-diff-inline-local
          "rD"     'ensime-undo-peek
          "ri"     'ensime-refactor-diff-organize-imports
          "rm"     'ensime-refactor-diff-extract-method
          "rr"     'ensime-refactor-diff-rename
          "rt"     'ensime-import-type-at-point
          "rv"     'ensime-refactor-diff-extract-local

          "ta"     'ensime-sbt-do-test-dwim
          "tr"     'ensime-sbt-do-test-quick-dwim
          "tt"     'ensime-sbt-do-test-only-dwim

          "sa"     'ensime-inf-load-file
          "sb"     'ensime-inf-eval-buffer
          "sB"     'spacemacs/ensime-inf-eval-buffer-switch
          "si"     'ensime-inf-switch
          "sr"     'ensime-inf-eval-region
          "sR"     'spacemacs/ensime-inf-eval-region-switch

          "yT"     'spacemacs/ensime-yank-type-at-point-full-name
          "yt"     'spacemacs/ensime-yank-type-at-point

          "z"      'ensime-expand-selection-command))
      (evil-define-key 'insert ensime-mode-map
        (kbd ".") 'spacemacs/ensime-completing-dot
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack)
      (evil-define-key 'normal ensime-mode-map
        (kbd "M-.") 'ensime-edit-definition
        (kbd "M-,") 'ensime-pop-find-definition-stack)
      (evil-define-key 'normal ensime-popup-buffer-map
        (kbd "q") 'ensime-popup-buffer-quit-function)
      (evil-define-key 'normal ensime-inspector-mode-map
        (kbd "q") 'ensime-popup-buffer-quit-function)
      (evil-define-key 'normal ensime-refactor-info-map
        (kbd "q") 'spacemacs/ensime-refactor-cancel
        (kbd "c") 'spacemacs/ensime-refactor-accept
        (kbd "RET") 'spacemacs/ensime-refactor-accept)
      (evil-define-key 'normal ensime-compile-result-map
        (kbd "g") 'ensime-show-all-errors-and-warnings
        (kbd "TAB") 'forward-button
        (kbd "<backtab>") 'backward-button
        (kbd "M-n") 'forward-button
        (kbd "M-p") 'backward-button
        (kbd "n") 'forward-button
        (kbd "N") 'backward-button)
      (evil-define-key '(insert normal) ensime-search-mode-map
        (kbd "C-q") 'ensime-search-quit
        (kbd "C-j") 'ensime-search-next-match
        (kbd "C-k") 'ensime-search-prev-match
        (kbd "RET") 'ensime-search-choose-current-result
        (kbd "C-i") 'ensime-search-insert-import-of-current-result)

      ;; Enable Expand Region integration from Ensime.  Ignore load errors to
      ;; handle older Ensime versions gracefully.
      (when (configuration-layer/package-used-p 'expand-region)
        (require 'ensime-expand-region nil 'noerror)))))

(defun scala/post-init-flycheck ()
  (spacemacs/enable-flycheck 'scala-mode)
  ;; Don't use scala checker if ensime mode is active, since it provides
  ;; better error checking.
  (when (spacemacs//scala-backend-ensime-p)
    (with-eval-after-load 'flycheck
      (add-hook 'ensime-mode-hook 'spacemacs//scala-disable-flycheck-scala))))

(defun scala/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'scala-mode)
  (when (spacemacs//scala-backend-ensime-p)
    (add-hook 'scala-mode-hook #'spacemacs//scala-setup-ensime-flyspell)))

(defun scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :config
    ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
    ;; allows for using SPACE in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)
    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
    (setq sbt:program-options '("-Dsbt.supershell=false"))
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'scala-mode "mb" "sbt")
      (spacemacs/declare-prefix-for-mode 'scala-mode "mg" "goto")
      (spacemacs/set-leader-keys-for-major-mode 'scala-mode
        "b." 'sbt-hydra
        "bb" 'sbt-command))))

(defun scala/init-scala-mode ()
  (use-package scala-mode
    :defer t
    :init
    (progn
      (dolist (ext '(".cfe" ".cfs" ".si" ".gen" ".lock"))
        (add-to-list 'completion-ignored-extensions ext)))
    :config
    (progn
      ;; Ensure only one of metals and ensime is loaded
      (unless (spacemacs//scala-backend-ensime-p)
        (progn
          (fmakunbound 'ensime)
          (remove-hook 'after-change-functions 'ensime-after-change-function)
          (remove-hook 'window-configuration-change-hook
                       'ensime-show-left-margin-hook)))

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

(defun scala/pre-init-dap-mode ()
  (add-to-list 'spacemacs--dap-supported-modes 'scala-mode)
  (spacemacs//scala-setup-dap))

(defun scala/post-init-lsp-mode ()
  (when (spacemacs//scala-backend-metals-p)
    (spacemacs//scala-setup-metals)))

(defun scala/post-init-lsp-treemacs ()
  (when (spacemacs//scala-backend-metals-p)
    (spacemacs//scala-setup-treeview)))

(defun scala/post-init-ggtags ()
  (when scala-enable-gtags
    (add-hook 'scala-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)))

(defun scala/post-init-counsel-gtags ()
  (when scala-enable-gtags
    (spacemacs/counsel-gtags-define-keys-for-mode 'scala-mode)))

(defun scala/post-init-helm-gtags ()
  (when scala-enable-gtags
    (spacemacs/helm-gtags-define-keys-for-mode 'scala-mode)))
