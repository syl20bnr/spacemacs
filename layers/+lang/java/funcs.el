;;; packages.el --- Java functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//java-define-command-prefixes ()
  "Define command prefixes for java-mode."
  (setq java/key-binding-prefixes '(("me" . "errors")
                                    ("md" . "eclimd")
                                    ("mf" . "find")
                                    ("mg" . "goto")
                                    ("mr" . "refactor")
                                    ("mh" . "documentation")
                                    ("mm" . "maven")
                                    ("ma" . "ant")
                                    ("mp" . "project")
                                    ("mt" . "test")))
  (mapc (lambda(x) (spacemacs/declare-prefix-for-mode
                    'java-mode (car x) (cdr x)))
        java/key-binding-prefixes))

;; ensime

(autoload 'ensime-config-find-file "ensime-config")
(autoload 'ensime-config-find "ensime-config")
(autoload 'projectile-project-p "projectile")

(defun spacemacs//ensime-init (mode &optional enable-eldoc auto-start)
  (let ((hook (intern (format "%S-hook" mode)))
        (jump-handlers (intern (format "spacemacs-jump-handlers-%S" mode))))
    (spacemacs/register-repl 'ensime 'ensime-inf-switch "ensime")
    (when enable-eldoc
      (add-hook 'ensime-mode-hook 'spacemacs//ensime-enable-eldoc))
    (add-hook hook 'spacemacs//ensime-configure-flyspell)
    (add-hook hook 'spacemacs//ensime-configure)
    (when auto-start
      (add-hook mode 'spacemacs//ensime-maybe-start))
    (add-to-list jump-handlers 'ensime-edit-definition)))

(defun spacemacs//ensime-configure ()
  "Ensure the file exists before starting `ensime-mode'."
  (cond
   ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (ensime-mode +1))
   ((buffer-file-name)
    (add-hook 'after-save-hook (lambda () (ensime-mode +1)) nil t))))

(defun spacemacs//ensime-maybe-start ()
  (when (buffer-file-name)
    (let ((ensime-buffer (spacemacs//ensime-buffer-for-file (buffer-file-name)))
          (file (ensime-config-find-file (buffer-file-name)))
          (is-source-file (s-matches? (rx (or "/src/" "/test/")) (buffer-file-name))))

      (when (and is-source-file (null ensime-buffer))
        (noflet ((ensime-config-find (&rest _) file))
          (save-window-excursion
            (ensime)))))))

(defun spacemacs//ensime-buffer-for-file (file)
  "Find the Ensime server buffer corresponding to FILE."
  (let ((default-directory (file-name-directory file)))
    (-when-let (project-name (projectile-project-p))
      (--first (-when-let (bufname (buffer-name it))
                 (and (s-contains? "inferior-ensime-server" bufname)
                      (s-contains? (file-name-nondirectory project-name) bufname)))
               (buffer-list)))))

(defun spacemacs//ensime-enable-eldoc ()
  (setq-local eldoc-documentation-function
              (lambda ()
                (when (ensime-connected-p)
                  (ensime-print-type-at-point))))
  (eldoc-mode +1))

(defun spacemacs//ensime-flyspell-verify ()
  "Prevent common flyspell false positives in scala-mode."
  (and (flyspell-generic-progmode-verify)
       (not (s-matches? (rx bol (* space) "package") (current-line)))))

(defun spacemacs//ensime-configure-flyspell ()
  (setq-local flyspell-generic-check-word-predicate 'spacemacs//ensime-flyspell-verify))

;; key bindings

(defun spacemacs/ensime-configure-keybindings (mode)
  "Define Ensime key bindings for MODE."
  (dolist (prefix '(("mb" . "build")
                    ("mc" . "check")
                    ("md" . "debug")
                    ("me" . "errors")
                    ("mg" . "goto")
                    ("mh" . "docs")
                    ("mi" . "inspect")
                    ("mn" . "ensime")
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
    "cT"     'ensime-typecheck-all

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

    "ee"     'ensime-print-errors-at-point
    "el"     'ensime-show-all-errors-and-warnings
    "es"     'ensime-stacktrace-switch

    "gp"     'ensime-pop-find-definition-stack
    "gi"     'ensime-goto-impl
    "gt"     'ensime-goto-test

    "hh"     'ensime-show-doc-for-symbol-at-point
    "hT"     'ensime-type-at-point-full-name
    "ht"     'ensime-type-at-point
    "hu"     'ensime-show-uses-of-symbol-at-point

    "ii"     'ensime-inspect-type-at-point
    "iI"     'ensime-inspect-type-at-point-other-frame
    "ip"     'ensime-inspect-project-package

    "nF"     'ensime-reload-open-files
    "ns"     'ensime
    "nS"     'spacemacs/ensime-gen-and-restart

    "ra"     'ensime-refactor-add-type-annotation
    "rd"     'ensime-refactor-diff-inline-local
    "rD"     'ensime-undo-peek
    "rf"     'ensime-format-source
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

;; interactive functions

(defun spacemacs/ensime-gen-and-restart()
  "Regenerate `.ensime' file and restart the ensime server."
  (interactive)
  (progn
    (sbt-command ";ensimeConfig;ensimeConfigProject")
    (ensime-shutdown)
    (ensime)))

(defun spacemacs/ensime-inf-eval-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (ensime-inf-eval-buffer)
  (ensime-inf-switch)
  (evil-insert-state))

(defun spacemacs/ensime-inf-eval-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (ensime-inf-switch)
  (ensime-inf-eval-region start end)
  (evil-insert-state))

(defun spacemacs/ensime-refactor-accept ()
  (interactive)
  (funcall continue-refactor)
  (ensime-popup-buffer-quit-function))

(defun spacemacs/ensime-refactor-cancel ()
  (interactive)
  (funcall cancel-refactor)
  (ensime-popup-buffer-quit-function))

(defun spacemacs/ensime-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (company-abort)
  (insert ".")
  (company-complete))

(defun spacemacs/ensime-yank-type-at-point ()
  "Yank to kill ring and print short type name at point to the minibuffer."
  (interactive)
  (ensime-type-at-point t nil))

(defun spacemacs/ensime-yank-type-at-point-full-name ()
  "Yank to kill ring and print full type name at point to the minibuffer."
  (interactive)
  (ensime-type-at-point t t))



;; Completion

(defun spacemacs/java-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (spacemacs//java-delete-horizontal-space)
  (insert ".")
  (company-emacs-eclim 'interactive))

(defun spacemacs/java-completing-double-colon ()
  "Insert double colon and show company completions."
  (interactive "*")
  (spacemacs//java-delete-horizontal-space)
  (insert ":")
  (let ((curr (point)))
    (when (s-matches? (buffer-substring (- curr 2) (- curr 1)) ":")
      (company-emacs-eclim 'interactive))))


;; Maven

(defun spacemacs/java-maven-test ()
  (interactive)
  (eclim-maven-run "test"))

(defun spacemacs/java-maven-clean-install ()
  (interactive)
  (eclim-maven-run "clean install"))

(defun spacemacs/java-maven-install ()
  (interactive)
  (eclim-maven-run "install"))


;; Misc

(defun spacemacs//java-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))
