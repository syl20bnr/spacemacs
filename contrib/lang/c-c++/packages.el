;;; packages.el --- C/C++ Layer packages File for Spacemacs
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

(defvar c-c++-packages
  '(
    cc-mode
    cmake-mode
    company
    company-c-headers
    flycheck
    stickyfunc-enhance
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(unless (version< emacs-version "24.4")
  (add-to-list 'c-c++-packages 'srefactor))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    (progn
      (require 'compile)
      (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
      (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
      (semantic-mode 1)
      (c-toggle-auto-newline 1)
      (setq srecode-map-save-file (concat spacemacs-cache-directory "srecode-map.el"))
      (setq semanticdb-default-save-directory (concat spacemacs-cache-directory "semanticdb/"))
      (evil-leader/set-key-for-mode 'c-mode
        "mga" 'projectile-find-other-file
        "mgA" 'projectile-find-other-file-other-window)
      (evil-leader/set-key-for-mode 'c++-mode
        "mga" 'projectile-find-other-file
        "mgA" 'projectile-find-other-file-other-window))))

(defun c-c++/init-cmake-mode ()
  (use-package cmake-mode
    :defer t
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init
    (progn
      (spacemacs|add-mode-company-backend cmake-mode company-cmake))))

(defun c-c++/init-company ()
  ;; .clang_complete file loading
  ;; Sets the arguments for company-clang based on a project-specific text file.

  ;; START Based on the Sarcasm/irony-mode compilation database code.
  (defun company-mode/find-clang-complete-file ()
    (when buffer-file-name
      (let ((dir (locate-dominating-file buffer-file-name ".clang_complete")))
        (when dir
          (concat (file-name-as-directory dir) ".clang_complete")))))

  (defun company-mode/load-clang-complete-file (cc-file)
    "Load the flags from CC-FILE, one flag per line."
    (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
          (case-fold-search nil)
          compile-flags)
      (with-temp-buffer
        (insert-file-contents cc-file)
        ;; Replace relative paths with absolute paths (by @trishume)
        ;; (goto-char (point-min))
        (while (re-search-forward "\\(-I\\|-isystem\n\\)\\(\\S-\\)" nil t)
          (replace-match (format "%s%s" (match-string 1)
                                 (expand-file-name (match-string 2) invocation-dir))))
        ;; Turn lines into a list
        (setq compile-flags
              ;; remove whitespaces at the end of each line, if any
              (mapcar #'(lambda (line)
                          (if (string-match "[ \t]+$" line)
                              (replace-match "" t t line)
                            line))
                      (split-string (buffer-string) "\n" t))))
      compile-flags))
  ;; END Back to things written by @trishume

  (defun company-mode/more-than-prefix-guesser ()
    (unless company-clang-arguments
      (let* ((cc-file (company-mode/find-clang-complete-file))
             (flags (if cc-file (company-mode/load-clang-complete-file cc-file) '())))
        (setq-local company-clang-arguments flags)
        (setq flycheck-clang-args flags)))
    (company-clang-guess-prefix))

  (setq company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser))

(defun c-c++/init-company-c-headers ()
  (use-package company-c-headers
    :if (configuration-layer/package-declaredp 'company)
    :defer t
    :init
    (progn
      (spacemacs|add-mode-company-backend c-mode company-c-headers)
      (spacemacs|add-mode-company-backend c++-mode company-c-headers))))

(defun c-c++/init-flycheck ()
  (add-to-hooks 'flycheck-mode '(c-mode-hook c++-mode-hook)))

(defun c-c++/init-srefactor ()
  (use-package srefactor
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'c-mode
        "mr" 'srefactor-refactor-at-point)
      (evil-leader/set-key-for-mode 'c++-mode
        "mr" 'srefactor-refactor-at-point))))

(defun c-c++/init-stickyfunc-enhance ()
  (use-package stickyfunc-enhance
    :defer t
    :init
    (progn
      (defun spacemacs/lazy-load-stickyfunc-enhance ()
        "Lazy load the package."
        (require 'stickyfunc-enhance))
      (add-to-hooks 'spacemacs/lazy-load-stickyfunc-enhance
                    '(c-mode-hook c++-mode-hook)))))
