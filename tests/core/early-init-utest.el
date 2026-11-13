;;; early-init-utest.el --- Early initialization tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(defvar dotspacemacs-directory)
(defvar user-lisp-directory)
(defvar package-enable-at-startup)

(defconst spacemacs-test-early-init-file
  (expand-file-name "../../early-init.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun spacemacs-test/early-init-directory (override xdg directories)
  (let ((process-environment (copy-sequence process-environment))
        (dotspacemacs-directory nil)
        (user-lisp-directory nil)
        (package-enable-at-startup t))
    (setenv "SPACEMACSDIR" override)
    (setenv "XDG_CONFIG_HOME" xdg)
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (path) (member path directories))))
      (load spacemacs-test-early-init-file nil t))
    (should (equal user-lisp-directory
                   (concat dotspacemacs-directory "user-lisp/")))
    dotspacemacs-directory))

(ert-deftest test-early-init-prefers-repository-local-directory ()
  (let ((local-directory
         (expand-file-name ".spacemacs.d/"
                           (file-name-directory spacemacs-test-early-init-file))))
    (should (equal
             (spacemacs-test/early-init-directory
              nil "/xdg" (list local-directory "/xdg/spacemacs/"))
             local-directory))))

(ert-deftest test-early-init-explicit-directory-overrides-repository-local ()
  (let ((local-directory
         (expand-file-name ".spacemacs.d/"
                           (file-name-directory spacemacs-test-early-init-file))))
    (should (equal
             (spacemacs-test/early-init-directory
              "/override" "/xdg"
              (list local-directory "/xdg/spacemacs/"))
             "/override/"))))

(ert-deftest test-early-init-falls-back-to-xdg-directory ()
  (should (equal
           (spacemacs-test/early-init-directory
            nil "/xdg" '("/xdg/spacemacs/"))
           "/xdg/spacemacs/")))

(ert-deftest test-early-init-keeps-home-fallback-without-existing-directories ()
  (dolist (xdg '(nil "/missing-xdg"))
    (should (equal
             (spacemacs-test/early-init-directory nil xdg nil)
             "~/.spacemacs.d/"))))
