;;; funcs.el --- Python Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
(defun spacemacs/python-annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import i?pu?db")
  (highlight-lines-matching-regexp "i?pu?db.set_trace()"))

(defun spacemacs/python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ((executable-find "pudb") "import pudb; pudb.set_trace()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert-string trace)
        (insert-string "\n")
        (python-indent-line)))))

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
(defun spacemacs/python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

(defun spacemacs//pyenv-mode-set-local-version ()
  "Set pyenv version from \".python-version\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".python-version")))
    (when root-path
      (let* ((file-path (expand-file-name ".python-version" root-path))
             (version
              (with-temp-buffer
                (insert-file-contents-literally file-path)
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))))
        (if (member version (pyenv-mode-versions))
            (pyenv-mode-set version)
          (message "pyenv: version `%s' is not installed (set by %s)"
                   version file-path))))))


;; Tests

(defun spacemacs//disable-semantic-idle-summary-mode ()
  "Disable semantic-idle-summary in Python mode.
Anaconda provides more useful information but can not do it properly
when this mode is enabled since the minibuffer is cleared all the time."
  (semantic-idle-summary-mode 0))

(defun spacemacs//python-imenu-create-index-use-semantic ()
  "Use semantic if the layer is enabled."
  (setq imenu-create-index-function 'semantic-create-imenu-index))

(defun spacemacs//python-get-main-testrunner ()
  "Get the main test runner."
  (if (listp python-test-runner) (car python-test-runner) python-test-runner))

(defun spacemacs//python-get-secondary-testrunner ()
  "Get the secondary test runner"
  (cdr (assoc (spacemacs//python-get-main-testrunner) '((pytest . nose)
                                             (nose . pytest)))))

(defun spacemacs//python-call-correct-test-function (arg funcalist)
  "Call a test function based on the chosen test framework.
ARG is the universal-argument which chooses between the main and
the secondary test runner. FUNCALIST is an alist of the function
to be called for each testrunner. "
  (let ((test-runner (if arg
                         (spacemacs//python-get-secondary-testrunner)
                       (spacemacs//python-get-main-testrunner))))
    (funcall (cdr (assoc test-runner funcalist)))))

(defun spacemacs/python-test-all (arg)
  "Run all tests."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-all)
                                           (nose . nosetests-all))))

(defun spacemacs/python-test-pdb-all (arg)
  "Run all tests in debug mode."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-pdb-all)
                                           (nose . nosetests-pdb-all))))

(defun spacemacs/python-test-module (arg)
  "Run all tests in the current module."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-module)
                                           (nose . nosetests-module))))

(defun spacemacs/python-test-pdb-module (arg)
  "Run all tests in the current module in debug mode."
  (interactive "P")
  (spacemacs//python-call-correct-test-function
   arg
   '((pytest . pytest-pdb-module)
     (nose . nosetests-pdb-module))))

(defun spacemacs/python-test-one (arg)
  "Run current test."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-one)
                                           (nose . nosetests-one))))

(defun spacemacs/python-test-pdb-one (arg)
  "Run current test in debug mode."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-pdb-one)
                                           (nose . nosetests-pdb-one))))

(defun spacemacs//bind-python-testing-keys ()
  "Bind the keys for testing in Python."
  (spacemacs/declare-prefix-for-mode 'python-mode "mt" "test")
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "tA" 'spacemacs/python-test-pdb-all
    "ta" 'spacemacs/python-test-all
    "tB" 'spacemacs/python-test-pdb-module
    "tb" 'spacemacs/python-test-module
    "tT" 'spacemacs/python-test-pdb-one
    "tt" 'spacemacs/python-test-one
    "tM" 'spacemacs/python-test-pdb-module
    "tm" 'spacemacs/python-test-module))
