;;; funcs.el --- Python Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//python-backend ()
  "Returns selected backend."
  (if python-backend
      python-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'anaconda))))

(defun space-macs//python-formatter ()
  "Returns selected backend."
  (if python-formatter
      python-formatter
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'yapf))))

(defun space-macs//python-setup-backend ()
  "Conditionally setup python backend."
  (when python-pipenv-activate (pipenv-activate))
  (pcase (space-macs//python-backend)
    (`anaconda (space-macs//python-setup-anaconda))
    (`lsp (space-macs//python-setup-lsp))))

(defun space-macs//python-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//python-backend)
    (`anaconda (space-macs//python-setup-anaconda-company))))

(defun space-macs//python-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (space-macs//python-backend)
    (`lsp (space-macs//python-setup-lsp-dap))))

(defun space-macs//python-setup-eldoc ()
  "Conditionally setup eldoc based on backend."
  (pcase (space-macs//python-backend)
    ;; lsp setup eldoc on its own
    (`anaconda (space-macs//python-setup-anaconda-eldoc))))

;; anaconda

(defun space-macs//python-setup-anaconda ()
  "Setup anaconda backend."
  (anaconda-mode))

(defun space-macs//python-setup-anaconda-company ()
  "Setup anaconda auto-completion."
  (space-macs|add-company-backends
    :backends company-anaconda
    :modes python-mode
    :append-hooks nil
    :call-hooks t)
  (company-mode))

(defun space-macs//python-setup-anaconda-eldoc ()
  "Setup anaconda eldoc."
  (eldoc-mode)
  (when (configuration-layer/package-used-p 'anaconda-mode)
    (anaconda-eldoc-mode)))

(defun space-macs/anaconda-view-forward-and-push ()
  "Find next button and hit RET"
  (interactive)
  (forward-button 1)
  (call-interactively #'push-button))


;; lsp

(defun space-macs//python-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (cond ((eq python-lsp-server 'mspyls)  (require 'lsp-python-ms))
              ((eq python-lsp-server 'pyright) (require 'lsp-pyright)))
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun space-macs//python-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-python))


;; others

(defun space-macs//python-default ()
  "Defaut settings for python buffers"
  (setq mode-name "Python"
        tab-width python-tab-width
        fill-column python-fill-column)

  ;; since we changed the tab-width we need to manually call python-indent-guess-indent-offset here
  (when python-space-macs-indent-guess
    (python-indent-guess-indent-offset))

  (when (version< e-macs-version "24.5")
    ;; auto-indent on colon doesn't work well with if statement
    ;; should be fixed in 24.5 and above
    (setq electric-indent-chars (delq ?: electric-indent-chars)))
  (setq-local comment-inline-offset 2)
  (space-macs/python-annotate-pdb)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent))

;; from http://pedrokroger.net/2010/07/configuring-e-macs-as-a-python-ide-2/
(defun space-macs/python-annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "breakpoint()")
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)")
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()")
  (highlight-lines-matching-regexp "trepan.api.debug()"))

(defun space-macs/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around https://github.com/pyenv/pyenv-which-ext
"
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command)))
              (pyenv-version-names (split-string (string-trim (shell-command-to-string "pyenv version-name")) ":"))
              (executable nil)
              (i 0))
          (if (not (string-match "not found" pyenv-string))
              (while (and (not executable)
                          (< i (length pyenv-version-names)))
                (if (string-match (elt pyenv-version-names i) (string-trim pyenv-string))
                    (setq executable (string-trim pyenv-string)))
                (if (string-match (elt pyenv-version-names i) "system")
                    (setq executable (string-trim (executable-find command))))
                (setq i (1+ i))))
          executable))
    (executable-find command)))

(defun space-macs//python-setup-shell (&rest args)
  (if (space-macs/pyenv-executable-find "ipython")
      (progn (setq python-shell-interpreter "ipython")
             (if (version< (replace-regexp-in-string "[\r\n|\n]$" "" (shell-command-to-string (format "\"%s\" --version" (string-trim (space-macs/pyenv-executable-find "ipython"))))) "5")
                 (setq python-shell-interpreter-args "-i")
               (setq python-shell-interpreter-args "--simple-prompt -i")))
    (progn
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python"))))


(defun space-macs//python-setup-checkers (&rest args)
  (when (fboundp 'flycheck-set-checker-executable)
    (let ((pylint (space-macs/pyenv-executable-find "pylint"))
          (flake8 (space-macs/pyenv-executable-find "flake8")))
      (when pylint
        (flycheck-set-checker-executable "python-pylint" pylint))
      (when flake8
        (flycheck-set-checker-executable "python-flake8" flake8)))))

(defun space-macs/python-setup-everything (&rest args)
  (apply 'space-macs//python-setup-shell args)
  (apply 'space-macs//python-setup-checkers args))

(defun space-macs/python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((space-macs/pyenv-executable-find "trepan3k") "import trepan.api; trepan.api.debug()")
                     ((space-macs/pyenv-executable-find "wdb") "import wdb; wdb.set_trace()")
                     ((space-macs/pyenv-executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ((space-macs/pyenv-executable-find "pudb") "import pudb; pudb.set_trace()")
                     ((space-macs/pyenv-executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
                     ((space-macs/pyenv-executable-find "pudb3") "import pudb; pudb.set_trace()")
                     ((space-macs/pyenv-executable-find "python3.7") "breakpoint()")
                     ((space-macs/pyenv-executable-find "python3.8") "breakpoint()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line)))))

;; from https://www.snip2code.com/Snippet/127022/e-macs-auto-remove-unused-import-statemen
(defun space-macs/python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

(defun space-macs//pyenv-mode-set-local-version ()
  "Set pyenv version from \".python-version\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".python-version")))
    (when root-path
      (let* ((file-path (expand-file-name ".python-version" root-path))
             (version
              (with-temp-buffer
                (insert-file-contents-literally file-path)
                (nth 0 (split-string (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))))
        (if (member version (pyenv-mode-versions))
            (progn
              (setenv "VIRTUAL_ENV" version)
              (pyenv-mode-set version))
          (message "pyenv: version `%s' is not installed (set by %s)"
                   version file-path))))))

(defun space-macs//pyvenv-mode-set-local-virtualenv ()
  "Set pyvenv virtualenv from \".venv\" by looking in parent directories.
Handle \".venv\" being a virtualenv directory or a file specifying either
absolute or relative virtualenv path. Relative path is checked relative to
location of \".venv\" file, then relative to pyvenv-workon-home()."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory ".venv")))
    (when root-path
      (let ((file-path (expand-file-name ".venv" root-path)))
        (cond ((file-directory-p file-path)
               (pyvenv-activate file-path) (setq-local pyvenv-activate file-path))
              (t (let* ((virtualenv-path-in-file
                         (with-temp-buffer
                           (insert-file-contents-literally file-path)
                           (buffer-substring-no-properties (line-beginning-position)
                                                           (line-end-position))))
                        (virtualenv-abs-path
                         (if (file-name-absolute-p virtualenv-path-in-file)
                             virtualenv-path-in-file
                           (format "%s/%s" root-path virtualenv-path-in-file))))
                   (cond ((file-directory-p virtualenv-abs-path)
                          (pyvenv-activate virtualenv-abs-path)
                          (setq-local pyvenv-activate virtualenv-abs-path))
                         (t (pyvenv-workon virtualenv-path-in-file)
                            (setq-local pyvenv-workon virtualenv-path-in-file))))))))))

;; Tests

(defun space-macs//python-imenu-create-index-use-semantic-maybe ()
  "Use semantic if the layer is enabled."
  (setq imenu-create-index-function 'space-macs/python-imenu-create-index))

;; fix for issue #2569 (https://github.com/syl20bnr/space-macs/issues/2569) and
;; e-macs 24.5 and older. use `semantic-create-imenu-index' only when
;; `semantic-mode' is enabled, otherwise use `python-imenu-create-index'
(defun space-macs/python-imenu-create-index ()
  (if (bound-and-true-p semantic-mode)
      (semantic-create-imenu-index)
    (python-imenu-create-index)))

(defun space-macs//python-get-main-testrunner ()
  "Get the main test runner."
  (if (listp python-test-runner) (car python-test-runner) python-test-runner))

(defun space-macs//python-get-secondary-testrunner ()
  "Get the secondary test runner"
  (cdr (assoc (space-macs//python-get-main-testrunner) '((pytest . nose)
                                                        (nose . pytest)))))

(defun space-macs//python-call-correct-test-function (arg funcalist)
  "Call a test function based on the chosen test framework.
ARG is the universal-argument which chooses between the main and
the secondary test runner. FUNCALIST is an alist of the function
to be called for each testrunner. "
  (when python-save-before-test
    (save-buffer))
  (let* ((test-runner (if arg
                          (space-macs//python-get-secondary-testrunner)
                        (space-macs//python-get-main-testrunner)))
         (test-function (assq test-runner funcalist)))
    (if test-function
        (funcall (cdr (assoc test-runner funcalist)))
      (user-error "This test function is not available with the `%S' runner."
                  test-runner))))

(defun space-macs/python-test-last (arg)
  "Re-run the last test command"
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((pytest . pytest-again)
                                                      (nose . nosetests-again))))

(defun space-macs/python-test-last-failed (arg)
  "Re-run the tests that last failed."
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((pytest . pytest-last-failed))))

(defun space-macs/python-test-pdb-last-failed (arg)
  "Re-run the tests that last failed in debug mode."
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((pytest . pytest-pdb-last-failed))))

(defun space-macs/python-test-all (arg)
  "Run all tests."
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((pytest . pytest-all)
                                                      (nose . nosetests-all))))

(defun space-macs/python-test-pdb-all (arg)
  "Run all tests in debug mode."
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((pytest . pytest-pdb-all)
                                                      (nose . nosetests-pdb-all))))

(defun space-macs/python-test-module (arg)
  "Run all tests in the current module."
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((pytest . pytest-module)
                                                      (nose . nosetests-module))))

(defun space-macs/python-test-pdb-module (arg)
  "Run all tests in the current module in debug mode."
  (interactive "P")
  (space-macs//python-call-correct-test-function
   arg
   '((pytest . pytest-pdb-module)
     (nose . nosetests-pdb-module))))

(defun space-macs/python-test-suite (arg)
  "Run all tests in the current suite."
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((nose . nosetests-suite))))

(defun space-macs/python-test-pdb-suite (arg)
  "Run all tests in the current suite in debug mode."
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((nose . nosetests-pdb-suite))))

(defun space-macs/python-test-one (arg)
  "Run current test."
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((pytest . pytest-one)
                                                      (nose . nosetests-one))))

(defun space-macs/python-test-pdb-one (arg)
  "Run current test in debug mode."
  (interactive "P")
  (space-macs//python-call-correct-test-function arg '((pytest . pytest-pdb-one)
                                                      (nose . nosetests-pdb-one))))

(defun space-macs//bind-python-testing-keys ()
  "Bind the keys for testing in Python."
  (space-macs/declare-prefix-for-mode 'python-mode "mt" "test")
  (space-macs/set-leader-keys-for-major-mode 'python-mode
    "tA" 'space-macs/python-test-pdb-all
    "ta" 'space-macs/python-test-all
    "tB" 'space-macs/python-test-pdb-module
    "tb" 'space-macs/python-test-module
    "tl" 'space-macs/python-test-last
    "tf" 'space-macs/python-test-last-failed
    "tF" 'space-macs/python-test-pdb-last-failed
    "tT" 'space-macs/python-test-pdb-one
    "tt" 'space-macs/python-test-one
    "tM" 'space-macs/python-test-pdb-module
    "tm" 'space-macs/python-test-module
    "tS" 'space-macs/python-test-pdb-suite
    "ts" 'space-macs/python-test-suite))

(defun space-macs//python-sort-imports ()
  ;; py-isort-before-save checks the major mode as well, however we can prevent
  ;; it from loading the package unnecessarily by doing our own check
  (when (and python-sort-imports-on-save
             (derived-mode-p 'python-mode))
    (py-isort-before-save)))


;; Formatters

(defun space-macs//bind-python-formatter-keys ()
  "Bind the python formatter keys.
Bind formatter to '==' for LSP and '='for all other backends."
  (space-macs/set-leader-keys-for-major-mode 'python-mode
    (if (eq (space-macs//python-backend) 'lsp)
        "=="
      "=") 'space-macs/python-format-buffer))

(defun space-macs/python-format-buffer ()
  "Bind possible python formatters."
  (interactive)
  (pcase (space-macs//python-formatter)
    (`yapf (yapfify-buffer))
    (`black (blacken-buffer))
    (`lsp (lsp-format-buffer))
    (code (message "Unknown formatter: %S" code))))


;; REPL

(defun space-macs/python-shell-send-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-buffer)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun space-macs/python-shell-send-buffer ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-buffer)))

(defun space-macs/python-shell-send-defun-switch ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-defun nil)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun space-macs/python-shell-send-defun ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-defun nil)))

(defun space-macs/python-shell-send-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (let ((python-mode-hook nil))
    (python-shell-send-region start end)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun space-macs/python-shell-send-region (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (let ((python-mode-hook nil))
    (python-shell-send-region start end)))

(defun space-macs/python-shell-send-line ()
	"Send the current line to shell"
	(interactive)
	(let ((python-mode-hook nil)
	       (start (point-at-bol))
	       (end (point-at-eol)))
	      (python-shell-send-region start end)))

(defun space-macs/python-shell-send-statement ()
	"Send the current statement to shell, same as `python-shell-send-statement' in e-macs27."
	(interactive)
  (if (fboundp 'python-shell-send-statement)
      (call-interactively #'python-shell-send-statement)
    (if (region-active-p)
        (call-interactively #'python-shell-send-region)
      (let ((python-mode-hook nil))
	      (python-shell-send-region
         (save-excursion (python-nav-beginning-of-statement))
         (save-excursion (python-nav-end-of-statement)))))))

(defun space-macs/python-shell-send-statement-switch ()
  "Send statement to shell and switch to it in insert mode."
  (interactive)
  (call-interactively #'space-macs/python-shell-send-statement)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun space-macs/python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             ;; `run-python' has different return values and different
             ;; errors in different e-macs versions. In 24.4, it throws an
             ;; error when the process didn't start, but in 25.1 it
             ;; doesn't throw an error, so we demote errors here and
             ;; check the process later
             (with-demoted-errors "Error: %S"
               ;; in e-macs 24.5 and 24.4, `run-python' doesn't return the
               ;; shell process
               (call-interactively #'run-python)
               (python-shell-get-process)))))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))

(defun space-macs/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (space-macs/pyenv-executable-find python-shell-interpreter)
                                 (shell-quote-argument (file-name-nondirectory buffer-file-name)))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))

(defun space-macs/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
 `insert state'."
  (interactive "P")
  (space-macs/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-insert-state))

;; fix for issue #2569 (https://github.com/syl20bnr/space-macs/issues/2569)
(when (version< e-macs-version "25")
  (advice-add 'wisent-python-default-setup :after
              #'space-macs//python-imenu-create-index-use-semantic-maybe))

(defun space-macs//bind-python-repl-keys ()
  "Bind the keys for testing in Python."
  (space-macs/declare-prefix-for-mode 'inferior-python-mode "mv" "virtualenv")
  (space-macs/set-leader-keys-for-major-mode 'inferior-python-mode
    "c" 'comint-clear-buffer
    "r" 'pyvenv-restart-python
    "vw" 'pyvenv-workon))


