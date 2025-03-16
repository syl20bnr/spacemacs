;;; funcs.el --- Python Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun spacemacs//poetry-activate ()
  "Attempt to activate Poetry only if its configuration file is found."
  (let ((root-path (locate-dominating-file default-directory "pyproject.toml")))
    (when root-path
      (message "Poetry configuration file found. Activating virtual environment.")
      (poetry-venv-workon))))


(defun spacemacs//python-setup-backend ()
  "Conditionally setup python backend."
  (when python-pipenv-activate (pipenv-activate))
  (when python-poetry-activate (spacemacs//poetry-activate))
  (pcase python-backend
    ('anaconda (spacemacs//python-setup-anaconda))
    ('lsp (spacemacs//python-setup-lsp))))

(defun spacemacs//python-setup-company ()
  "Conditionally setup company based on backend."
  (when (eq python-backend 'anaconda)
    (spacemacs//python-setup-anaconda-company)))

(defun spacemacs//python-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (when (eq python-backend 'lsp)
    (spacemacs//python-setup-lsp-dap)))

(defun spacemacs//python-setup-eldoc ()
  "Conditionally setup eldoc based on backend."
  (when (eq python-backend 'anaconda)
    ;; lsp setup eldoc on its own
    (spacemacs//python-setup-anaconda-eldoc)))


;; anaconda

(defun spacemacs//python-setup-anaconda ()
  "Setup anaconda backend."
  (anaconda-mode))

(defun spacemacs//python-setup-anaconda-company ()
  "Setup anaconda auto-completion."
  (spacemacs|add-company-backends
    :backends company-anaconda
    :modes python-mode
    :append-hooks nil
    :call-hooks t)
  (company-mode))

(defun spacemacs//python-setup-anaconda-eldoc ()
  "Setup anaconda eldoc."
  (eldoc-mode)
  (when (configuration-layer/package-used-p 'anaconda-mode)
    (anaconda-eldoc-mode)))

(defun spacemacs/anaconda-view-forward-and-push ()
  "Find next button and hit RET"
  (interactive)
  (forward-button 1)
  (call-interactively #'push-button))


;; lsp

(defun spacemacs//python-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (require (pcase python-lsp-server
                   ('pylsp 'lsp-pylsp)
                   ('pyright 'lsp-pyright)
                   (x (user-error "Unknown value for `python-lsp-server': %s" x))))
        (lsp-deferred))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//python-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-python))


;; others

(defun spacemacs//python-default ()
  "Default settings for python buffers"
  (setq mode-name "Python"
        tab-width python-tab-width
        fill-column python-fill-column)

  ;; since we changed the tab-width we need to manually call python-indent-guess-indent-offset here
  (when python-spacemacs-indent-guess
    (python-indent-guess-indent-offset))

  (setq-local comment-inline-offset 2)
  (spacemacs/python-annotate-pdb)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent))

;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
(defun spacemacs/python-annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "breakpoint()")
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)")
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()")
  (highlight-lines-matching-regexp "trepan.api.debug()"))

(defun spacemacs/pyenv-executable-find (commands)
  "Find executable taking pyenv shims into account.

Return the first executable in COMMANDS whose path was found.  If
the pyenv was configured with \"system\" then the system
executable will be included, otherwise the system executable
will be ignored.

COMMANDS may also be a single string, for backwards
compatibility."
  (unless (listp commands)
    (setq commands (list commands)))
  (if (or (bound-and-true-p pyvenv-virtual-env) ; in virtualenv
          (not (executable-find "pyenv")))      ; or no pyenv
      (cl-some (lambda (dir)
                 (let ((exec-path (list dir)))
                   (cl-find-if 'executable-find commands)))
               exec-path)

    (let ((pyenv-vers (split-string (string-trim (shell-command-to-string "pyenv version-name")) ":")))
      (cl-some
       (lambda (cmd)
         (when-let* ((pyenv-cmd (string-trim (shell-command-to-string (concat "pyenv which " cmd))))
                     ((not (string-match "not found" pyenv-cmd))))
           (cl-some
            (lambda (ver)
              (cond ((string-match ver pyenv-cmd) pyenv-cmd)
                    ((string-match ver "system") (and (executable-find cmd) cmd))))
            pyenv-vers)))
       commands))))

(defun spacemacs//python-setup-shell (&optional root-dir)
  "Setup the python shell if no customer prefered value or the value be cleaned.
ROOT-DIR should be the directory path for the environment, `nil' for clean up."
  (when (or (not (bound-and-true-p python-shell-interpreter))
            (equal python-shell-interpreter spacemacs--python-shell-interpreter-origin))
    (if-let* ((default-directory root-dir))
        (let* ((pyshell (or (spacemacs/pyenv-executable-find
                             '("ipython3" "ipython" "python3" "python2" "python"))
                            "python3"))
               (ipythonp (string-search "ipython" (file-name-nondirectory pyshell))))
          (setq-local python-shell-interpreter pyshell
                      python-shell-interpreter-args (if ipythonp "-i --simple-prompt" "-i")))
      ;; args is nil, clean up the variables
      (setq-local python-shell-interpreter nil
                  python-shell-interpreter-args nil))))

(defun spacemacs//python-setup-checkers (&optional root-dir)
  "Setup the checkers.
ROOT-DIR should be the path for the environemnt, `nil' for clean up"
  (when (fboundp 'flycheck-set-checker-executable)
    (dolist (x '("pylint" "flake8"))
      (if-let* ((default-directory root-dir))
          (when-let* ((exe (spacemacs/pyenv-executable-find (list x))))
            (flycheck-set-checker-executable (concat "python-" x) exe))
        ;; else root-dir is nil
        (set (flycheck-checker-executable-variable (concat "python-" x)) nil)))))

(defun spacemacs/python-setup-everything (&optional root-dir)
  (funcall 'spacemacs//python-setup-shell root-dir)
  (funcall 'spacemacs//python-setup-checkers root-dir))

(defun spacemacs/python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let* ((exe (spacemacs/pyenv-executable-find '("trepan3k" "wdb" "ipdb3" "pudb3" "ipdb" "pudb" "python3")))
         (trace (pcase (and exe (file-name-nondirectory exe))
                  ("trepan3k"          "import trepan.api; trepan.api.debug()")
                  ("wdb"               "import wdb; wdb.set_trace()")
                  ((or "ipdb" "ipdb3") "import ipdb; ipdb.set_trace()")
                  ((or "pudb" "pudb3") "import pudb; pudb.set_trace()")
                  ("python3"           "breakpoint()") ; not consider the python3.6 or lower
                  (_ "import pdb; pdb.set_trace()"))))
    (unless (cl-some
             (lambda (bounds)
               (when-let* ((beg (car-safe bounds))
                           (end (cdr-safe bounds))
                           ((string-search trace (buffer-substring beg end))))
                 (kill-region beg end)
                 (back-to-indentation)
                 ;; return t to discontinue
                 t))
             (list (bounds-of-thing-at-point 'line)               ; current line
                   (save-excursion (and (zerop (forward-line -1)) ; previous line
                                        (bounds-of-thing-at-point 'line)))))
      ;; insert the instruction
      (back-to-indentation)
      (insert trace ?\n)
      (python-indent-line))))

;; from https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
(defun spacemacs/python-remove-unused-imports ()
  "Use Autoflake to remove unused imports.
Equivalent to: autoflake --remove-all-unused-imports --in-place <FILE>"
  (interactive)
  (if (executable-find "autoflake")
      (if (not (eql 0
                    (shell-command (format "autoflake --remove-all-unused-imports --in-place %s"
                                           (shell-quote-argument (buffer-file-name))))))
          (pop-to-buffer shell-command-buffer-name)
        (revert-buffer t t t))
    (user-error "Cannot find autoflake executable")))

(defun spacemacs//pyenv-mode-set-local-version ()
  "Set pyenv version from \".python-version\" by looking in parent directories."
  (interactive)
  (when-let* ((root-path (locate-dominating-file default-directory
                                                 ".python-version"))
              (file-path (expand-file-name ".python-version" root-path))
              (version
               (with-temp-buffer
                 (insert-file-contents-literally file-path)
                 (nth 0 (split-string (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position)))))))
    (cond ((member version (pyenv-mode-versions))
           (setenv "VIRTUAL_ENV" version)
           (pyenv-mode-set version))
          (t (message "pyenv: version `%s' is not installed (set by %s)"
                      version file-path)))))

(defun spacemacs//pyvenv-mode-set-local-virtualenv ()
  "Set pyvenv virtualenv from \".venv\" by looking in parent directories.
Handle \".venv\" being a virtualenv directory or a file specifying either
absolute or relative virtualenv path. Relative path is checked relative to
location of \".venv\" file, then relative to pyvenv-workon-home()."
  (interactive)
  (when-let* ((root-path (locate-dominating-file default-directory ".venv"))
              (file-path (expand-file-name ".venv" root-path)))
    (cond ((file-directory-p file-path)
           (pyvenv-activate file-path)
           (setq-local pyvenv-activate file-path))
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
                        (setq-local pyvenv-workon virtualenv-path-in-file))))))))


;; Tests

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
  (when python-save-before-test
    (save-buffer))
  (let* ((test-runner (if arg
                          (spacemacs//python-get-secondary-testrunner)
                        (spacemacs//python-get-main-testrunner)))
         (test-function (assq test-runner funcalist)))
    (if test-function
        (funcall (cdr (assoc test-runner funcalist)))
      (user-error "This test function is not available with the `%S' runner."
                  test-runner))))

(defun spacemacs/python-test-last (arg)
  "Re-run the last test command"
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-again)
                                                      (nose . nosetests-again))))

(defun spacemacs/python-test-last-failed (arg)
  "Re-run the tests that last failed."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-last-failed))))

(defun spacemacs/python-test-pdb-last-failed (arg)
  "Re-run the tests that last failed in debug mode."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((pytest . pytest-pdb-last-failed))))

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

(defun spacemacs/python-test-suite (arg)
  "Run all tests in the current suite."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((nose . nosetests-suite))))

(defun spacemacs/python-test-pdb-suite (arg)
  "Run all tests in the current suite in debug mode."
  (interactive "P")
  (spacemacs//python-call-correct-test-function arg '((nose . nosetests-pdb-suite))))

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
    "tl" 'spacemacs/python-test-last
    "tf" 'spacemacs/python-test-last-failed
    "tF" 'spacemacs/python-test-pdb-last-failed
    "tT" 'spacemacs/python-test-pdb-one
    "tt" 'spacemacs/python-test-one
    "tM" 'spacemacs/python-test-pdb-module
    "tm" 'spacemacs/python-test-module
    "tS" 'spacemacs/python-test-pdb-suite
    "ts" 'spacemacs/python-test-suite))

(defun spacemacs//python-sort-imports ()
  ;; py-isort-before-save checks the major mode as well, however we can prevent
  ;; it from loading the package unnecessarily by doing our own check
  (when (and python-sort-imports-on-save
             (derived-mode-p 'python-mode))
    (py-isort-before-save)))


;; Formatters

(defun spacemacs//bind-python-formatter-keys ()
  "Bind the python formatter keys.
Bind formatter to '==' for LSP and '='for all other backends."
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    (if (eq python-backend 'lsp)
        "=="
      "=")
    'spacemacs/python-format-buffer))

(defun spacemacs/python-format-buffer ()
  "Bind possible python formatters."
  (interactive)
  (pcase python-formatter
    ('yapf (yapfify-buffer))
    ('black (blacken-buffer))
    ('lsp (lsp-format-buffer))
    (code (message "Unknown formatter: %S" code))))

(defun spacemacs//python-lsp-set-up-format-on-save ()
  (when (and python-format-on-save
             (eq python-formatter 'lsp))
    (add-hook
     'python-mode-hook
     'spacemacs//python-lsp-set-up-format-on-save-local)))

(defun spacemacs//python-lsp-set-up-format-on-save-local ()
  (add-hook 'before-save-hook 'spacemacs//python-lsp-format-on-save nil t))

(defun spacemacs//python-lsp-format-on-save ()
  (condition-case err
      (when (and python-format-on-save
                 (eq python-formatter 'lsp))
        (lsp-format-buffer))
    (lsp-capability-not-supported
     (display-warning
      '(spacemacs python)
      "Configuration error: `python-formatter' is `lsp', no active workspace supports textDocument/formatting"
      :error))))



;; REPL
(defun spacemacs/python-shell-send-block (&optional arg)
  "Send the block under cursor to shell. If optional argument ARG is non-nil
(interactively, the prefix argument), send the block body with its header."
  (interactive "P")
  (if (fboundp 'python-shell-send-block)
      (let ((python-mode-hook nil))
        (call-interactively #'python-shell-send-block))
    (let ((python-mode-hook nil)
          (beg (save-excursion
                 (when (python-nav-beginning-of-block)
                   (if arg
                       (beginning-of-line)
                     (python-nav-end-of-statement)
                     (beginning-of-line 2)))
                 (point-marker)))
          (end (save-excursion (python-nav-end-of-block)))
          (python-indent-guess-indent-offset-verbose nil))
      (if (and beg end)
          (python-shell-send-region beg end nil msg t)
        (user-error "Can't get code block from current position.")))))

(defun spacemacs/python-shell-send-block-switch (&optional arg)
  "Send block to shell and switch to it in insert mode."
  (interactive "P")
  (call-interactively #'spacemacs/python-shell-send-block)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun spacemacs/python-shell-send-buffer-switch ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-buffer)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun spacemacs/python-shell-send-buffer ()
  "Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-buffer)))

(defun spacemacs/python-shell-send-defun-switch ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-defun nil)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun spacemacs/python-shell-send-defun ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-send-defun nil)))

(defun spacemacs/python-shell-send-region-switch (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (let ((python-mode-hook nil))
    (python-shell-send-region start end)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun spacemacs/python-shell-send-region (start end)
  "Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (let ((python-mode-hook nil))
    (python-shell-send-region start end)))

(defun spacemacs/python-shell-send-line ()
  "Send the current line to shell"
  (interactive)
  (let ((python-mode-hook nil)
        (start (point-at-bol))
        (end (point-at-eol)))
    (python-shell-send-region start end)))

(defun spacemacs/python-shell-send-statement ()
  "Send the statement under cursor to shell."
  (interactive)
  (let ((python-mode-hook nil))
    (call-interactively #'python-shell-send-statement)))

(defun spacemacs/python-shell-send-statement-switch ()
  "Send statement to shell and switch to it in insert mode."
  (interactive)
  (call-interactively #'spacemacs/python-shell-send-statement)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun spacemacs/python-shell-send-with-output(start end)
  "Send region content to shell and show output in comint buffer.
If region is not active then send line."
  (interactive "r")
  (let ((python-mode-hook nil)
        (process-buffer (python-shell-get-process))
        (line-start (point-at-bol))
        (line-end (point-at-eol)))
    (if (region-active-p)
        (comint-send-region process-buffer start end)
      (comint-send-region process-buffer line-start line-end))
    (comint-simple-send process-buffer "\r")))

(defun spacemacs/python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (if-let* ((shell-process (or (python-shell-get-process)
                               (call-interactively #'run-python))))
      (progn
        (pop-to-buffer (process-buffer shell-process))
        (evil-insert-state))
    (error "Failed to start python shell properly")))

(defun spacemacs/python-shell-restart ()
  "Restart python shell."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-restart)))

(defun spacemacs/python-shell-restart-switch ()
  "Restart python shell and switch to it in insert mode."
  (interactive)
  (let ((python-mode-hook nil))
    (python-shell-restart)
    (python-shell-switch-to-shell)
    (evil-insert-state)))

(defun spacemacs/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (spacemacs/pyenv-executable-find (list python-shell-interpreter))
                                 (shell-quote-argument (file-name-nondirectory buffer-file-name)))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))

(defun spacemacs/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
 `insert state'."
  (interactive "P")
  (spacemacs/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-insert-state))

(defun spacemacs//bind-python-repl-keys ()
  "Bind the keys for testing in Python."
  (spacemacs/declare-prefix-for-mode 'inferior-python-mode "mv" "virtualenv")
  (spacemacs/set-leader-keys-for-major-mode 'inferior-python-mode
    "c" 'comint-clear-buffer
    "r" 'pyvenv-restart-python
    "vw" 'pyvenv-workon))
