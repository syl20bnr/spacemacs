;;; packages.el --- Python Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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


(defconst python-packages
  '(
    blacken
    code-cells
    company
    counsel-gtags
    cython-mode
    dap-mode
    eldoc
    evil-matchit
    flycheck
    ggtags
    helm-cscope
    (helm-pydoc :requires helm)
    importmagic
    live-py-mode
    (nose :location (recipe :fetcher github :repo "syl20bnr/nose.el"))
    org
    pip-requirements
    pipenv
    poetry
    pippel
    py-isort
    pydoc
    pyenv-mode
    (pylookup :location (recipe :fetcher local))
    pytest
    (python :location built-in)
    pyvenv
    semantic
    sphinx-doc
    smartparens
    stickyfunc-enhance
    xcscope
    window-purpose
    yapfify
    ;; packages for anaconda backend
    anaconda-mode
    (company-anaconda :requires company)
    ;; packages for Microsoft LSP backend
    (lsp-python-ms :requires lsp-mode)
    ;; packages for Microsoft's pyright language server
    (lsp-pyright :requires lsp-mode)))

(defun python/init-anaconda-mode ()
  (use-package anaconda-mode
    :if (eq python-backend 'anaconda)
    :defer t
    :init
    (setq anaconda-mode-installation-directory
          (concat spacemacs-cache-directory "anaconda-mode"))
    :config
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "hh" 'anaconda-mode-show-doc
      "ga" 'anaconda-mode-find-assignments
      "gb" 'xref-pop-marker-stack
      "gu" 'anaconda-mode-find-references)
    ;; new anaconda-mode (2018-06-03) removed `anaconda-view-mode-map' in
    ;; favor of xref. Eventually we need to remove this part.
    (when (boundp 'anaconda-view-mode-map)
      (evilified-state-evilify-map anaconda-view-mode-map
        :mode anaconda-view-mode
        :bindings
        (kbd "q") 'quit-window
        (kbd "C-j") 'next-error-no-select
        (kbd "C-k") 'previous-error-no-select
        (kbd "RET") 'spacemacs/anaconda-view-forward-and-push))
    (spacemacs|hide-lighter anaconda-mode)
    (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
      (evil--jumps-push))
    (add-to-list 'spacemacs-jump-handlers-python-mode
                 '(anaconda-mode-find-definitions :async t))))

(defun python/init-code-cells ()
  (use-package code-cells
    :if (not (configuration-layer/layer-used-p 'ipython-notebook))
    :defer t
    :commands (code-cells-mode)
    :init (add-hook 'python-mode-hook 'code-cells-mode)
    :config (spacemacs/set-leader-keys-for-minor-mode 'code-cells-mode
            "gB" 'code-cells-backward-cell
            "gF" 'code-cells-forward-cell
            "sc" 'code-cells-eval
            "sa" 'code-cells-eval-above)))

(defun python/post-init-company ()
  ;; backend specific
  (spacemacs/add-local-var-hook #'spacemacs//python-setup-company
                                :major-mode 'python-mode)
  (spacemacs|add-company-backends
    :backends (company-files company-capf)
    :modes inferior-python-mode
    :variables
    company-minimum-prefix-length 0
    company-idle-delay 0.5)
  (when (configuration-layer/package-used-p 'pip-requirements)
    (spacemacs|add-company-backends
      :backends company-capf
      :modes pip-requirements-mode)))

(defun python/init-company-anaconda ()
  (use-package company-anaconda
    :if (eq python-backend 'anaconda)
    :defer t))
;; see `spacemacs//python-setup-anaconda-company'

(defun python/init-blacken ()
  (use-package blacken
    :defer t
    :init
    (spacemacs//bind-python-formatter-keys)
    (when (and python-format-on-save
               (eq 'black python-formatter))
      (add-hook 'python-mode-hook 'blacken-mode))
    :config (spacemacs|hide-lighter blacken-mode)))

(defun python/init-cython-mode ()
  (use-package cython-mode
    :defer t
    :config
    (when (eq python-backend 'anaconda)
      (spacemacs/set-leader-keys-for-major-mode 'cython-mode
        "hh" 'anaconda-mode-show-doc
        "gu" 'anaconda-mode-find-references))))

(defun python/pre-init-dap-mode ()
  (when (eq python-backend 'lsp)
    (add-to-list 'spacemacs--dap-supported-modes 'python-mode))
  (spacemacs/add-local-var-hook #'spacemacs//python-setup-dap
                                :major-mode 'python-mode))

(defun python/post-init-eldoc ()
  (spacemacs/add-local-var-hook #'spacemacs//python-setup-eldoc
                                :major-mode 'python-mode))

(defun python/post-init-evil-matchit ()
  (add-hook `python-mode-hook `turn-on-evil-matchit-mode))

(defun python/post-init-flycheck ()
  (spacemacs/enable-flycheck 'python-mode))

(defun python/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (spacemacs/setup-helm-cscope 'python-mode)))

(defun python/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'python-mode))

(defun python/post-init-ggtags ()
  (spacemacs/add-local-var-hook #'spacemacs/ggtags-mode-enable
                                :major-mode 'python-mode))

(defun python/init-helm-pydoc ()
  (use-package helm-pydoc
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "hd" 'helm-pydoc)))

(defun python/init-importmagic ()
  (use-package importmagic
    :defer t
    :init
    (add-hook 'python-mode-hook 'importmagic-mode)
    (spacemacs|diminish importmagic-mode " ⓘ" " [i]")
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "rf" 'importmagic-fix-symbol-at-point)))

(defun python/init-live-py-mode ()
  (use-package live-py-mode
    :defer t
    :commands live-py-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "l" 'live-py-mode)))

(defun python/init-nose ()
  (use-package nose
    :commands (nosetests-one
               nosetests-pdb-one
               nosetests-all
               nosetests-pdb-all
               nosetests-module
               nosetests-pdb-module
               nosetests-suite
               nosetests-pdb-suite)
    :init (spacemacs//bind-python-testing-keys)
    :config
    (add-to-list 'nose-project-root-files "setup.cfg")
    (setq nose-use-verbose nil)))

(defun python/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(python . t))))

(defun python/init-pipenv ()
  (use-package pipenv
    :defer t
    :init
    ;; (spacemacs|spacebind
    ;;  :minor
    ;;  (pipenv-mode
    ;;   "Pipenv key bindings"
    ;;   ("V" "Environment"
    ;;    ("'" pipenv-shell "Start a shell")
    ;;    ("a" pipenv-activate "Activate Python version")
    ;;    ("d" pipenv-deactivate "Deactivate Python version")
    ;;    ("i" pipenv-install "Install packages...")
    ;;    ("o" pipenv-open "View module...")
    ;;    ("u" pipenv-uninstall "Uninstall package..."))))
    ))

(defun python/pre-init-poetry ()
  (add-to-list 'spacemacs--python-poetry-modes 'python-mode))
(defun python/init-poetry ()
  (use-package poetry
    :defer t
    :commands (poetry-venv-toggle
               poetry-tracking-mode)
    :init
    (dolist (m spacemacs--python-poetry-modes)
      (eval
       `(spacemacs|spacebind
         :major
         (,m
          "Poetry key bindings"
          ("V" "Environment"
           ("P" "Poetry"
            ("d" poetry-venv-deactivate "Deactivate virtualenv")
            ("t" poetry-venv-toggle "Toggle virtualenv")
            ("w" poetry-venv-workon "Activate virtualenv")))))))))

(defun python/init-pip-requirements ()
  (use-package pip-requirements
    :defer t))

(defun python/init-pippel ()
  (use-package pippel
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'python-mode
            "P" 'pippel-list-packages)
    :config
    (evilified-state-evilify-map pippel-package-menu-mode-map
      :mode pippel-package-menu-mode)))

(defun python/init-py-isort ()
  (use-package py-isort
    :defer t
    :init
    (add-hook 'before-save-hook 'spacemacs//python-sort-imports)
    (spacemacs|spacebind
     :major
     (python-mode
      "Python key bindings"
      ("r" "Refactor"
       ("I" py-isort-buffer "Sort imports with 'isort'"))))))

(defun python/init-sphinx-doc ()
  (use-package sphinx-doc
    :defer t
    :init
    (add-hook 'python-mode-hook 'sphinx-doc-mode)
    (spacemacs|spacebind
     :major
     (python-mode
      "Python key bindings"
      ("S" "Sphinx"
       ("d" sphinx-doc "Insert function docstring")
       ("s" sphinx-doc-mode "Toggle sphinx-doc-mode"))))
    :config (spacemacs|hide-lighter sphinx-doc-mode)))

(defun python/init-pydoc ()
  (use-package pydoc
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "hp" 'pydoc-at-point-no-jedi
      "hP" 'pydoc)))

(defun python/init-pyenv-mode ()
  (use-package pyenv-mode
    :defer t
    ;; initialize via `spacemacs//python-setup-version-manager'
    :init
    (add-hook 'pyenv-mode-hook 'spacemacs//python-setup-pyenv-hook)
    (spacemacs/add-local-var-hook #'spacemacs//python-setup-version-manager
                                  :project-type 'python)
    (spacemacs|spacebind
     :project-minor
     (pyenv-mode
      "Pyenv-mode key bindings"
      ("V" "Environment"
       ("v" pyenv-mode-set "Set Python version")
       ("V" pyenv-mode-unset "Unset Python version"))))))

(defun python/init-pyvenv ()
  (use-package pyvenv
    :defer t
    ;; initialize via `spacemacs//python-setup-version-manager'
    :init
    (add-hook 'pyvenv-mode-hook 'spacemacs//python-setup-pyvenv-hook)
    (spacemacs/add-local-var-hook #'spacemacs//python-setup-version-manager
                                  :project-type 'python)
    (spacemacs|spacebind
     :project-minor
     (pyvenv-mode
      "Pyvenv-mode key bindings"
      ("V" "Environment"
       ("a" pyvenv-activate "Activate virtualenv...")
       ("d" pyvenv-deactivate "Deactivate virtualenv")
       ("w" pyvenv-workon "Activate virtualenv from $WORKON_HOME..."))))))

(defun python/init-pylookup ()
  (use-package pylookup
    :commands (pylookup-lookup pylookup-update pylookup-update-all)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "hH" 'pylookup-lookup)
    :config
    (evilified-state-evilify-map pylookup-mode-map
      :mode pylookup-mode)
    (let ((dir (configuration-layer/get-layer-local-dir 'python)))
      (setq pylookup-dir (concat dir "pylookup/")
            pylookup-program (concat pylookup-dir "pylookup.py")
            pylookup-db-file (concat pylookup-dir "pylookup.db")))
    (setq pylookup-completing-read 'completing-read)))

(defun python/init-pytest ()
  (use-package pytest
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-last-failed
               pytest-pdb-last-failed
               pytest-module
               pytest-pdb-module)
    :init (spacemacs//bind-python-testing-keys)
    :config (add-to-list 'pytest-project-root-files "setup.cfg")))

(defun python/init-python ()
  (use-package python
    :defer t
    :mode (("SConstruct\\'" . python-mode) ("SConscript\\'" . python-mode))
    :init
    (spacemacs/register-repl 'python
                             'spacemacs/python-start-or-switch-repl "python")
    (spacemacs//bind-python-repl-keys)
    (add-hook 'python-mode-hook 'spacemacs//python-default)
    (spacemacs/add-local-var-hook #'spacemacs//python-setup-backend
                                  :major-mode 'python-mode)
    :config
    ;; add support for `ahs-range-beginning-of-defun' for python-mode
    (with-eval-after-load 'auto-highlight-symbol
      (add-to-list 'ahs-plugin-bod-modes 'python-mode))
    (spacemacs/declare-prefix-for-mode 'python-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'python-mode "mg" "goto")
    (spacemacs|spacebind
     :major
     (python-mode
      "Python key bindings"
      ("'"  spacemacs/python-start-or-switch-repl "Start REPL / go toREPL")
      ("c" "Compile/Execute"
       ("c" spacemacs/python-execute-file "Compile script (C-u prompts for the command)")
       ("C" spacemacs/python-execute-file-focus "Compile script and focus (C-u prompts for the command)"))
      ("d" "Debug"
       ("b" spacemacs/python-toggle-breakpoint "Insert a breakpoint"))
      ("r" "Refactor"
       ("i" spacemacs/python-remove-unused-imports "Remove unused imports"))
      ("s" "REPL"
       ("b" spacemacs/python-shell-send-buffer "Send buffer")
       ("B" spacemacs/python-shell-send-buffer-switch "Send buffer and focus")
       ("e" spacemacs/python-shell-send-statement "Send statement")
       ("E" spacemacs/python-shell-send-statement-switch "Send statement and focus")
       ("f" spacemacs/python-shell-send-defun "Send function")
       ("F" spacemacs/python-shell-send-defun-switch "Send function and focus")
       ("i" spacemacs/python-start-or-switch-repl "Start REPL / Go to REPL")
       ("l" spacemacs/python-shell-send-line "Send line")
       ("L" spacemacs/python-shell-send-line-switch "Send line and focus")
       ("r" spacemacs/python-shell-send-region "Send region")
       ("R" spacemacs/python-shell-send-region-switch "Send region and focus")
       ("s" spacemacs/python-shell-send-with-output "Send region or line with output"))))

    ;; Set `python-indent-guess-indent-offset' to `nil' to prevent guessing `python-indent-offset
    ;; (we call python-indent-guess-indent-offset manually so python-mode does not need to do it)
    (setq-default python-indent-guess-indent-offset nil)

    ;; Emacs users won't need these key bindings
    ;; TODO: make these key bindings dynamic given the current style
    ;; Doing it only at init time won't update it if the user switches style
    ;; Also find a way to generalize these bindings.
    (when (eq dotspacemacs-editing-style 'vim)
      ;; the default in Emacs is M-n
      (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
      ;; the default in Emacs is M-p and this key binding overrides
      ;; default C-k which prevents Emacs users to kill line
      (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input)
      ;; the default in Emacs is M-r; C-r to search backward old output
      ;; and should not be changed
      (define-key inferior-python-mode-map
        (kbd "C-r") 'comint-history-isearch-backward)
      ;; this key binding is for recentering buffer in Emacs
      ;; it would be troublesome if Emacs user
      ;; Vim users can use this key since they have other key
      (define-key inferior-python-mode-map
        (kbd "C-l") 'spacemacs/comint-clear-buffer))

    ;; add this optional key binding for Emacs user, since it is unbound
    (define-key inferior-python-mode-map
      (kbd "C-c M-l") 'spacemacs/comint-clear-buffer)))

(defun python/post-init-semantic ()
  (when (configuration-layer/package-used-p 'anaconda-mode)
    (add-hook 'python-mode-hook
              'spacemacs//disable-semantic-idle-summary-mode t))
  (spacemacs/add-to-hook 'python-mode-hook
                         '(semantic-mode
                           spacemacs//python-imenu-create-index-use-semantic-maybe))
  (defadvice semantic-python-get-system-include-path
      (around semantic-python-skip-error-advice activate)
    "Don't cause error when Semantic cannot retrieve include
paths for Python then prevent the buffer to be switched. This
issue might be fixed in Emacs 25. Until then, we need it here to
fix this issue."
    (condition-case-unless-debug nil
        ad-do-it
      (error nil))))

(defun python/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (defadvice python-indent-dedent-line-backspace
        (around python/sp-backward-delete-char activate)
      (let ((pythonp (or (not smartparens-strict-mode)
                         (char-equal (char-before) ?\s))))
        (if pythonp
            ad-do-it
          (call-interactively 'sp-backward-delete-char))))))
(defun python/post-init-smartparens ()
  (add-hook 'inferior-python-mode-hook #'spacemacs//activate-smartparens))

(defun python/post-init-stickyfunc-enhance ()
  (add-hook 'python-mode-hook 'spacemacs/load-stickyfunc-enhance))

(defun python/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "gi" 'cscope/run-pycscope)))

(defun python/init-yapfify ()
  (use-package yapfify
    :defer t
    :init
    (spacemacs//bind-python-formatter-keys)
    (when (and python-format-on-save
               (eq 'yapf python-formatter))
      (add-hook 'python-mode-hook 'yapf-mode))
    :config (spacemacs|hide-lighter yapf-mode)))

(defun python/init-lsp-python-ms ()
  (use-package lsp-python-ms
    :if (eq python-lsp-server 'mspyls)
    :ensure nil
    :defer t
    :config
    (when python-lsp-git-root
      ;; Use dev version of language server checked out from github
      (setq lsp-python-ms-dir
            (expand-file-name (concat python-lsp-git-root
                                      "/output/bin/Release/")))
      (message "lsp-python-ms: Using version at `%s'" lsp-python-ms-dir)
      ;; Use a precompiled exe
      (setq lsp-python-ms-executable (concat lsp-python-ms-dir
                                             (pcase system-type
                                               ('gnu/linux "linux-x64/publish/")
                                               ('darwin "osx-x64/publish/")
                                               ('windows-nt "win-x64/publish/"))
                                             "Microsoft.Python.LanguageServer"
                                             (and (eq system-type 'windows-nt)
                                                  ".exe"))))))

(defun python/init-lsp-pyright ()
  (use-package lsp-pyright
    :if (eq python-lsp-server 'pyright)
    :ensure nil
    :defer t))

(defun python/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :python-layer
   (purpose-conf
    :mode-purposes '((inferior-python-mode . repl))
    :name-purposes '(("*compilation*" . logs)
                     ("*Pylookup Completions*" . help))
    :regexp-purposes '(("^\\*Anaconda" . help)
                       ("^\\*Pydoc" . help)
                       ("^\\*live-py" . logs)))))
