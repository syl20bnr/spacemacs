;;; packages.el --- Python Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq python-packages
  '(
    company
    counsel-gtags
    cython-mode
    eldoc
    evil-matchit
    flycheck
    ggtags
    helm-cscope
    helm-gtags
    (helm-pydoc :requires helm)
    importmagic
    live-py-mode
    (nose :location local)
    org
    pip-requirements
    pipenv
    pippel
    py-isort
    pyenv-mode
    (pylookup :location local)
    pytest
    (python :location built-in)
    pyvenv
    semantic
    smartparens
    stickyfunc-enhance
    xcscope
    yapfify
    ;; packages for anaconda backend
    anaconda-mode
    (company-anaconda :requires company)
    ;; packages for lsp backend
    (lsp-python :requires lsp-mode)
    ))

(defun python/init-anaconda-mode ()
  (use-package anaconda-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hh" 'anaconda-mode-show-doc
        "ga" 'anaconda-mode-find-assignments
        "gb" 'xref-pop-marker-stack
        "gu" 'anaconda-mode-find-references)
      (setq anaconda-mode-installation-directory
            (concat spacemacs-cache-directory "anaconda-mode")))
    :config
    (progn
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
                   '(anaconda-mode-find-definitions :async t)))))

(defun python/post-init-company ()
  ;; backend specific
  (add-hook 'python-mode-local-vars-hook #'spacemacs//python-setup-company)
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
    :defer t
    ;; see `spacemacs//python-setup-anaconda-company'
    ))

(defun python/init-cython-mode ()
  (use-package cython-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'cython-mode
        "hh" 'anaconda-mode-show-doc
        "gu" 'anaconda-mode-find-references))))

(defun python/post-init-eldoc ()
  (add-hook 'python-mode-local-vars-hook #'spacemacs//python-setup-eldoc))

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

(defun python/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'python-mode))

(defun python/post-init-ggtags ()
  (add-hook 'python-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun python/init-helm-pydoc ()
  (use-package helm-pydoc
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "hd" 'helm-pydoc)))

(defun python/init-importmagic ()
  (use-package importmagic
    :defer t
    :init
    (progn
      (add-hook 'python-mode-hook 'importmagic-mode)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "rf" 'importmagic-fix-symbol-at-point))))

(defun python/init-live-py-mode ()
  (use-package live-py-mode
    :defer t
    :commands live-py-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "l" 'live-py-mode)))

(defun python/init-lsp-python ()
  (use-package lsp-python
    :commands lsp-python-enable
    :config (spacemacs//setup-lsp-jump-handler 'python-mode)))

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
    (progn
      (add-to-list 'nose-project-root-files "setup.cfg")
      (setq nose-use-verbose nil))))

(defun python/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(python . t))))

(defun python/init-pipenv ()
  (use-package pipenv
    :commands (pipenv-activate
               pipenv-deactivate
               pipenv-shell
               pipenv-open
               pipenv-install
               pipenv-uninstall)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "vpa" 'pipenv-activate
        "vpd" 'pipenv-deactivate
        "vpi" 'pipenv-install
        "vpo" 'pipenv-open
        "vps" 'pipenv-shell
        "vpu" 'pipenv-uninstall))))

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
    (progn
      (add-hook 'before-save-hook 'spacemacs//python-sort-imports)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "rI" 'py-isort-buffer))))

(defun python/pre-init-pyenv-mode ()
  (add-to-list 'spacemacs--python-pyenv-modes 'python-mode))
(defun python/init-pyenv-mode ()
  (use-package pyenv-mode
    :if (executable-find "pyenv")
    :commands (pyenv-mode-versions)
    :init
    (progn
      (pcase python-auto-set-local-pyenv-version
       (`on-visit
        (dolist (m spacemacs--python-pyenv-modes)
          (add-hook (intern (format "%s-hook" m))
                    'spacemacs//pyenv-mode-set-local-version)))
       (`on-project-switch
        (add-hook 'projectile-after-switch-project-hook
                  'spacemacs//pyenv-mode-set-local-version)))
      ;; setup shell correctly on environment switch
      (dolist (func '(pyenv-mode-set pyenv-mode-unset))
        (advice-add func :after 'spacemacs/python-setup-everything))
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "vu" 'pyenv-mode-unset
        "vs" 'pyenv-mode-set))))

(defun python/pre-init-pyvenv ()
  (add-to-list 'spacemacs--python-pyvenv-modes 'python-mode))
(defun python/init-pyvenv ()
  (use-package pyvenv
    :defer t
    :init
    (progn
      (pcase python-auto-set-local-pyvenv-virtualenv
        (`on-visit
         (dolist (m spacemacs--python-pyvenv-modes)
           (add-hook (intern (format "%s-hook" m))
                     'spacemacs//pyvenv-mode-set-local-virtualenv)))
        (`on-project-switch
         (add-hook 'projectile-after-switch-project-hook
                   'spacemacs//pyvenv-mode-set-local-virtualenv)))
      (dolist (m spacemacs--python-pyvenv-modes)
        (spacemacs/set-leader-keys-for-major-mode m
          "va" 'pyvenv-activate
          "vd" 'pyvenv-deactivate
          "vw" 'pyvenv-workon))
      ;; setup shell correctly on environment switch
      (dolist (func '(pyvenv-activate pyvenv-deactivate pyvenv-workon))
        (advice-add func :after 'spacemacs/python-setup-everything)))))

(defun python/init-pylookup ()
  (use-package pylookup
    :commands (pylookup-lookup pylookup-update pylookup-update-all)
    :init
    (progn
      (evilified-state-evilify pylookup-mode pylookup-mode-map)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hH" 'pylookup-lookup))
    :config
    (progn
      (let ((dir (configuration-layer/get-layer-local-dir 'python)))
        (setq pylookup-dir (concat dir "pylookup/")
              pylookup-program (concat pylookup-dir "pylookup.py")
              pylookup-db-file (concat pylookup-dir "pylookup.db")))
        (setq pylookup-completing-read 'completing-read))))

(defun python/init-pytest ()
  (use-package pytest
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-module
               pytest-pdb-module)
    :init (spacemacs//bind-python-testing-keys)
    :config (add-to-list 'pytest-project-root-files "setup.cfg")))

(defun python/init-python ()
  (use-package python
    :defer t
    :mode (("SConstruct\\'" . python-mode) ("SConscript\\'" . python-mode))
    :init
    (progn
      (spacemacs/register-repl 'python
                               'spacemacs/python-start-or-switch-repl "python")
      (add-hook 'inferior-python-mode-hook
                'spacemacs//inferior-python-setup-hook)
      (spacemacs/add-to-hook 'python-mode-hook
                             '(spacemacs//python-setup-backend
                               spacemacs//python-default))
      ;; call `spacemacs//python-setup-shell' once, don't put it in a hook
      ;; (see issue #5988)
      (spacemacs//python-setup-shell))
    :config
    (progn
      ;; add support for `ahs-range-beginning-of-defun' for python-mode
      (with-eval-after-load 'auto-highlight-symbol
        (add-to-list 'ahs-plugin-bod-modes 'python-mode))

      (spacemacs/declare-prefix-for-mode 'python-mode "mc" "execute")
      (spacemacs/declare-prefix-for-mode 'python-mode "md" "debug")
      (spacemacs/declare-prefix-for-mode 'python-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'python-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'python-mode "ms" "REPL")
      (spacemacs/declare-prefix-for-mode 'python-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'python-mode "mv" "virtualenv")
      (spacemacs/declare-prefix-for-mode 'python-mode "mvp" "pipenv")
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "'"  'spacemacs/python-start-or-switch-repl
        "cc" 'spacemacs/python-execute-file
        "cC" 'spacemacs/python-execute-file-focus
        "db" 'spacemacs/python-toggle-breakpoint
        "ri" 'spacemacs/python-remove-unused-imports
        "sB" 'spacemacs/python-shell-send-buffer-switch
        "sb" 'python-shell-send-buffer
        "sF" 'spacemacs/python-shell-send-defun-switch
        "sf" 'python-shell-send-defun
        "si" 'spacemacs/python-start-or-switch-repl
        "sR" 'spacemacs/python-shell-send-region-switch
        "sr" 'python-shell-send-region)

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
        (kbd "C-c M-l") 'spacemacs/comint-clear-buffer))))

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
  (add-hook 'inferior-python-mode-hook 'smartparens-mode))

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
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "=" 'yapfify-buffer)
      (when python-enable-yapf-format-on-save
        (add-hook 'python-mode-hook 'yapf-mode)))
    :config (spacemacs|hide-lighter yapf-mode)))
