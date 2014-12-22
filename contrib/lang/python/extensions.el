;; Extensions are in emacs_paths/extensions

;; Pre extensions are loaded *before* the packages
(defvar python-pre-extensions
  '(
    ))

;; Post extensions are loaded *after* the packages
(defvar python-post-extensions
  '(
    nose
    pylookup
    ))

;; Initialize the extensions

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
    :init
    (evil-leader/set-key-for-mode 'python-mode
      "mTa" 'nosetests-pdb-all
      "mta" 'nosetests-all
      "mTb" 'nosetests-pdb-module
      "mtb" 'nosetests-module
      "mTt" 'nosetests-pdb-one
      "mtt" 'nosetests-one
      "mTm" 'nosetests-pdb-module
      "mtm" 'nosetests-module
      "mTs" 'nosetests-pdb-suite
      "mts" 'nosetests-suite)
    :config
    (progn
      (add-to-list 'nose-project-root-files "setup.cfg")
      (setq nose-use-verbose nil)
      )))

(defun python/init-pylookup ()
  (use-package pylookup
    :commands pylookup-lookup
    :config
    (progn
      (add-to-list 'evil-emacs-state-modes 'pylookup-mode)
      (evil-add-hjkl-bindings pylookup-mode-map 'emacs)
      (evil-leader/set-key-for-mode 'python-mode
        "mhD"  'pylookup-lookup)

      (let ((dir (config-system/get-layer-property 'python :ext-dir)))
        (setq pylookup-dir (concat dir "/pylookup")
              pylookup-program (concat pylookup-dir "/pylookup.py")
              pylookup-db-file (concat pylookup-dir "/pylookup.db"))))))
