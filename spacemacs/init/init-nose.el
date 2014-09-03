(use-package nose
  :commands (nosetests-one
             nosetests-pdb-one
             nosetests-all
             nosetests-pdb-all
             nosetests-module
             nosetests-pdb-module
             nosetests-suite
             nosetests-pdb-suite)
  :config
  (progn
    (add-to-list 'nose-project-root-files "setup.cfg")
    (setq nose-use-verbose nil)))
