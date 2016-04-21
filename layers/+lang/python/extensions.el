;;; extensions.el --- Python Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Pre extensions are loaded *before* the packages
(setq python-pre-extensions '())

;; Post extensions are loaded *after* the packages
(setq python-post-extensions
  '(
    nose
    pylookup
    python-compile
    ))

;; Initialize the extensions

(defun python/init-nose ()
  (use-package nose
    :if (eq 'nose python-test-runner)
    :commands (nosetests-one
               nosetests-pdb-one
               nosetests-all
               nosetests-pdb-all
               nosetests-module
               nosetests-pdb-module
               nosetests-suite
               nosetests-pdb-suite)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "tA" 'nosetests-pdb-all
      "ta" 'nosetests-all
      "tB" 'nosetests-pdb-module
      "tb" 'nosetests-module
      "tT" 'nosetests-pdb-one
      "tt" 'nosetests-one
      "tM" 'nosetests-pdb-module
      "tm" 'nosetests-module
      "tS" 'nosetests-pdb-suite
      "ts" 'nosetests-suite)
    :config
    (progn
      (add-to-list 'nose-project-root-files "setup.cfg")
      (setq nose-use-verbose nil))))

(defun python/init-pylookup ()
  (use-package pylookup
    :commands (pylookup-lookup pylookup-update pylookup-update-all)
    :init
    (progn
      (evilified-state-evilify pylookup-mode pylookup-mode-map)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "mhH" 'pylookup-lookup))
    :config
    (progn
      (let ((dir (configuration-layer/get-layer-local-dir 'python)))
        (setq pylookup-dir (concat dir "pylookup/")
              pylookup-program (concat pylookup-dir "pylookup.py")
              pylookup-db-file (concat pylookup-dir "pylookup.db"))))))
