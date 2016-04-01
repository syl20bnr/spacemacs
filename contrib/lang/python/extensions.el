;;; extensions.el --- Python Layer extensions File for Spacemacs
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
    :init
    (progn
      (evilify pylookup-mode pylookup-mode-map)
      (evil-leader/set-key-for-mode 'python-mode
        "mhH"  'pylookup-lookup))
    :config
    (progn
      (let ((dir (configuration-layer/get-layer-property 'python :ext-dir)))
        (setq pylookup-dir (concat dir "/pylookup")
              pylookup-program (concat pylookup-dir "/pylookup.py")
              pylookup-db-file (concat pylookup-dir "/pylookup.db"))))))
