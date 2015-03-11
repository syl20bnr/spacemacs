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
    python-compile
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
        "mhH"  'pylookup-lookup)

      (let ((dir (configuration-layer/get-layer-property 'python :ext-dir)))
        (setq pylookup-dir (concat dir "/pylookup")
              pylookup-program (concat pylookup-dir "/pylookup.py")
              pylookup-db-file (concat pylookup-dir "/pylookup.db"))))))

(defun python/init-python-compile ()
   "Initialize Compile command for python buffers"
   ;; set compile command to buffer-file-name
   ;; if buffer-file-name exists
   ;; otherwise error occurs on e.g. org export including python src
   (add-hook 'python-mode-hook
           (lambda ()
             (set (make-local-variable 'compile-command)
                  (if buffer-file-name
                      (format "python %s" (file-name-nondirectory buffer-file-name))))))

    (defadvice compile (before ad-compile-smart activate)
    "Advises `compile' so it sets the argument COMINT to t
    in `python-mode' files"
    (when (derived-mode-p major-mode 'python-mode)
        (save-excursion
        (save-match-data
            (goto-char (point-min))
                ;; set COMINT argument to `t'.
                (ad-set-arg 1 t)))))
)
