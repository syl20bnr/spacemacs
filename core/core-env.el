;;; core-env.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'core-dotspacemacs)
(require 'load-env-vars)

(defvar spacemacs-env-vars-file
  (concat (or dotspacemacs-directory spacemacs-private-directory) "spacemacs.env")
  "Absolute path to the env file where environment variables are set.")

(defun spacemacs/init-env (&optional force)
  "Attempt to fetch the environment variables from the users shell.
This solution is far from perfect and we should not rely on this function
a lot. We use it only to initialize the env file when it does not exist
yet.
If FORCE is non-nil then force the initialization of the file, note that the
current contents of the file will be overwritten."
  (when (or force (not (file-exists-p spacemacs-env-vars-file)))
    (with-temp-file spacemacs-env-vars-file
      (let ((shell-command-switch "-ic"))
        (insert (shell-command-to-string "env"))))
    (spacemacs-buffer/warning
     (concat "Spacemacs tried to import your environment variables from "
             "your shell and saved them to `%s'. "
             "Please check that the values are correct by calling "
             "`spacemacs/edit-env' function and update the file if needed. "
             "If you later need to add environment variables add them to this "
             "file.")
     spacemacs-env-vars-file)))

(defun spacemacs/force-init-env ()
  "Forces a reinitialization of environment variables."
  (interactive)
  (spacemacs/init-env t))

(defun spacemacs/edit-env ()
  "Open the env file for edition."
  (interactive)
  (if (file-exists-p spacemacs-env-vars-file)
      (progn
        (find-file spacemacs-env-vars-file)
        (when (fboundp 'dotenv-mode)
          (dotenv-mode)))
    (message "Cannot file env file `%s'" spacemacs-env-vars-file)))

(defun spacemacs/load-env (&optional force)
  "Load the environment variables from the env file.
If FORCE is non-nil then force the loading of environment variables from env
file."
  (interactive "P")
  (when (or force (and (display-graphic-p)
                       (or (eq system-type 'darwin)
                           (eq system-type 'gnu/linux)
                           (eq window-system 'x)))))
  (if (file-exists-p spacemacs-env-vars-file)
      (progn
        (load-env-vars spacemacs-env-vars-file)
        (message "Environment variables loaded."))
    (message "Cannot file env file `%s'" spacemacs-env-vars-file)))

(provide 'core-env)
