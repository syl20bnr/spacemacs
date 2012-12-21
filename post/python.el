
;; flymake -------------------------------------------------------------------
(defun flymake-create-copy-file ()
  "Create a copy local file"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                     'flymake-create-temp-inplace)))
    (file-relative-name 
     temp-file 
     (file-name-directory buffer-file-name))))     

(defun flymake-command-parse (cmdline)
  "Parses the command line CMDLINE in a format compatible
       with flymake, as:(list cmd-name arg-list)

The CMDLINE should be something like:

 flymake %f python custom.py %f

%f will be substituted with a temporary copy of the file that is
 currently being checked.
"
  (let ((cmdline-subst (replace-regexp-in-string "%f" (flymake-create-copy-file) cmdline)))
    (setq cmdline-subst (split-string-and-unquote cmdline-subst))
    (list (first cmdline-subst) (rest cmdline-subst))
    ))


(when (load-file (concat user-extensions-directory "flymake-patch.el"))
  (setq flymake-info-line-regex
        (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))
  (load-library "flymake-cursor"))

(defun epy-setup-checker (cmdline)
  (add-to-list 'flymake-allowed-file-name-masks
               (list "\\.py\\'" (apply-partially 'flymake-command-parse cmdline)))
  )
(epy-setup-checker "pyflakes %f")

;; pymacs --------------------------------------------------------------------
(setq ropemacs-global-prefix "C-x /") ;; avoid conflict with p4 global prefix
(require 'pymacs (concat user-extensions-directory "pymacs.el"))
(defun setup-ropemacs ()
  (pymacs-load "ropemacs" "rope-")
  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)
  ;; Configurations
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"))
  ;; Adding hook to automatically open a rope project if there is one
  ;; in the current or in the upper level directory
   (add-hook 'python-mode-hook
            (lambda ()
              (cond ((file-exists-p ".ropeproject")
                     (rope-open-project default-directory))
                    ((file-exists-p "../.ropeproject")
                     (rope-open-project (concat default-directory "..")))
                    )))
  )

(eval-after-load 'python
  '(progn
     ;; Ropemacs Configuration
     (setup-ropemacs)
     ;; Not on all modes, please
     ;; Be careful of mumamo, buffer file name nil
     (add-hook 'python-mode-hook (lambda () (if (buffer-file-name)
                        (flymake-mode))))
     )
  )

(add-hook 'python-mode-hook (lambda ()
                              (local-set-key "\C-c\C-c" 'syl-python-compile)))
