(setq ropemacs-global-prefix "C-x /") ;; avoid conflict with p4 global prefix
(require 'pymacs)

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
                   ))))

;; Ropemacs Configuration
(eval-after-load 'python
  '(progn
     (setup-ropemacs)
  ))
