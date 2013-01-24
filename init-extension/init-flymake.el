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

(when (load-file (concat user-extensions-directory "flymake/flymake-patch.el"))
  (setq flymake-info-line-regex
        (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))
  (load-library "flymake-cursor"))

;; from http://stackoverflow.com/questions/2571436/emacs-annoying-flymake-dialog-box
(defun flymake-display-warning (warning)
  "Display a warning to the user in the mini-buffer instead of a dialog box"
  (message warning))

(defun epy-setup-checker (cmdline)
  (add-to-list 'flymake-allowed-file-name-masks
               (list "\\.py\\'" (apply-partially 'flymake-command-parse cmdline)))
  )
(epy-setup-checker "pyflakes %f")

(eval-after-load 'python
  '(progn
     (add-hook 'python-mode-hook (lambda () (if (buffer-file-name)
                        (flymake-mode))))
     ))

;; from http://www.emacswiki.org/emacs/FlyMake
(defun safer-flymake-find-file-hook ()
  "Don't barf if we can't open this flymake file"
  (let ((flymake-filename
         (flymake-create-temp-inplace (buffer-file-name) "flymake")))
    (if (file-writable-p flymake-filename)
        (flymake-find-file-hook)
      (message
       (format
        "Couldn't enable flymake; permission denied on %s" flymake-filename)))))
(add-hook 'find-file-hook 'safer-flymake-find-file-hook)

;; from http://stackoverflow.com/questions/6110691/is-there-a-way-to-make-flymake-to-compile-only-when-i-save
(eval-after-load "flymake"
  '(progn
    (defun flymake-after-change-function (start stop len)
      "Start syntax check for current buffer if it isn't already running."
      ;; Do nothing, don't want to run checks until I save.
      )))
