;;; funcs.el --- Factor Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: timor <timor.dd@googlemail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(autoload 'feature-file "loadhist")
(autoload 'file-requires "loadhist")

(defvar factor--ui-listener-process nil
  "Holds the factor process which serves the current fuel connection.")

(defun factor//fuel-stack-effect ()
  "Small wrapper around factors stack effect help. If region is
active, use that, otherwise use sexp under point."
  (interactive)
  (if (region-active-p)
      (call-interactively 'fuel-stack-effect-region)
    (call-interactively 'fuel-stack-effect-sexp)))

(defun factor//fuel-elisp-dir ()
  "Get the directory of the currently loaded fuel implementation."
  (expand-file-name (concat (feature-file 'factor-mode)
                           "/..")))

(defun factor//fuel-feature-p (fuel-directory feature)
  "Check if FEATURE is provided by a file under FUEL-DIRECTORY."
  (string-prefix-p fuel-directory (feature-file feature)))

(defun factor//loaded-fuel-features ()
  "Recursively determine all loaded features that belong to the
currently loaded fuel implementation."
  (cl-loop with fuel-directory = (factor//fuel-elisp-dir)
           for fuel-features = '(fuel-mode factor-mode)
           then (cl-union fuel-features new-features)
           for next-features = (cl-remove-duplicates
                                (cl-reduce 'cl-union
                                           (mapcar (lambda(f) (file-requires (feature-file f)))
                                                   fuel-features)))
           for new-features = (cl-remove-if-not
                               (lambda(f) (and (featurep f)
                                               (factor//fuel-feature-p fuel-directory f)))
                               (cl-set-difference next-features fuel-features))
           while new-features
           finally (return fuel-features)))

(defun factor//unload-fuel ()
  "Close fuel connection and unload fuel code.

Will stop current fuel connection if applicable."
  (when (and (featurep 'fuel-listener)
             fuel-listener--buffer)
    (kill-buffer fuel-listener--buffer))
  (cl-loop for f in (factor//loaded-fuel-features) do
           (unload-feature f t)))

(defun factor//load-fuel-from-path (path)
  "Load emacs lisp fuel implementation from the specified PATH."
  (let ((load-path (cons path load-path)))
    (require 'fuel-mode)
    (require 'factor-mode)))

(defun factor//reload-fuel-from-path (path)
  "Unload current emacs lisp fuel implementation and load the on from PATH.

Since unloading switches buffers which were in factor-mode back
to fundamental mode, this re-enables factor-mode in these buffers
afterwards.

Only reloads if currently loaded factor mode belongs to a different factor root.
"
  (when (not (string-equal (factor//fuel-elisp-dir)
                           path))
    (let ((factor-buffers (cl-loop for b being the buffers
                                   if (eq (buffer-local-value 'major-mode b)
                                          'factor-mode)
                                   collect b)))
      (factor//unload-fuel)
      (message "Reloading fuel mode from %s" path)
      (factor//load-fuel-from-path path)
      (cl-loop for b in factor-buffers do
               (with-current-buffer b
                 (factor-mode))))))

(defun factor/start-connect-factor (factor-binary factor-image fuel-path &optional cmd-line-options)
  "Start a graphical Factor listener at FACTOR-ROOT.

If non-nil IMAGE-NAME denotes a path to the desired factor image
relative to FACTOR-ROOT, if no absolute path is given. Connect to
it using the fuel-mode implementation in FUEL-PATH, if non-nil. Returns the
process object.

Will append the value of `factor-ui-listener-args' to the command line options.

Returns the process object.
"
  (when fuel-path
    (factor//reload-fuel-from-path fuel-path))
  (setq factor--ui-listener-process
        (start-process-shell-command
         "Factor-UI-Listener" "*Factor-UI-Listener*"
         (format "%s -image='%s' -e='USING: fuel.remote vocabs.loader ; fuel-start-remote-listener* \"ui.tools\" run ' %s"
                 factor-binary
                 factor-image
                 (or cmd-line-options "")))))

(defun factor/start-ui-listener ()
  "Run the graphical factor listener with fuel support and connect to it.

If `fuel-factor-root-dir' is set,
unloads the current fuel implementation and reloads fuel from there.
"
  (interactive)
  (when (and (process-live-p factor--ui-listener-process)
             (y-or-n-p "Graphical listener already running.  Kill process?"))
    (delete-process factor--ui-listener-process))
  (let* ((factor-process (factor/start-connect-factor
                          (fuel-listener-factor-binary)
                          (fuel-listener-factor-image)
                          (expand-file-name "misc/fuel" fuel-factor-root-dir))))
    ;; This check will only catch immediate failures:
    (unless (process-live-p factor-process)
      (error "Listener process exited with code: %d"
             (process-exit-status factor-process)))))
