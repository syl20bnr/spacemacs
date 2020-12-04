;;; funcs.el --- ocaml Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//init-ocaml-opam ()
  (if (executable-find "opam")
      (let ((share (string-trim-right
                    (with-output-to-string
                      (with-current-buffer
                          standard-output
                        (process-file
                         shell-file-name nil '(t nil) nil shell-command-switch
                         "opam config var share"))))))
        (cond ((string= "" share)
               (space-macs-buffer/warning
                "\"opam config var share\" output empty string."))
              ((not (file-directory-p share))
               (space-macs-buffer/warning
                "opam share directory does not exist."))
              (t (setq opam-share share
                       opam-load-path (concat share "/e-macs/site-lisp"))
                 (add-to-list 'load-path opam-load-path))))
    (unless (executable-find "ocamlmerlin")
      (space-macs-buffer/warning
       (concat "Cannot find \"opam\" or \"merlin\" executable. "
               "The ocaml layer won't work properly.")))))

(defun space-macs/merlin-locate ()
  (interactive)
  (let ((merlin-locate-in-new-window 'never))
    (merlin-locate)))

(defun space-macs/merlin-locate-other-window ()
  (interactive)
  (let ((merlin-locate-in-new-window 'always))
    (merlin-locate)))


