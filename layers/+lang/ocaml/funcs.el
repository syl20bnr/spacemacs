;;; funcs.el --- ocaml Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//init-ocaml-opam ()
  (if (executable-find "opam")
      (let* ((output (shell-command-to-string
                      "opam config var share 2> /dev/null"))
             (share (when (< 0 (length output))
                      (substring output 0 -1))))
          (when share
            (setq opam-share share
                  opam-load-path (concat share "/emacs/site-lisp")))
          (add-to-list 'load-path opam-load-path))
    (spacemacs-buffer/warning
     (concat "Cannot find \"opam\" executable. "
             "The ocaml layer won't work properly."))))

(defun spacemacs/merlin-locate ()
  (interactive)
  (let ((merlin-locate-in-new-window 'never))
    (merlin-locate)))

(defun spacemacs/merlin-locate-other-window ()
  (interactive)
  (let ((merlin-locate-in-new-window 'always))
    (merlin-locate)))
