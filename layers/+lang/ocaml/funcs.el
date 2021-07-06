;;; funcs.el --- ocaml Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun spacemacs//init-ocaml-opam ()
  (if (executable-find "opam")
      (let ((share (string-trim-right
                    (with-output-to-string
                      (with-current-buffer
                          standard-output
                        (process-file
                         shell-file-name nil '(t nil) nil shell-command-switch
                         "opam config var share"))))))
        (cond ((string= "" share)
               (spacemacs-buffer/warning
                "\"opam config var share\" output empty string."))
              ((not (file-directory-p share))
               (spacemacs-buffer/warning
                "opam share directory does not exist."))
              (t (setq opam-share share
                       opam-load-path (concat share "/emacs/site-lisp"))
                 (add-to-list 'load-path opam-load-path))))
    (unless (executable-find "ocamlmerlin")
      (spacemacs-buffer/warning
       (concat "Cannot find \"opam\" or \"merlin\" executable. "
               "The ocaml layer won't work properly.")))))

(defun spacemacs/merlin-locate ()
  (interactive)
  (let ((merlin-locate-in-new-window 'never))
    (merlin-locate)))

(defun spacemacs/merlin-locate-other-window ()
  (interactive)
  (let ((merlin-locate-in-new-window 'always))
    (merlin-locate)))
