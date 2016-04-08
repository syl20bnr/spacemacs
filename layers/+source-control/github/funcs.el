;;; funcs.el --- Github layer functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/git-link-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'spacemacs/git-link)))

(defun spacemacs/git-link-commit-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'spacemacs/git-link-commit)))

(defun spacemacs/git-link ()
  "Allow the user to run git-link in a git-timemachine buffer."
  (interactive)
  (require 'git-link)
  (if (and (boundp 'git-timemachine-revision)
           git-timemachine-revision)
      (cl-letf (((symbol-function 'git-link--branch)
                 (lambda ()
                   (car git-timemachine-revision))))
        (call-interactively 'git-link))
    (call-interactively 'git-link)))

(defun spacemacs/git-link-commit ()
  "Allow the user to run git-link-commit in a git-timemachine buffer."
  (interactive)
  (require 'git-link)
  (if (and (boundp 'git-timemachine-revision)
           git-timemachine-revision)
      (cl-letf (((symbol-function 'word-at-point)
                 (lambda ()
                   (car git-timemachine-revision))))
        (call-interactively 'git-link-commit))
    (call-interactively 'git-link-commit)))
