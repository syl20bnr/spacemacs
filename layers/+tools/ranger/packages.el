;;; packages.el --- ranger Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Rich Alesi
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ranger-packages
      '(
        (dired :location built-in)
        golden-ratio
        ranger
        ))

(defun ranger//set-leader-keys ()
  (spacemacs/set-leader-keys
    "ar" 'ranger
    "ad" 'deer))

(defun ranger/init-ranger ()
  (use-package ranger
    :defer t
    :init
    (progn
      (ranger//set-leader-keys)
      ;; set up image-dired to allow picture resize
      (setq image-dired-dir (concat spacemacs-cache-directory "image-dir"))
      (unless (file-directory-p image-dired-dir)
        (make-directory image-dired-dir)))
    :config
    (define-key ranger-mode-map (kbd "-") 'ranger-up-directory)))

(defun ranger/post-init-dired ()
  ;; Be sure to override dired bindings
  (ranger//set-leader-keys))

(defun ranger/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (add-to-list 'golden-ratio-exclude-modes "ranger-mode")))
