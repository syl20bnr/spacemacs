;;; packages.el --- ranger Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
        ranger
        ))

(defun ranger//set-leader-keys ()
  (spacemacs/set-leader-keys
    "ar" 'ranger
    "ad" 'deer))

(defun ranger/init-ranger ()
  (use-package ranger
    :defer t
    :commands (ranger deer ranger-override-dired-fn)
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
  (ranger//set-leader-keys)
  ;; need to apply this to compensate for defer
  (spacemacs|use-package-add-hook ranger
    :post-init (when ranger-override-dired
                 (add-hook 'dired-mode-hook #'ranger-override-dired-fn))))
