;;; packages.el --- ranger Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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
  (spacemacs/declare-prefix "atr" "ranger/deer")
  (spacemacs/set-leader-keys
    "atrr" 'ranger
    "atrd" 'deer
    "jD" 'deer-jump-other-window
    "jd" 'deer))

(defun ranger/init-ranger ()
  (use-package ranger
    :commands (ranger deer deer-jump-other-window ranger-override-dired-mode)
    :init
    (progn
      (ranger//set-leader-keys)

      (when (memq ranger-enter-with-minus '(deer ranger))
        (define-key evil-motion-state-map (kbd "-") ranger-enter-with-minus))

      ;; set up image-dired to allow picture resize
      (setq image-dired-dir (concat spacemacs-cache-directory "image-dir"))
      (unless (file-directory-p image-dired-dir)
        (make-directory image-dired-dir)))
    :config
    (progn
      (when (memq 'helm dotspacemacs-configuration-layers)
        (require 'helm))
      (define-key ranger-mode-map (kbd "-") 'ranger-up-directory))))

(defun ranger/post-init-dired ()
  ;; Be sure to override dired bindings
  (ranger//set-leader-keys))

(defun ranger/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (add-to-list 'golden-ratio-exclude-modes "ranger-mode")))
