;;; packages.el --- docker Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2015 Sylvain Benner
;; Copyright (c) 2015 Alan Zimmerman & Contributors
;;
;; Author: Alan Zimmerman <alan.zimm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst docker-packages
  '(
    docker
    docker-tramp
    dockerfile-mode
    ))

(defun docker/init-docker ()
  (use-package docker
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "D" "Docker")
      (evil-leader/set-key
        "Dc" 'docker-containers
        "Dd" 'docker-rmi
        "De" 'docker-unpause
        "DF" 'docker-pull
        "Dk" 'docker-rm
        "Di" 'docker-images
        "Do" 'docker-stop
        "DP" 'docker-push
        "Dp" 'docker-pause
        "Dr" 'docker-restart
        "Ds" 'docker-start)))
  (with-eval-after-load 'docker-containers
    (evilified-state-evilify-map docker-containers-mode-map
      :mode docker-containers-mode))
  (with-eval-after-load 'docker-images
    (evilified-state-evilify-map docker-images-mode-map
      :mode docker-images-mode)))

(defun docker/init-docker-tramp ()
  (use-package docker-tramp
    :defer t))

(defun docker/init-dockerfile-mode ()
  (use-package docker-mode
    :defer t
    :config (evil-leader/set-key-for-mode 'dockerfile-mode
              "mcb" 'dockerfile-build-buffer)))
