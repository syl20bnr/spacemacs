;;; packages.el --- dockerfile Layer packages File for Spacemacs
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

(setq dockerfile-packages
  '(
    dockerfile-mode
    docker-tramp
    docker
    ))

(defun dockerfile/init-dockerfile-mode ()
  (use-package dockerfile-mode
    :defer t
    :config
    (progn
      (evil-leader/set-key-for-mode 'dockerfile-mode
         "mcb" 'dockerfile-build-buffer
       )))
 )

(defun dockerfile/init-docker-tramp ()
  (use-package docker-tramp
    :defer t))

(defun dockerfile/init-docker ()
  (use-package docker
    :defer t
    :commands docker-containers
              docker-images
    :init
    (spacemacs/declare-prefix "D" "Docker")
    (evil-leader/set-key
      "Dc" 'docker-containers
      "Dk" 'docker-rm
      "Do" 'docker-stop
      "Dp" 'docker-pause
      "Dr" 'docker-restart
      "Ds" 'docker-start
      "De" 'docker-unpause
      "Di" 'docker-images
      "Dd" 'docker-rmi
      "DF" 'docker-pull
      "DP" 'docker-push)
    (evilify docker-containers-mode docker-container-mode-map)
    (evilify docker-images-mode docker-images-mode-map)))
