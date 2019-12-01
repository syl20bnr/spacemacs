;;; packages.el --- docker Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
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
    flycheck
    ))

(defun docker/init-docker ()
  (use-package docker
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "aD" "Docker")
      (evil-leader/set-key
        "aDc" 'docker-containers
        "aDC" 'docker-compose
        "aDd" 'docker-rmi
        "aDe" 'docker-unpause
        "aDF" 'docker-pull
        "aDk" 'docker-rm
        "aDi" 'docker-images
        "aDm" 'docker-machines
        "aDn" 'docker-networks
        "aDo" 'docker-stop
        "aDP" 'docker-push
        "aDp" 'docker-pause
        "aDr" 'docker-restart
        "aDs" 'docker-start
        "aDv" 'docker-volumes)))
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
  (use-package dockerfile-mode
    :defer t
    :init (add-hook 'dockerfile-mode-local-vars-hook #'spacemacs//docker-dockerfile-setup-backend)
    :config
    (spacemacs/declare-prefix-for-mode 'dockerfile-mode "mc" "compile")
    (spacemacs/set-leader-keys-for-major-mode 'dockerfile-mode
      "cb" 'dockerfile-build-buffer
      "cB" 'dockerfile-build-no-cache-buffer)))

(defun docker/post-init-flycheck ()
  (spacemacs/enable-flycheck 'dockerfile-mode))
