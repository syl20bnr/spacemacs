;;; packages.el --- docker Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner
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
    company-lsp
    docker
    docker-tramp
    dockerfile-mode
    flycheck
    (lsp-dockerfile
     :requires lsp-mode
     :location (recipe :fetcher github
                       :repo "emacs-lsp/lsp-dockerfile"))
    ))

(defun docker/post-init-company-lsp ()
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes dockerfile-mode
    :append-hooks nil
    :call-hooks t))

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
        "aDo" 'docker-stop
        "aDP" 'docker-push
        "aDp" 'docker-pause
        "aDr" 'docker-restart
        "aDs" 'docker-start)))
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
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'dockerfile-mode
        "mc" "compile")
      (spacemacs/set-leader-keys-for-major-mode 'dockerfile-mode
        "cb" 'dockerfile-build-buffer
        "cB" 'dockerfile-build-no-cache-buffer))))

(defun docker/post-init-flycheck ()
  (spacemacs/enable-flycheck 'dockerfile-mode))

(defun docker/init-lsp-dockerfile ()
  (use-package lsp-dockerfile
    :commands lsp-dockerfile-enable
    :init
    (if dockerfile-mode-enable-lsp
        (add-hook 'dockerfile-mode-local-vars-hook
                  #'spacemacs//dockerfile-setup-lsp))
    :config
    (spacemacs/set-leader-keys-for-major-mode 'dockerfile-mode
      "=" #'lsp-format-buffer)))
