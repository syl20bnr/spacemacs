;;; packages.el --- docker Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;; Copyright (c) 2015 Alan Zimmerman & Contributors
;;
;; Author: Alan Zimmerman <alan.zimm@gmail.com>
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


(defconst docker-packages
  '(
    docker
    (docker-tramp :toggle (version< emacs-version "29.0.50"))
    dockerfile-mode
    flycheck))

(defun docker/init-docker ()
  (use-package docker
    :defer t
    :init
    (spacemacs/set-leader-keys "atd" #'docker)
    (evil-define-key 'normal docker-image-mode-map (kbd "q") 'quit-window)
    (evil-define-key 'normal docker-container-mode-map (kbd "q") 'quit-window)
    (evil-define-key 'normal docker-volume-mode-map (kbd "q") 'quit-window)
    (evil-define-key 'normal docker-network-mode-map (kbd "q") 'quit-window)
    (evil-define-key 'normal docker-machine-mode-map (kbd "q") 'quit-window)))

(defun docker/init-docker-tramp ()
  (use-package docker-tramp
    :defer t))

(defun docker/init-dockerfile-mode ()
  (use-package dockerfile-mode
    :defer t
    :init (add-hook 'dockerfile-mode-local-vars-hook #'spacemacs//docker-dockerfile-setup-backend)
    :config
    (spacemacs/set-leader-keys-for-major-mode 'dockerfile-mode
      (if (null docker-dockerfile-backend) "b" "cb") 'dockerfile-build-buffer
      (if (null docker-dockerfile-backend) "B" "cB") 'dockerfile-build-no-cache-buffer)
    (if (package-installed-p 'docker)
        (spacemacs/set-leader-keys-for-major-mode 'dockerfile-mode
          "d" 'docker
          "i" 'docker-images
          "p" 'docker-containers))))

(defun docker/post-init-flycheck ()
  (spacemacs/enable-flycheck 'dockerfile-mode))
