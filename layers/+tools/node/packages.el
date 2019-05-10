;;; packages.el --- node layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Juan Placencia <juan.placencia.512@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq node-packages
      '((add-node-modules-path :toggle node-add-modules-path)
        (npm-and-yarn :location local)))

(defun node/init-add-node-modules-path ()
  (use-package add-node-modules-path :defer t))

(defun node/init-npm-and-yarn ()
  (use-package npm-and-yarn
    :commands (npm-and-yarn/npm-install
               npm-and-yarn/npm-install-dev
               npm-and-yarn/yarn-add
               npm-and-yarn/yarn-add-dev)
    :init
    (dolist (mode spacemacs--npm-and-yarn-modes)
      (spacemacs/declare-prefix-for-mode mode "mn" "npm")
      (spacemacs/declare-prefix-for-mode mode "my" "yarn")
      (spacemacs/set-leader-keys-for-major-mode mode
        "ni" #'npm-and-yarn/npm-install
        "nI" #'npm-and-yarn/npm-install-dev
        "ya" #'npm-and-yarn/yarn-add
        "yA" #'npm-and-yarn/yarn-add-dev))))
