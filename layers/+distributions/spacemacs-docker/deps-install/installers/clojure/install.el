#!/usr/bin/emacs --script
;;; install.el --- clojure layer dependencies installation script
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(load (expand-file-name "../../lib/deps-install-helpers.el"
                        (file-name-directory
                         load-file-name)) nil t)

(install default-jre-headless)
(with-installed (curl)
  (! "Installing Leiningen...")
  (cp ".lein" (dir $UHOME))
  ($ ["curl -Lo /usr/local/bin/lein"
      "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein"]
     "chown ${UID}:${GID} -R ${UHOME}"
     "chmod 755 /usr/local/bin/lein"
     "su - ${UNAME} -c 'echo exit | lein repl'")
  (! "Installing Boot...")
  (cp ".boot" (dir $UHOME))
  (set-glob-envs '("BOOT_CLOJURE_VERSION" . "1.8.0")
                 `("BOOT_LOCAL_REPO" . ,(concat (dir $UHOME) ".m2/repository")))
  ($ ["curl -Lo /usr/local/bin/boot"
      "https://github.com/boot-clj/boot-bin/releases/download/latest/boot.sh"]
     "chown ${UID}:${GID} -R ${UHOME}"
     "chmod 755 /usr/local/bin/boot"
     "su - ${UNAME} -c 'boot -u'"
     "su - ${UNAME} -c boot"))
