#!/usr/bin/emacs --script
;;; install.el --- html layer dependencies installation script
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

(with-installed (git curl)
  (install rubygems npm)
  (!"Installing npm stuff...")
  ($ "npm cache clean -f"
     "npm install -g n"
     "n stable"
     "curl -L https://npmjs.org/install.sh | sh"
     "npm install csslint -g"
     "npm install pug"
     "npm install pug-cli -g"
     "npm install pug-lint -g")
  (!"Installing ruby stuff...")
  ($ "gem install rubygems-update"
     "update_rubygems"
     "gem update --system"
     "gem install rake"
     "gem install slim"
     "gem install slim_lint"
     "gem install sass"
     "gem install specific_install"
     ["gem specific_install"
      "https://github.com/brigade/scss-lint.git"]
     ["gem specific_install"
      "https://github.com/Sweetchuck/scss_lint_reporter_checkstyle.git"]))
