;;; auto-layer.el --- auto-mode-alist entries for layer installation
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; TODO comments placeholder means the utility function to insert form has failed

;; agda
(configuration-layer/lazy-install 'ansible :extensions '("\\(\\.jinja2\\'\\|\\.j2\\'\\)" jinja2-mode))
(configuration-layer/lazy-install 'asciidoc :extensions '("\\(\\.adoc?$\\)" adoc-mode))
(configuration-layer/lazy-install 'autohotkey :extensions '("\\(\\.ahk\\'\\|\\.ahk$\\)" ahk-mode))
(configuration-layer/lazy-install 'clojure :extensions '("\\(\\.\\(clj\\|dtm\\|edn\\)\\'\\|\\(?:build\\|profile\\)\\.boot\\'\\|\\.boot\\'\\)" clojure-mode))
(configuration-layer/lazy-install 'clojure :extensions '("\\(\\.cljs\\)" clojurescript-mode))
(configuration-layer/lazy-install 'clojure :extensions '("\\(\\.cljc\\)" clojurec-mode))
(configuration-layer/lazy-install 'clojure :extensions '("\\(\\.cljx\\)" clojurex-mode))
;; common-lisp
;; csharp
(configuration-layer/lazy-install 'csv :extensions '("\\(\\.[Cc][Ss][Vv]\\'\\)" csv-mode))
(configuration-layer/lazy-install 'd :extensions '("\\(\\.d[i]?\\'\\)" d-mode))
(configuration-layer/lazy-install 'dart :extensions '("\\(\\.dart\\'\\)" dart-mode))
(configuration-layer/lazy-install 'elixir :extensions '("\\.\\(ex\\|exs\\|elixir\\)\\'" elixir-mode))
(configuration-layer/lazy-install 'elm :extensions '("\\(\\.elm\\'\\)" elm-mode))
(configuration-layer/lazy-install 'erlang :extensions '("\\(\\.erl$\\|\\.app\\.src$\\|\\.escript\\|\\.hrl$\\|\\.xrl$\\|\\.yrl\\|/ebin/.+\\.app\\|\\.erl\\'\\|\\.hrl\\'\\)" erlang-mode))
;; ess
(configuration-layer/lazy-install 'finance :extensions '("\\(\\.\\(ledger\\|ldg\\)\\'\\)" ledger-mode))
(configuration-layer/lazy-install 'fsharp :extensions '("\\(\\.fs[iylx]?$\\)" fsharp-mode))
(configuration-layer/lazy-install 'go :extensions '("\\(\\.go\\'\\)" go-mode))
;; groovy
(configuration-layer/lazy-install 'graphviz :extensions '("\\(\\.dot\\'\\|\\.gv\\'\\|\\.diag\\'\\|\\.blockdiag\\'\\|\\.nwdiag\\'\\|\\.rackdiag\\'\\)" graphviz-dot-mode))
(configuration-layer/lazy-install 'haskell :extensions '("\\(\\.cmm\\'\\)" cmm-mode))
(configuration-layer/lazy-install 'haskell :extensions '("\\(\\.[gh]s\\'\\|\\.hsc\\'\\)" haskell-mode))
(configuration-layer/lazy-install 'html :extensions '("\\(\\.css\\'\\)" css-mode))
(configuration-layer/lazy-install 'html :extensions '("\\(\\.haml\\'\\)" haml-mode))
(configuration-layer/lazy-install 'html :extensions '("\\(\\.jade\\'\\)" jade-mode))
(configuration-layer/lazy-install 'html :extensions '("\\(\\.less\\'\\)" less-css-mode))
(configuration-layer/lazy-install 'html :extensions '("\\(\\.sass\\'\\)" sass-mode))
(configuration-layer/lazy-install 'html :extensions '("\\(\\.scss\\'\\)" scss-mode))
(configuration-layer/lazy-install 'html :extensions '("\\(\\.slim\\'\\)" slim-mode))
(configuration-layer/lazy-install 'html :extensions '("\\(\\.phtml\\'\\|\\.tpl\\.php\\'\\|\\.twig\\'\\|\\.html\\'\\|\\.htm\\'\\|\\.[gj]sp\\'\\|\\.as[cp]x?\\'\\|\\.eex\\'\\|\\.erb\\'\\|\\.mustache\\'\\|\\.handlebars\\'\\|\\.hbs\\'\\|\\.eco\\'\\|\\.ejs\\'\\|\\.djhtml\\'\\)" web-mode))
(configuration-layer/lazy-install 'idris :extensions '("\\(\\.idr$\\|\\.lidr$\\)" idris-mode))
;; javascript
(configuration-layer/lazy-install 'javascript :extensions '("\\(\\.coffee\\'\\|\\.iced\\'\\|Cakefile\\'\\|\\.cson\\'\\)" coffee-mode))
(configuration-layer/lazy-install 'javascript :extensions '("\\(\\.js\\'\\)" js2-mode))
(configuration-layer/lazy-install 'javascript :extensions '("\\(\\.json$\\)" json-mode))
(configuration-layer/lazy-install 'react :extensions '("\\(\\.jsx$\\)" react-mode))
;; latex
(configuration-layer/lazy-install 'lua :extensions '("\\(\\.lua$\\|\\.lua\\'\\)" lua-mode))
(configuration-layer/lazy-install 'nginx :extensions '("\\(nginx\\.conf\\'\\|/nginx/.+\\.conf\\'\\)" nginx-mode))
(configuration-layer/lazy-install 'octave :extensions '("\\(\\.m\\'\\)" octave-mode))
(configuration-layer/lazy-install 'markdown :extensions '("\\(\\.markdown\\'\\|\\.md\\'\\|\\.m[k]d\\)" markdown-mode))
(configuration-layer/lazy-install 'ocaml :extensions '("\\(\\.ml[ip]?\\'\\|\\.eliomi?\\'\\)" tuareg-mode))
(configuration-layer/lazy-install 'perl5 :extensions '("\\.\\(p[lm]x?\\|P[LM]X?\\)\\'" cperl-mode))
(configuration-layer/lazy-install 'php :extensions '("\\(\\.amk\\'\\|/Amkfile\\'\\|\\.phtml\\'\\|\\.php[s345t]?\\'\\|[^/]\\.\\(module\\|test\\|install\\|profile\\|tpl\\.php\\|theme\\|inc\\)\\'\\|\\.php\\'\\)" php-mode))
(configuration-layer/lazy-install 'protobuf :extensions '("\\(\\.proto\\'\\)" protobuf-mode))
(configuration-layer/lazy-install 'makepkg :extensions '("\\`PKGBUILD\\'" pkgbuild-mode))
(configuration-layer/lazy-install 'purescript :extensions '("\\(\\.purs\\'\\)" purescript-mode))
(configuration-layer/lazy-install 'python :extensions '("\\(\\.pyx\\'\\|\\.pxd\\'\\|\\.pxi\\'\\)" cython-mode))
(configuration-layer/lazy-install 'python :extensions '("\\(\\.hy\\'\\)" hy-mode))
(configuration-layer/lazy-install 'python :extensions '("\\(\\.pip\\'\\|requirements\\(?:.\\|\n\\)*\\.txt\\'\\)" pip-requirements-mode))
(configuration-layer/lazy-install 'python :extensions '("\\(\\.py\\'\\)" python-mode) :interpreter '("python[0-9.]*" python-mode))
(configuration-layer/lazy-install 'racket :extensions '("\\(\\.rkt[dl]?\\'\\)" racket-mode))
;; TODO check how to support for the layer variable `restclient-use-org'
(configuration-layer/lazy-install 'restclient :extensions '("\\(\\.http\\'\\)" restclient-mode))
(configuration-layer/lazy-install 'ruby
  :extensions '("\\(\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'\\|Puppetfile\\)" ruby-mode))
(configuration-layer/lazy-install 'rust :extensions '("\\(\\.rs\\'\\)" rust-mode))
(configuration-layer/lazy-install 'rust :extensions '("\\(\\.toml$\\)" toml-mode))
;; scala
;; scheme
(configuration-layer/lazy-install 'sml :extensions '("\\(\\.s\\(ml\\|ig\\)\\'\\|\\.\\(sml\\|sig\\)\\'\\)" sml-mode))
(configuration-layer/lazy-install 'sql :extensions '("\\(\\.sql\\'\\)" sql-mode))
(configuration-layer/lazy-install 'swift :extensions '("\\(\\.swift\\'\\)" swift-mode))
(configuration-layer/lazy-install 'systemd :extensions'("\\(\\.\\(nspawn\\|automount\\|busname\\|mount\\|service\\|slice\\|socket\\|swap\\|target\\|timer\\|link\\|netdev\\|network\\)\\'\\)" systemd-mode))
(configuration-layer/lazy-install 'shell-scripts :extensions '("\\(\\.fish\\'\\|/fish_funced\\..*\\'\\)" fish-mode))
(configuration-layer/lazy-install 'typescript :extensions '("\\(\\.ts$\\)" typescript-mode))
(configuration-layer/lazy-install 'vimscript :extensions '("\\(\\.vim\\'\\|[._]?g?vimrc\\'\\|\\.exrc\\'\\|_vimrc\\'\\|\\.vim[rc]?\\'\\)" vimrc-mode))
(configuration-layer/lazy-install 'vimscript :extensions '("\\(_vimperatorrc\\'\\|_pentadactylrc\\'\\|\\.penta\\'\\|vimperatorrc\\'\\|\\.vimp\\'\\|pentadactylrc\\'\\)" dactyl-mode))
(configuration-layer/lazy-install 'windows-scripts :extensions '("\\(\\.bat$\\)" dos-mode))
(configuration-layer/lazy-install 'windows-scripts :extensions '("\\(\\.ps[dm]?1\\'\\|\\.ps1$\\)" powershell-mode))
(configuration-layer/lazy-install 'yaml :extensions '("\\(\\.e?ya?ml$\\|\\.\\(yml\\|yaml\\)\\'\\|Procfile\\'\\)" yaml-mode))
