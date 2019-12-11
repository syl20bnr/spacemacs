;;; packages.el --- search-engine Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq search-engine-packages
      '(
        engine-mode))


(defun search-engine/init-engine-mode ()
  (use-package engine-mode
    :commands (defengine spacemacs/search-engine-select)
    :defines search-engine-alist
    :init
    (progn
      (spacemacs/set-leader-keys
        "a/" 'spacemacs/search-engine-select)
      (setq search-engine-alist
            `((amazon
               :name "Amazon"
               :url "https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%%3Daps&field-keywords=%s")
              (bing
               :name "Bing"
               :url "https://www.bing.com/search?q=%s")
              (duck-duck-go
               :name "Duck Duck Go"
               :url "https://duckduckgo.com/?q=%s")
              (ecosia
               :name "Ecosia"
               :url "https://www.ecosia.org/search?q=%s")
              (google
               :name "Google"
               :url "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
              (google-images
               :name "Google Images"
               :url "https://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
              (github
               :name "GitHub"
               :url "https://github.com/search?ref=simplesearch&q=%s")
              (google-maps
               :name "Google Maps"
               :url "https://maps.google.com/maps?q=%s")
              (twitter
               :name "Twitter"
               :url "https://twitter.com/search?q=%s")
              (project-gutenberg
               :name "Project Gutenberg"
               :url "https://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=&query=%s")
              (youtube
               :name "YouTube"
               :url "https://www.youtube.com/results?aq=f&oq=&search_query=%s")
              (stack-overflow
               :name "Stack Overflow"
               :url "https://stackoverflow.com/search?q=%s")
              (spacemacs-issues
               :name "Spacemacs Issues"
               :url "https://github.com/syl20bnr/spacemacs/issues?utf8=%%E2%%9C%%93&q=is%%3Aissue+is%%3Aopen+%s")
              (spacemacs-pullrequests
               :name "Spacemacs Pull Requests"
               :url "https://github.com/syl20bnr/spacemacs/pulls?utf8=%%E2%%9C%%93&q=is%%3Aissue+is%%3Aopen+%s")
              (wikipedia
               :name "Wikipedia"
               :url "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
              (maven
               :name "Maven Central"
               :url "https://search.maven.org/search?q=%s")
              (npm
               :name "Npmjs"
               :url "https://www.npmjs.com/search?q=%s")
              (hoogle
               :name "Hoogle 5"
               :url "https://hoogle.haskell.org/?hoogle=%s")
              (haskell-packages
               :name "Hackage Package Search"
               :url "https://hackage.haskell.org/packages/search?terms=%s")
              (clojure
               :name "Clojure Docs"
               :url "https://clojuredocs.org/search?q=%s")
              (pip
               :name "Python Package Index"
               :url "https://pypi.org/search/?q=%s")
              (python-doc
               :name "Python Docs"
               :url "https://docs.python.org/3/search.html?q=%s")
              (c++-api-reference
               :name "C++ Reference"
               :url "https://en.cppreference.com/mwiki/index.php?search=%s")
              (wolfram-alpha
               :name "Wolfram Alpha"
               :url "https://www.wolframalpha.com/input/?i=%s")
              (ctan
               :name "CTAN"
               :url "https://ctan.org/search?phrase=%s")
              ,@search-engine-config-list))
      (dolist (engine search-engine-alist)
        (let ((func (intern (format "engine/search-%S" (car engine)))))
          (autoload func "engine-mode" nil 'interactive))))
    :config
    (progn
      (engine-mode t)
      (dolist (engine search-engine-alist)
        (let* ((cur-engine (car engine))
               (engine-url (plist-get (cdr engine) :url))
               (engine-keywords (plist-get (cdr engine) :keywords)))
          (eval `(defengine ,cur-engine ,engine-url ,@engine-keywords)))))))
