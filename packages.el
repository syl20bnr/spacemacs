(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("technomancy" . "http://repo.technomancy.us/emacs/")))
(package-initialize)
(defvar syl:packages
  '(
    ace-jump-mode
    auto-complete
    auto-dictionary
    autopair
    buffer-move
    color-moccur
    deft
    deferred
    diminish
    dired+
    elisp-slime-nav
    epc
    erc
    erlang
    evil
    evil-leader
    expand-region
    ;; fill-column-indicator
    find-file-in-project
    flymake
    flymake-cursor
    flymake-easy
    flymake-haskell-multi
    fuzzy
    ghc
    google-translate
    haskell-mode
    helm
    helm-c-yasnippet
    helm-projectile
    htmlize
    jedi
    json-mode
    key-chord
    less-css-mode
    magit
    markdown-mode
    move-text
    nose
    org
    ;; p4
    page-break-lines
    paredit
    powerline
    popup
    projectile
    puppet-mode
    python
    quickrun
    rainbow-delimiters
    recentf
    rfringe
    ruby-electric
    ruby-mode
    ruby-test-mode
    smart-operator
    sr-speedbar
    stripe-buffer
    surround
    tagedit
    twittering-mode
    wdired
    window-layout
    yasnippet
    ))

;;; install missing packages
(let ((not-installed (remove-if 'package-installed-p syl:packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? " (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package syl:packages)
                   (when (not (package-installed-p package))
                     (package-install package)))))))

;;; initialize packages
(setq syl:package-init-dir (concat user-emacs-directory "init-package/"))
(message (format "initializing packages out of %s" syl:package-init-dir))
(dolist (package (append (mapcar 'car package--builtins) package-activated-list))
    (let* ((initfile (concat syl:package-init-dir (format "init-%s.el" package))))
      (if (and (package-installed-p package)
               (file-exists-p initfile))
          (progn (load initfile)
                 (message (format "loaded %s" initfile))))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'packages)
