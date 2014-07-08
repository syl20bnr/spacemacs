(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(defvar syl:packages
  '(
    ac-ispell
    ace-jump-mode
    ample-theme
    anti-zenburn-theme
    auto-complete
    auto-complete-clang
    auto-dictionary
    bookmark
    buffer-move
    cc-mode
    cmake-mode
    color-moccur
    csharp-mode
    dash
    deft
    deferred
    diminish
    dired+
    elisp-slime-nav
    elixir-mix
    elixir-mode
    epc
    erc
    erlang
    ess
    ess-R-data-view
    ess-R-object-popup
    ess-smart-underscore
    evil
    evil-exchange
    evil-leader
    evil-matchit
    evil-visualstar
    expand-region
    fill-column-indicator
    flx-ido
    flycheck
    flycheck-color-mode-line
    flyspell
    fringe-helper
    gist
    git-gutter-fringe
    git-messenger
    ghc
    golden-ratio
    google-translate
    haskell-mode
    helm
    helm-css-scss
    helm-c-yasnippet
    helm-helm-commands
    helm-descbinds
    helm-make
    helm-mode-manager
    ;; not working for now
    ;; helm-proc
    helm-projectile
    helm-swoop
    helm-themes
    howdoi
    hy-mode
    ido-vertical-mode
    jedi
    json-mode
    less-css-mode
    magit
    markdown-mode
    monokai-theme
    move-text
    multi-term
    org
    org-bullets
    org-trello
    p4
    page-break-lines
    paredit
    popup
    popwin
    powershell
    powershell-mode
    projectile
    puppet-mode
;;    python
    quickrun
    ;; not working well for now
    ;; rainbow-blocks
    rainbow-mode
    rainbow-delimiters
    rainbow-identifiers
    recentf
    rfringe
    ruby-end
    ruby-mode
    ruby-test-mode
    s
    smartparens
    smeargle
    string-edit
    stripe-buffer
    subword
    surround
    tagedit
    twittering-mode
    visual-regexp-steroids
    volatile-highlights
    wand
    wdired
    yasnippet
    zenburn-theme
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
(dolist (package (append (mapcar 'car package--builtins) package-activated-list))
    (let* ((initfile (concat syl:package-init-dir (format "init-%s.el" package))))
      (if (and (package-installed-p package)
               (file-exists-p initfile))
          (load initfile))))

(provide 'packages)
