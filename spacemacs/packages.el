;; installed packages as well as build-in packages with a corresponding
;; init-xxx file
(defvar spacemacs-packages
  '(
    ;; must be initialized first
    evil
    evil-exchange
    evil-leader
    evil-visualstar
    window-numbering
    powerline
    ;; the rest is in alphabetical order
    ac-ispell
    ac-js2
    ace-jump-mode
    auto-complete
    auto-complete-clang
    auto-dictionary
    auto-highlight-symbol
    bookmark
    buffer-move
    cc-mode
    cmake-mode
    color-moccur
    csharp-mode
    coffee-mode
    dash
    deft
    deferred
    diminish
    dired+
    edts
    elisp-slime-nav
    elixir-mix
    elixir-mode
    epc
    erlang
    ess
    ess-R-data-view
    ess-R-object-popup
    ess-smart-underscore
    exec-path-from-shell
    expand-region
    fill-column-indicator
    fish-mode
    flx-ido
    flycheck
    flycheck-color-mode-line
    flycheck-ledger
    flyspell
    fringe-helper
    gist
    git-gutter-fringe
    git-messenger
    git-timemachine
    ghc
    golden-ratio
    google-translate
    haskell-mode
    helm
    helm-css-scss
    helm-c-yasnippet
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
    js2-mode
    js2-refactor
    json-mode
    ledger-mode
    less-css-mode
    magit
    markdown-mode
    monokai-theme
    move-text
    multi-term
    org
    org-bullets
    ;; annoying error message, disable it for now
    ;; org-trello
    p4
    page-break-lines
    paredit
    popup
    popwin
    powershell
    powershell-mode
    projectile
    puppet-mode
    quickrun
    ;; not working well for now
    ;; rainbow-blocks
    rainbow-mode
    rainbow-delimiters
    rainbow-identifiers
    rcirc
    rcirc-color
    recentf
    rfringe
    ruby-end
    ruby-mode
    ruby-test-mode
    s
    scss-mode
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
    weather
    web-mode
    wdired
    yasnippet
    zenburn-theme
    )
  "List of all packages to install and/or initialized. Built-in packages
which require an initialization must be listed explicitly in the list."
)
