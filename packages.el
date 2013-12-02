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
    evil
    evil-leader
    expand-region
    fill-column-indicator
    flx-ido
    flycheck
    flycheck-color-mode-line
    flyspell
    fringe-helper
    ;; to be deleted
    ;; fuzzy
    gist
    git-gutter-fringe
    ghc
    golden-ratio
    google-translate
    haskell-mode
    helm
    helm-descbinds
    helm-c-yasnippet
    helm-projectile
    helm-themes
    ;; to be deleted
    ;; htmlize
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
    pretty-symbols-mode
    projectile
    puppet-mode
    python
    quickrun
    rainbow-mode
    rainbow-delimiters
    recentf
    rfringe
    ruby-end
    ruby-mode
    ruby-test-mode
    s
    smartparens
    string-edit
    stripe-buffer
    subword
    surround
    tagedit
    twittering-mode
    visual-regexp-steroids
    volatile-highlights
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

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'packages)
