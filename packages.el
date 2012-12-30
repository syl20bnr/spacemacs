(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("technomancy" . "http://repo.technomancy.us/emacs/")))
(package-initialize)
(defvar z:packages
  '(
    ace-jump-mode
    auto-complete
    autopair
    deferred
    diminish
    elisp-slime-nav
    epc
    eproject
    erlang
    evil
    expand-region
    fill-column-indicator
    find-file-in-project
    fuzzy
    highlight-symbol
    ido-ubiquitous
    ipython
    jedi
    key-chord
    magit
    multiple-cursors
    multi-term
    p4
    paredit
    powerline
    popup
    puppet-mode
    rainbow-delimiters
    smart-operator
    smex
    solarized-theme
    surround
    webjump
    yasnippet
    ))

;;; install missing packages
(let ((not-installed (remove-if 'package-installed-p z:packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? " (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package z:packages)
                   (when (not (package-installed-p package))
                     (package-install package)))))))

;;; initialize packages
(setq z:package-init-dir (concat user-emacs-directory "init-package/"))
(message (format "initializing packages out of %s" z:package-init-dir))
(dolist (package (append (mapcar 'car package--builtins) package-activated-list))
    (let* ((initfile (concat z:package-init-dir (format "init-%s.el" package))))
      (if (and (package-installed-p package)
               (file-exists-p initfile))
          (progn (load initfile)
                 (message (format "loaded %s" initfile))))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'packages)
