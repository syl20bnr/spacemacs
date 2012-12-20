(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("technomancy" . "http://repo.technomancy.us/emacs/")))
(package-initialize)
(defvar z:my-packages
  '(
    ace-jump-mode
    erlang
    evil
    fill-column-indicator
    key-chord
    p4
    powerline
    rainbow-delimiters
    solarized-theme
    surround
    ))

;;; install missing packages
(let ((not-installed (remove-if 'package-installed-p z:my-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? " (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package z:my-packages)
                   (when (not (package-installed-p package))
                     (package-install package)))))))

;;; initialize packages
(setq z:package-init-dir (concat user-emacs-directory "my-package-init/"))
(message (format "initializing packages out of %s" z:package-init-dir))
(dolist (package (append (mapcar 'car package--builtins) package-activated-list))
    (let* ((initfile (concat z:package-init-dir (format "init-%s.el" package))))
      (if (and (package-installed-p package)
               (file-exists-p initfile))
          (progn (load initfile)
                 (message (format "loaded %s" initfile))))))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'my-packages)
