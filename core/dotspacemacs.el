(defvar dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')"
)

(defvar dotspacemacs-configuration-layers '()
  "list of contribution to load."
)

(defvar dotspacemacs-fullscreen-at-startup nil
  "If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).")

(defvar dotspacemacs-smooth-scrolling t
  "If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
overrides the default behavior of Emacs which recenters the point when
it reaches the top or bottom of the screen.")

(defvar dotspacemacs-feature-toggle-leader-on-jk nil
  "If non nil pressing 'jk' in insert state, ido or helm will activate the
evil leader.")

(defvar dotspacemacs-default-package-repository 'melpa-stable
  "The default package repository used if no explicit repository has been
specified with an installed package.
NOT USED FOR NOW :-)"
)

(defvar dotspacemacs-excluded-packages '()
  "A list of packages and/or extensions that will not be install and loaded.")

(defun dotspacemacs/location ()
  "Return the absolute path to the spacemacs dotfile."
  (concat user-home-directory ".spacemacs"))

(defun dotspacemacs/install ()
  "Install `.spacemacs.template' in home directory. Ask for confirmation
before installing the file if the destination already exists."
  (interactive)
  (let* ((dotfile "~/.spacemacs")
         (install (if (file-exists-p dotfile)
                      (y-or-n-p (format "%s already exists. Do you want to overwite it ? "
                                        dotfile))
                    t)))
    (when install
      (copy-file (concat spacemacs-template-directory
                         ".spacemacs.template") dotfile t)
      (message "%s has been installed." dotfile))))

(defun dotspacemacs/load ()
  "Load ~/.spacemacs. If it is not found then copy .spacemacs.template to
~/.spacemacs"
  (let ((dotspacemacs (dotspacemacs/location)))
    (if (file-exists-p dotspacemacs) (load dotspacemacs))))

(defmacro dotspacemacs|call-func (func)
  "Call the function from the dotfile only if it is bound."
  `(if (fboundp ',func) (,func)))

(provide 'dotspacemacs)
