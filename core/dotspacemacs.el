(defconst dotspacemacs-template-directory
  (expand-file-name (concat spacemacs-core-directory "templates/"))
  "Templates directory.")

(defvar dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')"
  )

(defvar dotspacemacs-startup-banner 'random
  "Specify the startup banner. If the value is an integer then the
banner with the corresponding index is used, if the value is `random'
then the banner is chosen randomly among the available banners, if
the value is nil then no banner is displayed.")

(defvar dotspacemacs-configuration-layers '()
  "list of contribution to load."
)

(defvar dotspacemacs-default-theme 'solarized-light
  "Default theme used to start Spacemacs.")

(defvar dotspacemacs-leader-key "SPC"
  "The leader key.")

(defvar dotspacemacs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`")

(defvar dotspacemacs-command-key ":"
  "The key used for Evil commands (ex-commands) and Emacs commands (M-x).
By default the command key is `:' so ex-commands are executed like in Vim
with `:' and Emacs commands are executed with `<leader> :'.")

(defvar dotspacemacs-guide-key-delay 0.4
  "Guide-key delay in seconds.")

(defvar dotspacemacs-fullscreen-at-startup nil
  "If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).")

(defvar dotspacemacs-maximized-at-startup nil
  "If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.")

(defvar dotspacemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters)")

(defvar dotspacemacs-smooth-scrolling t
  "If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
overrides the default behavior of Emacs which recenters the point when
it reaches the top or bottom of the screen.")

(defvar dotspacemacs-feature-toggle-leader-on-jk nil
  "If non nil pressing 'jk' in insert state, ido or helm will activate the
evil leader.")

(defvar dotspacemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar dotspacemacs-smartparens-strict-mode nil
  "If non-nil smartparens-strict-mode will be enabled in programming modes.")

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
      (copy-file (concat dotspacemacs-template-directory
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
