;;; core-dotspacemacs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst dotspacemacs-template-directory
  (expand-file-name (concat spacemacs-core-directory "templates/"))
  "Templates directory.")

(defvar dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')")

(defvar dotspacemacs-startup-banner 'random
   "Specify the startup banner. If the value is an integer then the
   text banner with the corresponding index is used, if the value is
   `random' then the banner is chosen randomly among the available banners,
   if the value is a string then it must be a path to a .PNG file,
   if the value is nil then no banner is displayed.")

(defvar dotspacemacs-configuration-layers '()
  "List of configuration layers to load. If it is the symbol `all' instead
of a list then all discovered layers will be installed.")

(defvar dotspacemacs-themes '(solarized-light
                              solarized-dark
                              leuven
                              monokai
                              zenburn)
  "List of themes, the first of the list is loaded when spacemacs starts.
Press <SPC> T n to cycle to the next theme in the list (works great
with 2 themes variants, one dark and one light")

(defvar dotspacemacs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color.")

(defvar dotspacemacs-leader-key "SPC"
  "The leader key.")

(defvar dotspacemacs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar dotspacemacs-default-font '("Source Code Pro"
                                    :size 13
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.1)
  "Default font. `powerline-scale' allows to quickly tweak the mode-line
size to make separators look not too crappy.")

(defvar dotspacemacs-command-key ":"
  "The key used for Evil commands (ex-commands) and Emacs commands (M-x).
By default the command key is `:' so ex-commands are executed like in Vim
with `:' and Emacs commands are executed with `<leader> :'.")

(defvar dotspacemacs-enable-paste-micro-state t
  "If non nil the paste micro-state is enabled. While enabled pressing `p`
several times cycle between the kill ring content.'")

(defvar dotspacemacs-guide-key-delay 0.4
  "Guide-key delay in seconds.")

(defvar dotspacemacs-loading-progress-bar t
  "If non nil a progress bar is displayed when spacemacs is loading. This
may increase the boot time on some systems and emacs builds, set it to nil
to boost the loading time.")

(defvar dotspacemacs-fullscreen-at-startup nil
  "If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).")

(defvar dotspacemacs-fullscreen-use-non-native nil
  "If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen. Use
to disable fullscreen animations in OSX.")

(defvar dotspacemacs-maximized-at-startup nil
  "If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.")

(defvar dotspacemacs-active-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotspacemacs-inactive-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotspacemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters)")

(defvar dotspacemacs-smooth-scrolling t
  "If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
overrides the default behavior of Emacs which recenters the point when
it reaches the top or bottom of the screen.")

(defvar dotspacemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar dotspacemacs-smartparens-strict-mode nil
  "If non-nil smartparens-strict-mode will be enabled in programming modes.")

(defvar dotspacemacs-delete-orphan-packages t
  "If non-nil spacemacs will delete any orphan packages, i.e. packages that are
declared in a layer which is not a member of
 `dotspacemacs-configuration-layers'")

(defvar dotspacemacs-default-package-repository 'melpa-stable
  "The default package repository used if no explicit repository has been
specified with an installed package.
NOT USED FOR NOW :-)")

(defvar dotspacemacs-excluded-packages '()
  "A list of packages and/or extensions that will not be install and loaded.")

(defvar dotspacemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map emacs-lisp-mode-map)
    (define-key map (kbd "C-c C-c") 'dotspacemacs/sync-configuration-layers)
    map)
  "Keymap for dostpacemacs-mode.")

(define-derived-mode dotspacemacs-mode emacs-lisp-mode "dotspacemacs"
  "dotspacemacs major mode for Spacemacs dotfile.

\\{dotspacemacs-mode-map}"
  :group 'spacemacs
  ;; first import evil-leader keymap for emacs-lisp-mode
  (let ((mode-map (cdr (assoc 'dotspacemacs-mode evil-leader--mode-maps))))
    (unless mode-map
      (push (cons 'dotspacemacs-mode
                  (cdr (assoc 'emacs-lisp-mode evil-leader--mode-maps)))
            evil-leader--mode-maps)))
  ;; then define additional leader key bindings
  (evil-leader/set-key-for-mode 'dotspacemacs-mode
    "mcc" 'dotspacemacs/sync-configuration-layers)
  (run-at-time
   "1 sec" nil
   (lambda () (message "SPC m c c (or C-c C-c) to apply your changes."))))

(defun dotspacemacs/sync-configuration-layers (arg)
  "Synchronize declared layers in dotfile with spacemacs.

If ARG is non nil then `dotspacemacs/config' is skipped."
  (interactive "P")
  (let ((dotspacemacs-loading-progress-bar nil))
    (save-buffer)
    (load-file buffer-file-name)
    (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
    (configuration-layer/sync)
    (if arg
        (message "Done (`dotspacemacs/config' function has been skipped).")
      (dotspacemacs|call-func dotspacemacs/config "Calling dotfile config...")
      (message "Done."))))

(defun dotspacemacs/location ()
  "Return the absolute path to the spacemacs dotfile."
  (concat user-home-directory ".spacemacs"))

(defun dotspacemacs/install ()
  "Install `.spacemacs.template' in home directory. Ask for confirmation
before installing the file if the destination already exists."
  (interactive)
  (let* ((dotfile "~/.spacemacs")
         (install
          (if (file-exists-p dotfile)
              (y-or-n-p
               (format "%s already exists. Do you want to overwite it ? "
                       dotfile)) t)))
    (when install
      (copy-file (concat dotspacemacs-template-directory
                         ".spacemacs.template") dotfile t)
      (message "%s has been installed." dotfile))))

(defun dotspacemacs/load-file ()
  "Load ~/.spacemacs if it exists."
  (let ((dotspacemacs (dotspacemacs/location)))
    (if (file-exists-p dotspacemacs) (load dotspacemacs))))

(defmacro dotspacemacs|call-func (func &optional msg)
  "Call the function from the dotfile only if it is bound.
If MSG is not nil then display a message in `*Messages'."
  `(progn
     (when ,msg (spacemacs/message ,msg))
     (if (fboundp ',func) (,func))))

(provide 'core-dotspacemacs)
