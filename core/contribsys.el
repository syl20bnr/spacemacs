;; Spacemacs Contribution System
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(setq warning-minimum-level :error)

;; Emacs 24.3 and above ships with python.el but in some Emacs 24.3.1 packages
;; for Ubuntu, python.el seems to be missing.
;; This hack adds marmalade repository for this case only.
(unless (or (package-installed-p 'python) (version< emacs-version "24.3"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/")))

(load (concat spacemacs-core-directory "ht.el"))

(defconst spacemacs-dotspacemacs-version "1.0"
  "Minimum Version exepected for ~/.spacemacs file.")

(defvar spacemacs-config-layers '()
  "Alist of configuration layers with the form (symbol . plist) where
SYMBOL is the name of the layer and PLIST is a property list with the following
keys:
:contrib    if t then the layer is a contribution layer.
:dir        the absolute path to the base directory of the layer.
:ext-dir    the absolute path to the directory containing the extensions.
")

(defvar spacemacs-all-packages #s(hash-table size 200 data ())
  "Hash table of all declared packages in all layers where the key is a package
symbol and the value is the layer symbol where to initialize the package.")

(defvar spacemacs-all-pre-extensions #s(hash-table size 64 data ())
  "Hash table of all declared pre-extensions in all layers where the key is a
extension symbol and the value is the layer symbol where to load and
initialize the extension.")

(defvar spacemacs-all-post-extensions #s(hash-table size 64 data ())
  "Hash table of all declared post-extensions in all layers where the key is a
extension symbol and the value is the layer symbol where to load and
initialize the extension.")

(defvar spacemacs-contrib-layer-paths #s(hash-table size 128 data ())
  "Hash table of layers locations where the key is a layer symbol and the value
is its path.")

(defun contribsys/load-dotfile ()
  "Load ~/.spacemacs. If it is not found then copy .spacemacs.template to
~/.spacemacs"
  (let ((dotfile (concat user-home-directory ".spacemacs")))
    (unless (file-exists-p dotfile)
      (copy-file (concat user-emacs-directory ".spacemacs.template") dotfile))
    (load dotfile)))

(defun contribsys/check-dotspacemacs-version ()
  "Return t if the check pass, otherwise return nil and display an error
message.

The check pass if the expected version stored in
`spacemacs-dotspacemacs-version' is less or equal to the current version of
~/.spacemacs
"
  (if (and (boundp 'dotspacemacs-version)
           (not (version< dotspacemacs-version spacemacs-dotspacemacs-version)))
      t
    (defun display-startup-echo-area-message ()
      "Change the default welcome message of minibuffer to another one."
      (message "Error during loading of Spacemacs."))
    (spacemacs/append-to-buffer
     (format (concat
              "Error: '~/.spacemacs' version mismatch.\n"
              "Found version %s instead of %s.\n\n"
              "Check for recent commits message and look for "
              "'~/.emacs.d/.spacemacs.template' to update your configuration "
              "file.\nIf you have a hard time to update it, feel free to open "
              "an issue.")
             (if (boundp 'dotspacemacs-version)
                 dotspacemacs-version)
             spacemacs-dotspacemacs-version))
    nil))

(defun contribsys/declare-layer (sym &optional contrib)
  "Declare a layer with SYM name (symbol). If CONTRIB is non nil then the layer
 is a contribution layer."
  (let* ((sym-name (symbol-name sym))
         (base-dir (if contrib
                       (ht-get spacemacs-contrib-layer-paths sym)
                     user-emacs-directory))
         (dir (format "%s%s/" base-dir sym-name))
         (ext-dir (format "%sextensions/" dir)))
    (push (cons sym (list :contrib contrib :dir dir :ext-dir ext-dir))
          spacemacs-config-layers)))

(defun contribsys/discover-contrib-layers ()
  "Fill the hash table `spacemacs-contrib-layer-paths' where the key is the
layer symbol and the value is its path."
  (mapc 'contribsys/discover-contrib-layers-in-dir
        (cons spacemacs-contrib-config-directory
              dotspacemacs-configuration-layer-path)))

(defun contribsys/discover-contrib-layers-in-dir (dir)
  "Fill the hash table `spacemacs-contrib-layer-paths' where the key is the
layer symbol and the value is its path for all layers found in directory DIR."
  (message "Looking for contribution layers in %s" dir)
  (ignore-errors
    (let ((files (directory-files dir nil nil 'nosort)))
      (dolist (f files)
        (if (and (file-directory-p (concat dir f))
                 (not (member f '("." ".."))))
            (progn
              (message "-> Discovered contribution layer: %s" f)
              (puthash (intern f) dir spacemacs-contrib-layer-paths)))))))

(defun contribsys/load-layers ()
  "Load all declared layers."
  (contribsys/load-layer-files '("funcs.el" "config.el"))
  (contribsys/read-packages-and-extensions)
  (contribsys/initialize-extensions spacemacs-all-pre-extensions)
  (contribsys/install-packages)
  (spacemacs/append-to-buffer spacemacs-loading-text)
  (contribsys/initialize-packages)
  (contribsys/initialize-extensions spacemacs-all-post-extensions)
  (contribsys/load-layer-files '("keybindings.el")))

(defun contribsys/load-layer-files (files)
  "Load the files of list FILES from all declared layers."
  (dolist (layer (reverse spacemacs-config-layers))
    (let* ((sym (car layer))
           (dir (plist-get (cdr layer) :dir)))
      (dolist (file files)
        (let ((file (concat dir file)))
          (if (file-exists-p file)
              (load file)))))))

(defun contribsys/read-packages-and-extensions ()
  "Load all packages and extensions declared in all layers and fill the
corresponding hash tables:
spacemacs-all-packages
spacemacs-all-pre-extensions
spacemacs-all-post-extensions
By using a hash table we ensure that *only one layer* is responsible for the
initialization of a package or extensions (as well as the loading in case of
extension), the winner layer is the last layer to declare the package or
extension.
"
  (dolist (layer (reverse spacemacs-config-layers))
    (let* ((sym (car layer))
           (dir (plist-get (cdr layer) :dir))
           (pkg-file (concat dir "packages.el"))
           (ext-file (concat dir "extensions.el")))
      (progn 
        ;; packages
        (unless (not (file-exists-p pkg-file))
          (load pkg-file)
          (dolist (pkg (eval (intern (format "%s-packages" (symbol-name sym)))))
            (unless (member pkg dotspacemacs-excluded-packages)
              (puthash pkg sym spacemacs-all-packages))))
        ;; extensions
        (unless (not (file-exists-p ext-file))
          (load ext-file)
          (dolist (pkg (eval (intern (format "%s-pre-extensions"
                                             (symbol-name sym)))))
            (unless (member pkg dotspacemacs-excluded-packages)
              (puthash pkg sym spacemacs-all-pre-extensions)))
          (dolist (pkg (eval (intern (format "%s-post-extensions"
                                             (symbol-name sym)))))
            (unless (member pkg dotspacemacs-excluded-packages)
              (puthash pkg sym spacemacs-all-post-extensions)))))))
  ;; number of chuncks for the loading screen
  (let ((total (+ (ht-size spacemacs-all-packages)
                  (ht-size spacemacs-all-pre-extensions)
                  (ht-size spacemacs-all-post-extensions))))
  (setq spacemacs-loading-dots-chunk-threshold
        (/ total spacemacs-loading-dots-chunk-count))))

(defun contribsys/install-packages ()
  "Install the packages all the packages if there are not currently installed."
  (interactive)
  (let* ((pkg-list (ht-keys spacemacs-all-packages))
         (sorted-pkg-list (mapcar 'intern
                                  (sort (mapcar 'symbol-name pkg-list)
                                        'string<)))
         (not-installed (remove-if 'package-installed-p sorted-pkg-list))
         (not-installed-count (length not-installed)))
    ;; installation
    (if not-installed
        (progn
          (spacemacs/append-to-buffer
           (format "Found %s new package(s) to install...\n"
                   not-installed-count))
          (spacemacs/append-to-buffer
           "--> fetching new package repository indexes...\n")
          (redisplay)
          (package-refresh-contents)
          (setq installed-count 0)
          (dolist (pkg not-installed)
            (setq installed-count (1+ installed-count))
            (when (not (package-installed-p pkg))
              (spacemacs/replace-last-line-of-buffer
               (format "--> installing %s:%s... [%s/%s]"
                       (ht-get spacemacs-all-packages pkg)
                       pkg
                       installed-count
                       not-installed-count) t)
              (package-install pkg))
            (redisplay))
          (spacemacs/append-to-buffer "\n")))))

(defun contribsys/initialize-packages ()
  "Initialize all the declared packages."
  (ht-each 'contribsys/initialize-package spacemacs-all-packages))

(defun contribsys/initialize-package (pkg lsym)
  "Initialize the package PKG from the configuration layer LSYM."
  (let* ((layer (assq lsym spacemacs-config-layers))
         (init-func (intern (format "%s/init-%s" (symbol-name lsym) pkg))))
    (spacemacs/loading-animation)
    (if (and (package-installed-p pkg) (fboundp init-func))
        (progn  (message "(Spacemacs) Initializing %s:%s..."
                         (symbol-name lsym) pkg)
                (funcall init-func)))))

(defun contribsys/initialize-extensions (ext-list)
  "Initialize all the declared extensions in EXT-LIST hash table."
  (ht-each 'contribsys/initialize-extension ext-list))

(defun contribsys/initialize-extension (ext lsym)
  "Initialize the extension EXT from the configuration layer LSYM."
  (let* ((layer (assq lsym spacemacs-config-layers))
         (ext-dir (plist-get (cdr layer) :ext-dir))
         (init-func (intern (format "%s/init-%s" (symbol-name lsym) ext))))
       (add-to-list 'load-path (format "%s%s/" ext-dir ext))
       (spacemacs/loading-animation)
       (message "(Spacemacs) Initializing %s:%s..." (symbol-name lsym) ext)
       (if (fboundp init-func) (funcall init-func))))

(defun contribsys/initialized-packages-count ()
  "Return the number of initialized packages and extensions."
  (+ (ht-size spacemacs-all-packages)
     (ht-size spacemacs-all-pre-extensions)
     (ht-size spacemacs-all-post-extensions)))

(defun contribsys/declare-user-configuration-layers ()
  "Declare the configuration layer in order of appearance in list
dotspacemacs-configuration-layers defined in ~/.spacemacs."
  (if (boundp 'dotspacemacs-configuration-layers)
      (dolist (layer dotspacemacs-configuration-layers)
        (contribsys/declare-layer layer t))))

(defun contribsys/get-layer-property (symlayer prop)
  "Return the value of the PROPerty for the given SYMLAYER symbol."
  (let* ((layer (assq symlayer spacemacs-config-layers)))
         (plist-get (cdr layer) prop)))

(defun contribsys/get-package-dependencies ()
  "Returns a hash map where key is a dependency package symbol and value is
a list of all packages which depend on it."
  (let ((result #s(hash-table size 200 data ())))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (pkg-info (cdr pkg))
             (deps (elt pkg-info 1)))
        (dolist (dep deps)
          (let* ((dep-sym (car dep))
                 (value (ht-get result dep-sym)))
            (puthash dep-sym
                     (if value (add-to-list 'value pkg-sym) (list pkg-sym))
                     result)))))
    result))

(defun contribsys/get-implicit-packages ()
  "Returns a list of all packages in `packages-alist' which are not found
in `spacemacs-all-packages'"
  (let ((imp-pkgs))
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (if (not (ht-contains? spacemacs-all-packages pkg-sym))
            (add-to-list 'imp-pkgs pkg-sym))))
    imp-pkgs))

(defun contribsys/get-orphan-packages (implicit-pkgs dependencies)
  "Return a list of all orphan packages which are basically meant to be
deleted safely. Orphan packages are packages whose all dependent packages
are not in `spacemacs-all-packages' (explicit packages)"
  (let ((result '()))
    (dolist (imp-pkg implicit-pkgs)
      (setq result (append result (contribsys//get-orphan-packages2
                                   imp-pkg dependencies '()))))
    (delete-dups result)))

(defun contribsys//get-orphan-packages2 (imp-pkg dependencies acc)
  "Reccursive function to get the orphans packages as well as their
orphan dependencies."
  (if (ht-contains? dependencies imp-pkg)
      (dolist (parent (ht-get dependencies imp-pkg))
        (let ((orphans (contribsys//get-orphan-packages2
                        parent dependencies acc)))
          (unless (not orphans)
            (add-to-list 'acc imp-pkg)
            (if acc (setq acc (append acc orphans))
              (setq acc orphans)))))
    (unless (ht-contains? spacemacs-all-packages imp-pkg)
      (if acc (add-to-list 'acc imp-pkg) (setq acc (list imp-pkg)))))
  acc)

(defun contribsys/get-package-version (package)
  "Return the version string for PACKAGE."
  (package-version-join (aref (cdr (assq package package-alist)) 0)))

(defun contribsys/delete-orphan-packages ()
  "Delete all the orphan packages."
  (interactive)
  (let* ((dependencies (contribsys/get-package-dependencies))
         (implicit-packages (contribsys/get-implicit-packages))
         (orphans (contribsys/get-orphan-packages implicit-packages
                                                  dependencies))
         (orphans-count (length orphans)))
    (if orphans
        (progn
          ;; for the loading dot bar
          (spacemacs/append-to-buffer "OK!\n")
          (spacemacs/append-to-buffer
           (format "Found %s orphan package(s) to delete...\n"
                   orphans-count))
          (setq deleted-count 0)
          (dolist (orphan orphans)
            (setq deleted-count (1+ deleted-count))
            (spacemacs/replace-last-line-of-buffer
             (format "--> deleting %s... [%s/%s]"
                     orphan
                     deleted-count
                     orphans-count) t)
            (package-delete (symbol-name orphan)
                            (contribsys/get-package-version orphan))
            (redisplay))
          (spacemacs/append-to-buffer "\n"))
      (message "No orphan package to delete."))))

(defun contribsys/setup-after-init-hook ()
  "Add post init processing."
  (add-hook 'after-init-hook
            (lambda ()
              (spacemacs/append-to-buffer (format "%s\n" spacemacs-loading-done-text))
              ;; from jwiegley
              ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
              (let ((elapsed (float-time
                              (time-subtract (current-time) emacs-start-time))))
                (spacemacs/append-to-buffer
                 (format "[%s packages loaded in %.3fs]\n"
                         (contribsys/initialized-packages-count)
                         elapsed))))))
