;; Spacemacs Contribution System

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

(defvar spacemacs-all-packages #s(hash-table size 256 data ())
  "Hash table of all declared packages in all layers where the key is a package
symbol and the value is a list of layer symbols responsible for initializing
and configuring the package.")

(defvar spacemacs-all-pre-extensions #s(hash-table size 128 data ())
  "Hash table of all declared pre-extensions in all layers where the key is a
extension symbol and the value is the layer symbols responsible for initializing
and configuring the package.")

(defvar spacemacs-all-post-extensions #s(hash-table size 128 data ())
  "Hash table of all declared post-extensions in all layers where the key is a
extension symbol and the value is the layer symbols responsible for initializing
and configuring the package.")

(defvar spacemacs-contrib-layer-paths #s(hash-table size 128 data ())
  "Hash table of layers locations where the key is a layer symbol and the value
is its path.")

(defvar spacemacs-contrib-categories '()
  "List of strings corresponding to category names. A category is a
sub-directory of the contribution directory.")

(defvar spacemacs-excluded-packages-from-layers '()
  "List of all excluded packages declared at the layer level.")

(defvar dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')"
)

(defvar dotspacemacs-configuration-layers '()
  "list of contribution to load."
)

(defvar dotspacemacs-fullscreen-at-startup nil
  "If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).")

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

(defun contribsys/package.el-initialize ()
  "Initialize package.el"
  (require 'package)
  (unless package--initialized
    (load (concat spacemacs-core-directory "ht.el"))
    (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                             ("gnu" . "http://elpa.gnu.org/packages/")
                             ("melpa" . "http://melpa.org/packages/")))
    (package-initialize)
    ;; Emacs 24.3 and above ships with python.el but in some Emacs 24.3.1 packages
    ;; for Ubuntu, python.el seems to be missing.
    ;; This hack adds marmalade repository for this case only.
    (unless (or (package-installed-p 'python) (version< emacs-version "24.3"))
      (add-to-list 'package-archives
                   '("marmalade" . "http://marmalade-repo.org/packages/")))
    (setq warning-minimum-level :error)))

(defun contribsys/dotfile-location ()
  "Return the absolute path to the spacemacs dotfile."
  (concat user-home-directory ".spacemacs"))

(defun contribsys/load-dotfile ()
  "Load ~/.spacemacs. If it is not found then copy .spacemacs.template to
~/.spacemacs"
  (let ((dotfile (contribsys/dotfile-location)))
    (if (file-exists-p dotfile) (load dotfile))))

(defmacro contribsys/call-dotfile-func (func)
  "Call the function from the dotfile only if it is bound."
  `(if (fboundp ',func) (,func)))

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

(defun contribsys//get-contrib-category-dirs ()
  "Return a list of all absolute paths to the contribution categories stored
in `spacemacs-contrib-categories'"
  (mapcar
   (lambda (d) (expand-file-name
                (concat spacemacs-contrib-config-directory
                        (format "%s/" d))))
   spacemacs-contrib-categories))

(defun contribsys/discover-contrib-layers ()
  "Fill the hash table `spacemacs-contrib-layer-paths' where the key is the
layer symbol and the value is its path."
  (let ((cat-dirs (contribsys//get-contrib-category-dirs)))
    (mapc 'contribsys/discover-contrib-layers-in-dir
          (append (list spacemacs-contrib-config-directory)
                  cat-dirs
                  dotspacemacs-configuration-layer-path))))

(defun contribsys/discover-contrib-layers-in-dir (dir)
  "Fill the hash table `spacemacs-contrib-layer-paths' where the key is the
layer symbol and the value is its path for all layers found in directory DIR.

Also fill the list of excluded packages `spacemacs-excluded-packages-from-layers'
declared at the layer level."
  (message "Looking for contribution layers in %s" dir)
  (ignore-errors
    (let ((files (directory-files dir nil nil 'nosort))
          (filter-out (append spacemacs-contrib-categories '("." ".."))))
      (dolist (f files)
        (when (and (file-directory-p (concat dir f))
                   (not (member f filter-out)))
          (message "-> Discovered contribution layer: %s" f)
          (puthash (intern f) dir spacemacs-contrib-layer-paths))))))

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

(defsubst contribsys//add-layer-to-hash (pkg layer hash)
  "Add LAYER to the list which the value stored in HASH with key PKG."
  (let ((list (ht-get hash pkg)))
    (puthash pkg (add-to-list 'list layer t) hash)))

(defun contribsys//add-excluded-packages (layer)
  "Add excluded packages declared in LAYER."
  (let ((excl-var (intern (format "%s-excluded-packages" (symbol-name layer)))))
    (when (boundp excl-var)
      (setq spacemacs-excluded-packages-from-layers
            (append spacemacs-excluded-packages-from-layers
                    (eval excl-var))))))

(defsubst contribsys//filter-out-excluded-packages ()
  "Remove excluded packages from the hash tables."
  (mapc (lambda (h)
          (dolist (x (ht-keys (eval h)))
            (when (or (member x dotspacemacs-excluded-packages)
                      (member x spacemacs-excluded-packages-from-layers))
              (ht-remove (eval h) x))))
        '(spacemacs-all-packages
          spacemacs-all-pre-extensions
          spacemacs-all-post-extensions)))

(defun contribsys/read-packages-and-extensions ()
  "Load all packages and extensions declared in all layers and fill the
corresponding hash tables:
spacemacs-all-packages
spacemacs-all-pre-extensions
spacemacs-all-post-extensions "
  (dolist (layer (reverse spacemacs-config-layers))
    (let* ((sym (car layer))
           (dir (plist-get (cdr layer) :dir))
           (pkg-file (concat dir "packages.el"))
           (ext-file (concat dir "extensions.el")))
      (progn
        ;; packages
        (when (file-exists-p pkg-file)
          (load pkg-file)
          (dolist (pkg (eval (intern (format "%s-packages" (symbol-name sym)))))
            (contribsys//add-excluded-packages sym)
            (contribsys//add-layer-to-hash pkg sym spacemacs-all-packages)))
        ;; extensions
        (when (file-exists-p ext-file)
          (load ext-file)
          (dolist (pkg (eval (intern (format "%s-pre-extensions"
                                             (symbol-name sym)))))
            (contribsys//add-excluded-packages sym)
            (contribsys//add-layer-to-hash pkg sym
                                           spacemacs-all-pre-extensions))
          (dolist (pkg (eval (intern (format "%s-post-extensions"
                                             (symbol-name sym)))))
            (contribsys//add-excluded-packages sym)
            (contribsys//add-layer-to-hash pkg sym
                                           spacemacs-all-post-extensions))))))
  (contribsys//filter-out-excluded-packages)
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

(defun contribsys/initialize-package (pkg layers)
  "Initialize the package PKG from the configuration layers LAYERS."
  (dolist (layer layers)
    (let* ((init-func (intern (format "%s/init-%s" (symbol-name layer) pkg))))
      (spacemacs/loading-animation)
      (if (and (package-installed-p pkg) (fboundp init-func))
          (progn  (message "(Spacemacs) Initializing %s:%s..."
                           (symbol-name layer) pkg)
                  (funcall init-func))))))

(defun contribsys/initialize-extensions (ext-list)
  "Initialize all the declared extensions in EXT-LIST hash table."
  (ht-each 'contribsys/initialize-extension ext-list))

(defun contribsys/initialize-extension (ext layers)
  "Initialize the extension EXT from the configuration layer LSYM."
  (dolist (layer layers)
    (let* ((l (assq layer spacemacs-config-layers))
           (ext-dir (plist-get (cdr l) :ext-dir))
           (init-func (intern (format "%s/init-%s" (symbol-name layer) ext))))
      (add-to-list 'load-path (format "%s%s/" ext-dir ext))
      (spacemacs/loading-animation)
      (message "(Spacemacs) Initializing %s:%s..." (symbol-name layer) ext)
      (if (fboundp init-func) (funcall init-func)))))

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

(defun contribsys/get-packages-dependencies ()
  "Returns a hash map where key is a dependency package symbol and value is
a list of all packages which depend on it."
  (let ((result #s(hash-table size 200 data ())))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (contribsys/get-package-dependencies pkg-sym)))
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
deleted safely."
  (let ((result '()))
    (dolist (imp-pkg implicit-pkgs)
      (if (contribsys//is-package-orphan imp-pkg dependencies)
          (add-to-list 'result imp-pkg)))
    result))

(defun contribsys//is-package-orphan (pkg dependencies)
  "Returns not nil if PKG is an orphan package."
  (if (ht-contains? spacemacs-all-packages pkg)
      nil
    (if (ht-contains? dependencies pkg)
        (let ((parents (ht-get dependencies pkg)))
          (reduce (lambda (x y) (and x y))
                  (mapcar (lambda (p) (contribsys//is-package-orphan
                                       p dependencies))
                          parents)
                  :initial-value t))
      (not (ht-contains? spacemacs-all-packages pkg)))))

(defun contribsys/get-package-dependencies (package)
  "Return the dependencies alist for PACKAGE."
  (let ((pkg (assq package package-alist)))
    (cond
     ((version< emacs-version "24.4") (aref (cdr pkg) 1))
     (t (package-desc-reqs (cadr pkg))))))

(defun contribsys/get-package-version (package)
  "Return the version string for PACKAGE."
  (let ((pkg (assq package package-alist)))
    (cond
     ((version< emacs-version "24.4")
      (package-version-join (aref (cdr pkg) 0)))
     (t
      (package-version-join (package-desc-version (cadr pkg)))))))

(defun contribsys/package-delete (package)
  "Delete the passed PACKAGE."
  (cond
   ((version< emacs-version "24.4")
    (package-delete (symbol-name package)
                    (contribsys/get-package-version package)))
   (t
    (package-delete (cadr (assq package package-alist))))))

(defun contribsys/delete-orphan-packages ()
  "Delete all the orphan packages."
  (interactive)
  (let* ((dependencies (contribsys/get-packages-dependencies))
         (implicit-packages (contribsys/get-implicit-packages))
         (orphans (contribsys/get-orphan-packages implicit-packages
                                                  dependencies))
         (orphans-count (length orphans)))
    ;; (message "dependencies: %s" dependencies)
    ;; (message "implicit: %s" implicit-packages)
    ;; (message "orphans: %s" orphans)
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
            (contribsys/package-delete orphan)
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
