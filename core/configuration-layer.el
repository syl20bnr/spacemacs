;;; configuration-layer.el --- Spacemacs Core File
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
()

(require 'dotspacemacs)
(require 'ht)
(require 'package)
(require 'spacemacs-buffer)

(unless package--initialized
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")))
  ;; optimization, no need to activate all the packages so early
  (package-initialize 'noactivate)
  ;; Emacs 24.3 and above ships with python.el but in some Emacs 24.3.1 packages
  ;; for Ubuntu, python.el seems to be missing.
  ;; This hack adds marmalade repository for this case only.
  (unless (or (package-installed-p 'python) (version< emacs-version "24.3"))
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/")))
  (setq warning-minimum-level :error))

(defconst configuration-layer-template-directory
  (expand-file-name (concat spacemacs-core-directory "templates/"))
  "Configuration layer templates directory.")

(defconst configuration-layer-contrib-directory
  (expand-file-name (concat user-emacs-directory "contrib/"))
  "Spacemacs contribution layers base directory.")

(defconst configuration-layer-private-directory
  (expand-file-name (concat user-emacs-directory "private/"))
  "Spacemacs private layers base directory.")

(defvar configuration-layer-layers '()
  "Alist of declared configuration layers.")

(defvar configuration-layer-paths (make-hash-table :size 128)
  "Hash table of layers locations. The key is a layer symbol and the value is
the path for this layer.")

(defvar configuration-layer-all-packages (make-hash-table :size 256)
  "Hash table of all declared packages in all layers where the key is a package
symbol and the value is a list of layer symbols responsible for initializing
and configuring the package.")

(defvar configuration-layer-all-packages-sorted '()
  "Sorted list of all package symbols.")

(defvar configuration-layer-all-pre-extensions (make-hash-table :size 128)
  "Hash table of all declared pre-extensions in all layers where the key is a
extension symbol and the value is the layer symbols responsible for initializing
and configuring the package.")

(defvar configuration-layer-all-pre-extensions-sorted '()
  "Sorted list of all pre extensions symbols.")

(defvar configuration-layer-all-post-extensions (make-hash-table :size 128)
  "Hash table of all declared post-extensions in all layers where the key is a
extension symbol and the value is the layer symbols responsible for initializing
and configuring the package.")

(defvar configuration-layer-all-post-extensions-sorted '()
  "Sorted list of all post extensions symbols.")

(defvar configuration-layer-contrib-categories '("usr" "lang")
  "List of strings corresponding to category names. A category is a
sub-directory of the contribution directory.")

(defvar configuration-layer-excluded-packages '()
  "List of all excluded packages declared at the layer level.")

(defvar configuration-layer--loaded-files '()
  "List of loaded files.")

(defun configuration-layer/create-layer (name)
  "Ask the user for a configuration layer name and create a layer with this
name in the private layers directory."
  (interactive "sConfiguration layer name: ")
  (let ((layer-dir (configuration-layer//get-private-layer-dir name)))
    (cond
     ((string-equal "" name)
      (message "Cannot create a configuration layer without a name."))
     ((file-exists-p layer-dir)
      (message "Cannot create configuration layer \"%s\", this layer already exists."
               name))
     (t
      (make-directory layer-dir)
      (configuration-layer//copy-template "extensions")
      (configuration-layer//copy-template "packages")
      (message "Configuration layer \"%s\" successfully created." name))
  )))

(defun configuration-layer//get-private-layer-dir (name)
  "Return an absolute path the the private configuration layer with name
NAME."
  (concat configuration-layer-private-directory name "/"))

(defun configuration-layer//copy-template (template)
  "Copy and replace special values of TEMPLATE to LAYER_DIR."
  (let ((src (concat configuration-layer-template-directory
                     (format "%s.template" template)))
        (dest (concat (configuration-layer//get-private-layer-dir name)
                      (format "%s.el" template))))
    
    (copy-file src dest)
    (find-file dest)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "NAME" nil t)
        (replace-match name t)))
    (save-buffer)))

(defun configuration-layer//get-contrib-category-dirs ()
  "Return a list of all absolute paths to the contribution categories stored
in `configuration-layer-contrib-categories'"
  (mapcar
   (lambda (d) (expand-file-name
                (concat configuration-layer-contrib-directory (format "%s/" d))))
   configuration-layer-contrib-categories))

(defun configuration-layer//discover-layers ()
  "Return a hash table where the key is the layer symbol and the value is its
path."
  (let ((cat-dirs (configuration-layer//get-contrib-category-dirs))
        (result (make-hash-table :size 128)))
    ;; add spacemacs layer
    (puthash 'spacemacs (expand-file-name user-emacs-directory) result)
    (mapc (lambda (dir)
            (let ((layers (configuration-layer//discover-layers-in-dir dir)))
              (mapc (lambda (layer)
                      (puthash (car layer) (cdr layer) result))
                    layers)))
          (append (list configuration-layer-contrib-directory)
                  cat-dirs
                  dotspacemacs-configuration-layer-path
                  ;; load private layers at the end on purpose
                  ;; we asume that the user layers must have the final word
                  ;; on configuration choices.
                  (list configuration-layer-private-directory)))
    result))

(defun configuration-layer//discover-layers-in-dir (dir)
  "Return an alist where the key is a layer symbol and the value is the path
for that layer."
  (spacemacs/message "Looking for configuration layers in %s" dir)
  (ignore-errors
    (let ((files (directory-files dir nil nil 'nosort))
          (filter-out (append configuration-layer-contrib-categories '("." "..")))
          result '())
      (dolist (f files)
        (when (and (file-directory-p (concat dir f))
                   (not (member f filter-out)))
          (spacemacs/message "-> Discovered configuration layer: %s" f)
          (push (cons (intern f) dir) result)))
      result)))

(defun configuration-layer/declare-all-layers ()
  "Declare default layers and user layers from the dotfile by filling the
`configuration-layer-layers' variable."
  (setq configuration-layer-paths (configuration-layer//discover-layers))
  (push (configuration-layer//declare-layer 'spacemacs)
        configuration-layer-layers)
  (mapc (lambda (layer) (push layer configuration-layer-layers))
        (configuration-layer//declare-layers
         dotspacemacs-configuration-layers)))

(defun configuration-layer//declare-layers (layers)
  "Declare the passed configuration LAYERS.
LAYERS is a list of layer symbols."
  (reduce (lambda (acc elt) (push elt acc))
          (mapcar 'configuration-layer//declare-layer layers)
          :initial-value nil))

(defun configuration-layer//declare-layer (layer)
  "Declare a layer with NAME symbol. Return a cons cell (symbol . plist)
where `symbol' is the name of the layer and `plist' is a property list with
the following keys:
- `:dir'       the absolute path to the base directory of the layer.
- `:ext-dir'   the absolute path to the directory containing the extensions.
- `:variables' list of layer configuration variables to set
- `:excluded'  list of packages to exlcude."
  (let* ((name-sym (if (listp layer) (car layer) layer))
         (name-str (symbol-name name-sym))
         (base-dir (configuration-layer/get-layer-path name-sym)))
    (if base-dir
        (let* ((dir (format "%s%s/" base-dir name-str))
               (ext-dir (format "%sextensions/" dir))
               (plist (append (list :dir dir :ext-dir ext-dir)
                              (when (listp layer) (cdr layer)))))
          (cons name-sym plist))
      (spacemacs/message "Warning: Cannot find layer %s !" layer))))

(defun configuration-layer//set-layers-variables (layers)
  "Set the configuration variables for the passed LAYERS."
  (dolist (layer layers)
    (let ((variables (configuration-layer//mplist-get layer :variables)))
      (while variables
        (let ((var (pop variables)))
          (if (consp variables)
              (set-default var (pop variables))
            (spacemacs/message "Warning: Missing value for variable %s !"
                               var)))))))

(defun configuration-layer/package-declaredp (pkg)
  "Return non-nil if PKG symbol corresponds to a used package."
  (ht-contains? configuration-layer-all-packages pkg))

(defun configuration-layer/get-layers-list ()
  "Return a list of all discovered layer symbols."
  (ht-keys configuration-layer-paths))

(defun configuration-layer/get-layer-path (layer)
  "Return the path for LAYER symbol."
  (ht-get configuration-layer-paths layer))

(defun configuration-layer/load-layers ()
  "Load all declared layers."
  (let ((layers (reverse configuration-layer-layers)))
    (configuration-layer//set-layers-variables layers)
    (configuration-layer//load-layer-files layers '("funcs.el" "config.el"))
    ;; fill the hash tables
    (setq configuration-layer-excluded-packages (configuration-layer/get-excluded-packages layers))
    (setq configuration-layer-all-packages (configuration-layer/get-packages layers))
    (setq configuration-layer-all-pre-extensions (configuration-layer/get-extensions layers t))
    (setq configuration-layer-all-post-extensions (configuration-layer/get-extensions layers))
    ;; This is what you get when you have no test cases... hopefully I will code
    ;; them soon :-)
    ;; (message "excluded: %s" configuration-layer-excluded-packages)
    ;; (message "packages: %s" configuration-layer-all-packages)
    ;; (message "pre-extensions: %s" configuration-layer-all-pre-extensions)
    ;; (message "post-extensions: %s" configuration-layer-all-post-extensions)
    ;; filter them
    (let ((excluded (append dotspacemacs-excluded-packages
                            configuration-layer-excluded-packages)))
      (configuration-layer//filter-out-excluded configuration-layer-all-packages excluded)
      (configuration-layer//filter-out-excluded configuration-layer-all-pre-extensions excluded)
      (configuration-layer//filter-out-excluded configuration-layer-all-post-extensions excluded))
    ;; number of chuncks for the loading screen
    (let ((total (+ (ht-size configuration-layer-all-packages)
                    (ht-size configuration-layer-all-pre-extensions)
                    (ht-size configuration-layer-all-post-extensions))))
      (setq spacemacs-loading-dots-chunk-threshold
            (/ total spacemacs-loading-dots-chunk-count)))
    ;; filter them
    (configuration-layer//sort-packages-and-extensions)
    ;; install and initialize packages and extensions
    (configuration-layer//initialize-extensions configuration-layer-all-pre-extensions-sorted t)
    (configuration-layer//install-packages)
    (spacemacs/append-to-buffer spacemacs-loading-text)
    (configuration-layer//initialize-packages)
    (configuration-layer//initialize-extensions configuration-layer-all-post-extensions-sorted)
    ;; restore warning level before initialization
    (setq warning-minimum-level :warning)
    (configuration-layer//load-layer-files layers '("keybindings.el"))))

(defun configuration-layer//load-layer-files (layers files)
  "Load the files of list FILES for all LAYERS."
  (dolist (layer layers)
    (let* ((sym (car layer))
           (dir (plist-get (cdr layer) :dir)))
      (dolist (file files)
        (let ((file (concat dir file)))
          (if (file-exists-p file) (configuration-layer/load-file file)))))))

(defsubst configuration-layer//add-layer-to-hash (pkg layer hash)
  "Add LAYER to the list value stored in HASH with key PKG."
  (let ((list (ht-get hash pkg)))
    (eval `(push ',layer list))
    (puthash pkg list hash)))

(defsubst configuration-layer//filter-out-excluded (hash excluded)
  "Remove EXCLUDED packages from the hash tables HASH."
  (dolist (pkg (ht-keys (eval hash)))
    (when (or (member pkg excluded)) (ht-remove (eval hash) pkg))))

(defun configuration-layer//sort-packages-and-extensions ()
  "Sort the packages and extensions symbol and store them in
`configuration-layer-all-packages-sorted'
`configuration-layer-all-pre-extensions-sorted'
`configuration-layer-all-post-extensions-sorted'"
  (setq configuration-layer-all-packages-sorted
        (configuration-layer/sort-hash-table-keys configuration-layer-all-packages))
  (setq configuration-layer-all-pre-extensions-sorted
        (configuration-layer/sort-hash-table-keys configuration-layer-all-pre-extensions))
  (setq configuration-layer-all-post-extensions-sorted
        (configuration-layer/sort-hash-table-keys configuration-layer-all-post-extensions)))

(defun configuration-layer/sort-hash-table-keys (h)
  "Return a sorted list of the keys in the given hash table H."
  (mapcar 'intern (sort (mapcar 'symbol-name (ht-keys h)) 'string<)))

(defun configuration-layer/load-file (file)
  "Assure that FILE is loaded only once."
  (unless (member file configuration-layer--loaded-files)
    (load file)
    (push file configuration-layer--loaded-files)))

(defun configuration-layer/get-excluded-packages (layers)
  "Read `layer-excluded-packages' lists for all passed LAYERS and return a list
of all excluded packages."
  (let (result)
    (dolist (layer layers)
      (let* ((layer-sym (car layer))
             (dir (plist-get (cdr layer) :dir))
             (pkg-file (concat dir "packages.el")))
        (when (file-exists-p pkg-file)
          (configuration-layer/load-file pkg-file)
          (let ((excl-var (intern (format "%s-excluded-packages"
                                          (symbol-name layer-sym)))))
            (when (boundp excl-var)
              (mapc (lambda (x) (push x result)) (eval excl-var)))))))
    result))

(defun configuration-layer//get-packages-or-extensions (layers file var)
  "Read the packages or extensions lists for all passed LAYERS and
 return a hash table of all packages where the key is a package symbol.

FILE is a string with value `packages' or `extensions'.
VAR is a string with value `packages', `pre-extensions' or `post-extensions'."
  (let ((result (make-hash-table :size 512)))
    (dolist (layer layers)
      (let* ((layer-sym (car layer))
             (dir (plist-get (cdr layer) :dir))
             (pkg-file (concat dir (format "%s.el" file))))
        (when (file-exists-p pkg-file)
          (configuration-layer/load-file pkg-file)
          (let* ((layer-name (symbol-name layer-sym))
                 (packages-var (intern (format "%s-%s" layer-name var))))
            (when (boundp packages-var)
              (dolist (pkg (eval packages-var))
                (puthash pkg (cons layer-sym (ht-get result pkg)) result)))))))
    result))

(defun configuration-layer/get-packages (layers)
  "Read `layer-packages' lists for all passed LAYERS and return a hash table
of all packages where the key is a package symbol."
  (configuration-layer//get-packages-or-extensions layers "packages" "packages"))

(defun configuration-layer/get-extensions (layers &optional pre)
  "Read `layer-pre-extensions' or `layer-post-extensions' lists for all passed
LAYERS and return a hash table of all packages where the key is a package
symbol.
If PRE is non nil then `layer-pre-extensions' is read instead of
 `layer-post-extensions'."
  (let ((var (if pre "pre-extensions" "post-extensions")))
    (configuration-layer//get-packages-or-extensions layers "extensions" var)))

(defun configuration-layer//install-packages ()
  "Install the packages all the packages if there are not currently installed."
  (interactive)
  (let* ((not-installed (remove-if 'package-installed-p
                                   configuration-layer-all-packages-sorted))
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
            (spacemacs/replace-last-line-of-buffer
             (format "--> installing %s:%s... [%s/%s]"
                     (ht-get configuration-layer-all-packages pkg)
                     pkg
                     installed-count
                     not-installed-count) t)
            (unless (package-installed-p pkg)
              (if (not (assq pkg package-archive-contents))
                  (spacemacs/append-to-buffer
                   (format "\nPackage %s is unavailable. Is the package name misspelled?\n"
                    pkg))
                (dolist (dep (configuration-layer//get-package-dependencies-from-archive
                              pkg))
                  (configuration-layer//activate-package (car dep)))
                (package-install pkg)))
            (redisplay))
          (spacemacs/append-to-buffer "\n")))))

(defun configuration-layer/update-packages ()
  "Upgrade elpa packages"
  (interactive)
  (spacemacs/append-to-buffer
   "\nUpdating Spacemacs... (for now only ELPA packages are updated)\n")
  (spacemacs/append-to-buffer
   "--> fetching new package repository indexes...\n")
  (redisplay)
  (package-refresh-contents)
  (setq upgraded-count 0)
  (dolist (pkg configuration-layer-all-packages-sorted)
    ;; do not stop with errors on builtins and compilation fails
    (ignore-errors
      (let ((installed-version (configuration-layer//get-package-version pkg))
            (newest-version (configuration-layer//get-latest-package-version pkg)))
        ;; (message "package - %s" pkg)
        ;; (message "installed - %s" installed-version)
        ;; (message "latest - %s" newest-version)
        (unless (version<= newest-version installed-version)
          (progn 
            (setq upgraded-count (1+ upgraded-count))
            (spacemacs/replace-last-line-of-buffer
             (format "--> updating packge %s:%s (%s)..."
                     (ht-get configuration-layer-all-packages pkg)
                     pkg
                     upgraded-count
                     ))
            (redisplay)
            (configuration-layer//package-delete pkg)
            (package-install pkg)
            )))))
  (spacemacs/append-to-buffer
   (format (concat (if (> upgraded-count 0) "\n" "")
                   "--> %s packages updated.\n")
           upgraded-count))
  (redisplay))

(defun configuration-layer//initialize-packages ()
  "Initialize all the declared packages."
  (mapc (lambda (x) (configuration-layer//initialize-package
                     x (ht-get configuration-layer-all-packages x)))
        configuration-layer-all-packages-sorted))

(defun configuration-layer//initialize-package (pkg layers)
  "Initialize the package PKG from the configuration layers LAYERS."
  (dolist (layer layers)
    (let* ((init-func (intern (format "%s/init-%s" layer pkg))))
      (spacemacs/loading-animation)
      (if (and (package-installed-p pkg) (fboundp init-func))
          (progn
            (spacemacs/message "Package: Initializing %s:%s..." layer pkg)
            (configuration-layer//activate-package pkg)
            (funcall init-func))))))

(defun configuration-layer//activate-package (pkg)
  "Activate PKG."
  (if (version< emacs-version "24.4")
      ;; fake version list to always activate the package
      (package-activate pkg '(0 0 0 0))
    (package-activate pkg)))

(defun configuration-layer//initialize-pre-extension (ext layers)
  "Initialize the pre-extensions EXT from configuration layers LAYERS."
  (configuration-layer//initialize-extension ext layers t))

(defun configuration-layer//initialize-extensions (ext-list &optional pre)
  "Initialize all the declared extensions in EXT-LIST hash table.
If PRE is non nil then the extensions are pre-extensions."
  (let ((func (if pre 'configuration-layer//initialize-pre-extension
                'configuration-layer//initialize-extension))
        (hash (if pre configuration-layer-all-pre-extensions
                configuration-layer-all-post-extensions)))
    (mapc (lambda (x) (funcall func x (ht-get hash x))) ext-list)))

(defun configuration-layer//initialize-extension (ext layers &optional pre)
  "Initialize the extension EXT from the configuration layers LAYERS.
If PRE is non nil then the extension is a pre-extensions."
  (dolist (layer layers)
    (let* ((l (assq layer configuration-layer-layers))
           (ext-dir (plist-get (cdr l) :ext-dir))
           (init-func (intern (format "%s/init-%s" layer ext))))
      (add-to-list 'load-path (format "%s%s/" ext-dir ext))
      (spacemacs/loading-animation)
      (spacemacs/message "%s-extension: Initializing %s:%s..."
                         (if pre "Pre" "Post") layer ext)
      (if (fboundp init-func) (funcall init-func)))))

(defun configuration-layer//initialized-packages-count ()
  "Return the number of initialized packages and extensions."
  (+ (ht-size configuration-layer-all-packages)
     (ht-size configuration-layer-all-pre-extensions)
     (ht-size configuration-layer-all-post-extensions)))

(defun configuration-layer/get-layer-property (symlayer prop)
  "Return the value of the PROPerty for the given SYMLAYER symbol."
  (let* ((layer (assq symlayer configuration-layer-layers)))
         (plist-get (cdr layer) prop)))

(defun configuration-layer//get-packages-dependencies ()
  "Returns a hash map where key is a dependency package symbol and value is
a list of all packages which depend on it."
  (let ((result (make-hash-table :size 200)))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (configuration-layer//get-package-dependencies pkg-sym)))
        (dolist (dep deps)
          (let* ((dep-sym (car dep))
                 (value (ht-get result dep-sym)))
            (puthash dep-sym
                     (if value (add-to-list 'value pkg-sym) (list pkg-sym))
                     result)))))
    result))

(defun configuration-layer//get-implicit-packages ()
  "Returns a list of all packages in `packages-alist' which are not found
in `configuration-layer-all-packages'"
  (let ((imp-pkgs))
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (if (not (ht-contains? configuration-layer-all-packages pkg-sym))
            (add-to-list 'imp-pkgs pkg-sym))))
    imp-pkgs))

(defun configuration-layer//get-orphan-packages (implicit-pkgs dependencies)
  "Return a list of all orphan packages which are basically meant to be
deleted safely."
  (let ((result '()))
    (dolist (imp-pkg implicit-pkgs)
      (if (configuration-layer//is-package-orphan imp-pkg dependencies)
          (add-to-list 'result imp-pkg)))
    result))

(defun configuration-layer//is-package-orphan (pkg dependencies)
  "Returns not nil if PKG is an orphan package."
  (if (ht-contains? configuration-layer-all-packages pkg)
      nil
    (if (ht-contains? dependencies pkg)
        (let ((parents (ht-get dependencies pkg)))
          (reduce (lambda (x y) (and x y))
                  (mapcar (lambda (p) (configuration-layer//is-package-orphan
                                       p dependencies))
                          parents)
                  :initial-value t))
      (not (ht-contains? configuration-layer-all-packages pkg)))))

(defun configuration-layer//get-package-dependencies (package)
  "Return the dependencies alist for PACKAGE."
  (let ((pkg (assq package package-alist)))
    (cond
     ((version< emacs-version "24.4") (aref (cdr pkg) 1))
     (t (package-desc-reqs (cadr pkg))))))

(defun configuration-layer//get-package-dependencies-from-archive (pkg)
  "Return the dependencies alist for a PKG from the archive data."
  (let* ((arch (assq pkg package-archive-contents))
         (reqs (when arch (if (version< emacs-version "24.4")
                              (aref (cdr arch) 1)
                            (package-desc-reqs (cadr arch))))))
    ;; recursively get the requirements of reqs
    (dolist (req reqs)
      (let* ((pkg2 (car req))
             (reqs2 (configuration-layer//get-package-dependencies-from-archive pkg2)))
        (when reqs2 (setq reqs (append reqs2 reqs)))))
    reqs))

(defun configuration-layer//get-package-version (package)
  "Return the version string for PACKAGE."
  (let ((pkg (or (assq package package-alist)
                 (assq package package--builtins))))
    (cond
     ((version< emacs-version "24.4") (package-version-join (aref (cdr pkg) 0)))
     (t (package-version-join (package-desc-version (cadr pkg)))))))

(defun configuration-layer//get-latest-package-version (package)
  "Return the version string for PACKAGE."
  (let ((pkg (assq package package-archive-contents)))
    (cond
     ((version< emacs-version "24.4") (package-version-join (aref (cdr pkg) 0)))
     (t (package-version-join (package-desc-version (cadr pkg)))))))

(defun configuration-layer//package-delete (package)
  "Delete the passed PACKAGE."
  (cond
   ((version< emacs-version "24.4")
    (package-delete (symbol-name package)
                    (configuration-layer//get-package-version package)))
   (t (package-delete (cadr (assq package package-alist))))))

(defun configuration-layer/delete-orphan-packages ()
  "Delete all the orphan packages."
  (interactive)
  (let* ((dependencies (configuration-layer//get-packages-dependencies))
         (implicit-packages (configuration-layer//get-implicit-packages))
         (orphans (configuration-layer//get-orphan-packages implicit-packages
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
            (configuration-layer//package-delete orphan)
            (redisplay))
          (spacemacs/append-to-buffer "\n"))
      (spacemacs/message "No orphan package to delete."))))

(defun configuration-layer/setup-after-init-hook ()
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
                         (configuration-layer//initialized-packages-count)
                         elapsed)))
              (spacemacs/check-for-new-version spacemacs-version-check-interval))))

(defun configuration-layer//mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(provide 'configuration-layer)
