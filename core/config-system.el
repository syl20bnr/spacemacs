(require 'dotspacemacs)
(require 'ht)

(defconst config-system-contrib-directory
  (expand-file-name (concat user-emacs-directory "contrib/"))
  "Spacemacs contribution layers base directory.")

(defconst config-system-private-directory
  (expand-file-name (concat user-emacs-directory "private/"))
  "Spacemacs private layers base directory.")

(defvar config-system-layers '()
  "Alist of declared configuration layers.")

(defvar config-system-layer-paths #s(hash-table size 128 data ())
  "Hash table of layers locations. The key is a layer symbol and the value is
the path for this layer.")

(defvar config-system-all-packages #s(hash-table size 256 data ())
  "Hash table of all declared packages in all layers where the key is a package
symbol and the value is a list of layer symbols responsible for initializing
and configuring the package.")

(defvar config-system-all-packages-sorted '()
  "Sorted list of all package symbols.")

(defvar config-system-all-pre-extensions #s(hash-table size 128 data ())
  "Hash table of all declared pre-extensions in all layers where the key is a
extension symbol and the value is the layer symbols responsible for initializing
and configuring the package.")

(defvar config-system-all-pre-extensions-sorted '()
  "Sorted list of all pre extensions symbols.")

(defvar config-system-all-post-extensions #s(hash-table size 128 data ())
  "Hash table of all declared post-extensions in all layers where the key is a
extension symbol and the value is the layer symbols responsible for initializing
and configuring the package.")

(defvar config-system-all-post-extensions-sorted '()
  "Sorted list of all post extensions symbols.")

(defvar config-system-contrib-categories '("usr" "lang")
  "List of strings corresponding to category names. A category is a
sub-directory of the contribution directory.")

(defvar config-system-excluded-packages '()
  "List of all excluded packages declared at the layer level.")

(defvar config-system--loaded-files '()
  "List of loaded files.")

(defun config-system/package.el-initialize ()
  "Initialize package.el"
  (require 'package)
  (unless package--initialized
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

(defun config-system/create-layer (name)
  "Ask the user for a configuration layer name and create a layer with this
name in the private layers directory."
  (interactive "sConfiguration layer name: ")
  (let ((layer-dir (config-system//get-private-layer-dir name)))
    (cond
     ((string-equal "" name)
      (message "Cannot create a configuration layer without a name."))
     ((file-exists-p layer-dir)
      (message "Cannot create configuration layer \"%s\", this layer already exists."
               name))
     (t
      (make-directory layer-dir)
      (config-system//copy-template "extensions")
      (config-system//copy-template "packages")
      (message "Configuration layer \"%s\" successfully created." name))
  )))

(defun config-system//get-private-layer-dir (name)
  "Return an absolute path the the private configuration layer with name
NAME."
  (concat config-system-private-directory name "/"))

(defun config-system//copy-template (template)
  "Copy and replace special values of TEMPLATE to LAYER_DIR."
  (let ((src (concat spacemacs-template-directory
                     (format "%s.template" template)))
        (dest (concat (config-system//get-private-layer-dir name)
                      (format "%s.el" template))))
    
    (copy-file src dest)
    (find-file dest)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "NAME" nil t)
        (replace-match name t)))
    (save-buffer)))

(defun config-system//get-contrib-category-dirs ()
  "Return a list of all absolute paths to the contribution categories stored
in `config-system-contrib-categories'"
  (mapcar
   (lambda (d) (expand-file-name
                (concat config-system-contrib-directory (format "%s/" d))))
   config-system-contrib-categories))

(defun config-system//discover-layers ()
  "Return a hash table where the key is the layer symbol and the value is its
path."
  (let ((cat-dirs (config-system//get-contrib-category-dirs))
        (result #s(hash-table size 128 data ())))
    (ht-clear result)
    ;; add spacemacs layer
    (puthash 'spacemacs (expand-file-name user-emacs-directory) result)
    (mapc (lambda (dir)
            (let ((layers (config-system//discover-layers-in-dir dir)))
              (mapc (lambda (layer)
                      (puthash (car layer) (cdr layer) result))
                    layers)))
          (append (list config-system-contrib-directory)
                  cat-dirs
                  dotspacemacs-configuration-layer-path
                  ;; load private layers at the end on purpose
                  ;; we asume that the user layers must have the final word
                  ;; on configuration choices.
                  (list config-system-private-directory)))
    (ht-copy result)))

(defun config-system//discover-layers-in-dir (dir)
  "Return an alist where the key is a layer symbol and the value is the path
for that layer."
  (spacemacs/message "Looking for configuration layers in %s" dir)
  (ignore-errors
    (let ((files (directory-files dir nil nil 'nosort))
          (filter-out (append config-system-contrib-categories '("." "..")))
          result '())
      (dolist (f files)
        (when (and (file-directory-p (concat dir f))
                   (not (member f filter-out)))
          (spacemacs/message "-> Discovered configuration layer: %s" f)
          (push (cons (intern f) dir) result)))
      result)))

(defun config-system//declare-layer (sym)
  "Declare a layer with SYM name (symbol). Return a cons cell (symbol . plist)
where `symbol' is the name of the layer and `plist' is a property list with
the following keys:
- `:dir'     the absolute path to the base directory of the layer.
- `:ext-dir' the absolute path to the directory containing the extensions."
  (let* ((sym-name (symbol-name sym))
         (base-dir (config-system/get-layer-path sym))
         (dir (format "%s%s/" base-dir sym-name))
         (ext-dir (format "%sextensions/" dir)))
    (if (file-exists-p dir)
        (cons sym (list :dir dir :ext-dir ext-dir))
      (spacemacs/message "Warning: layer %s does not exist in dir %s!"
                         sym-name dir))))

(defun config-system/get-layers-list ()
  "Return a list of all discovered layer symbols."
  (ht-keys config-system-layer-paths))

(defun config-system/get-layer-path (layer)
  "Return the path for LAYER symbol."
  (ht-get config-system-layer-paths layer))

(defun config-system/load-layers ()
  "Load all declared layers."
  (let ((layers (reverse config-system-layers)))
    (config-system//load-layer-files layers '("funcs.el" "config.el"))
    ;; fill the hash tables
    (setq config-system-excluded-packages (config-system/get-excluded-packages layers))
    (setq config-system-all-packages (config-system/get-packages layers))
    (setq config-system-all-pre-extensions (config-system/get-extensions layers t))
    (setq config-system-all-post-extensions (config-system/get-extensions layers))
    ;; This is what you get when you have no test cases... hopefully I will code
    ;; them soon :-)
    ;; (message "excluded: %s" config-system-excluded-packages)
    ;; (message "packages: %s" config-system-all-packages)
    ;; (message "pre-extensions: %s" config-system-all-pre-extensions)
    ;; (message "post-extensions: %s" config-system-all-post-extensions)
    ;; filter them
    (let ((excluded (append dotspacemacs-excluded-packages
                            config-system-excluded-packages)))
      (config-system//filter-out-excluded config-system-all-packages excluded)
      (config-system//filter-out-excluded config-system-all-pre-extensions excluded)
      (config-system//filter-out-excluded config-system-all-post-extensions excluded))
    ;; number of chuncks for the loading screen
    (let ((total (+ (ht-size config-system-all-packages)
                    (ht-size config-system-all-pre-extensions)
                    (ht-size config-system-all-post-extensions))))
      (setq spacemacs-loading-dots-chunk-threshold
            (/ total spacemacs-loading-dots-chunk-count)))
    ;; filter them
    (config-system//sort-packages-and-extensions)
    ;; install and initialize packages and extensions
    (config-system//initialize-extensions config-system-all-pre-extensions-sorted t)
    (config-system//install-packages)
    (spacemacs/append-to-buffer spacemacs-loading-text)
    (config-system//initialize-packages)
    (config-system//initialize-extensions config-system-all-post-extensions-sorted)
    ;; restore warning level before initialization
    (setq warning-minimum-level :warning)
    (config-system//load-layer-files layers '("keybindings.el"))))

(defun config-system//load-layer-files (layers files)
  "Load the files of list FILES for all LAYERS."
  (dolist (layer layers)
    (let* ((sym (car layer))
           (dir (plist-get (cdr layer) :dir)))
      (dolist (file files)
        (let ((file (concat dir file)))
          (if (file-exists-p file)
              (load file)))))))

(defsubst config-system//add-layer-to-hash (pkg layer hash)
  "Add LAYER to the list value stored in HASH with key PKG."
  (let ((list (ht-get hash pkg)))
    (eval `(push ',layer list))
    (puthash pkg list hash)))

(defsubst config-system//filter-out-excluded (hash excluded)
  "Remove EXCLUDED packages from the hash tables HASH."
  (dolist (pkg (ht-keys (eval hash)))
    (when (or (member pkg excluded)) (ht-remove (eval hash) pkg))))

(defun config-system//sort-packages-and-extensions ()
  "Sort the packages and extensions symbol and store them in
`config-system-all-packages-sorted'
`config-system-all-pre-extensions-sorted'
`config-system-all-post-extensions-sorted'"
  (setq config-system-all-packages-sorted
        (config-system/sort-hash-table-keys config-system-all-packages))
  (setq config-system-all-pre-extensions-sorted
        (config-system/sort-hash-table-keys config-system-all-pre-extensions))
  (setq config-system-all-post-extensions-sorted
        (config-system/sort-hash-table-keys config-system-all-post-extensions)))

(defun config-system/sort-hash-table-keys (h)
  "Return a sorted list of the keys in the given hash table H."
  (mapcar 'intern (sort (mapcar 'symbol-name (ht-keys h)) 'string<)))

(defun config-system/load-file (file)
  "Assure that FILE is loaded only once."
  (unless (member file config-system--loaded-files)
    (load file)
    (push file config-system--loaded-files)))

(defun config-system/get-excluded-packages (layers)
  "Read `layer-excluded-packages' lists for all passed LAYERS and return a list
of all excluded packages."
  (let (result)
    (dolist (layer layers)
      (let* ((layer-sym (car layer))
             (dir (plist-get (cdr layer) :dir))
             (pkg-file (concat dir "packages.el")))
        (when (file-exists-p pkg-file)
          (config-system/load-file pkg-file)
          (let ((excl-var (intern (format "%s-excluded-packages"
                                          (symbol-name layer-sym)))))
            (when (boundp excl-var)
              (mapc (lambda (x) (push x result)) (eval excl-var)))))))
    result))

(defun config-system//get-packages-or-extensions (layers file var)
  "Read the packages or extensions lists for all passed LAYERS and
 return a hash table of all packages where the key is a package symbol.

FILE is a string with value `packages' or `extensions'.
VAR is a string with value `packages', `pre-extensions' or `post-extensions'."
  (let ((result #s(hash-table size 512 data ())))
    (ht-clear result)
    (dolist (layer layers)
      (let* ((layer-sym (car layer))
             (dir (plist-get (cdr layer) :dir))
             (pkg-file (concat dir (format "%s.el" file))))
        (when (file-exists-p pkg-file)
          (config-system/load-file pkg-file)
          (let* ((layer-name (symbol-name layer-sym))
                 (packages (eval (intern (format "%s-%s" layer-name var)))))
            (dolist (pkg packages)
              (puthash pkg (cons layer-sym (ht-get result pkg)) result))))))
    (ht-copy result)))

(defun config-system/get-packages (layers)
  "Read `layer-packages' lists for all passed LAYERS and return a hash table
of all packages where the key is a package symbol."
  (config-system//get-packages-or-extensions layers "packages" "packages"))

(defun config-system/get-extensions (layers &optional pre)
  "Read `layer-pre-extensions' or `layer-post-extensions' lists for all passed
LAYERS and return a hash table of all packages where the key is a package
symbol.
If PRE is non nil then `layer-pre-extensions' is read instead of
 `layer-post-extensions'."
  (let ((var (if pre "pre-extensions" "post-extensions")))
    (config-system//get-packages-or-extensions layers "extensions" var)))

(defun config-system//install-packages ()
  "Install the packages all the packages if there are not currently installed."
  (interactive)
  (let* ((not-installed (remove-if 'package-installed-p
                                   config-system-all-packages-sorted))
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
                     (ht-get config-system-all-packages pkg)
                     pkg
                     installed-count
                     not-installed-count) t)
            (cond
             ((package-installed-p pkg))
             ;; Check whether the package exists in the archives before attempting to install.
             ((assoc pkg package-archive-contents)
              (package-install pkg))
             (t
              (spacemacs/append-to-buffer
               (format "\nPackage %s is unavailable. Is the package name misspelled?\n" pkg))))

            (redisplay))
          (spacemacs/append-to-buffer "\n")))))

(defun config-system//initialize-packages ()
  "Initialize all the declared packages."
  (mapc (lambda (x) (config-system//initialize-package
                     x (ht-get config-system-all-packages x)))
        config-system-all-packages-sorted))

(defun config-system//initialize-package (pkg layers)
  "Initialize the package PKG from the configuration layers LAYERS."
  (dolist (layer layers)
    (let* ((init-func (intern (format "%s/init-%s" layer pkg))))
      (spacemacs/loading-animation)
      (if (and (package-installed-p pkg) (fboundp init-func))
          (progn  (spacemacs/message "Package: Initializing %s:%s..." layer pkg)
                  (funcall init-func))))))

(defun config-system//initialize-pre-extension (ext layers)
  "Initialize the pre-extensions EXT from configuration layers LAYERS."
  (config-system//initialize-extension ext layers t))

(defun config-system//initialize-extensions (ext-list &optional pre)
  "Initialize all the declared extensions in EXT-LIST hash table.
If PRE is non nil then the extensions are pre-extensions."
  (let ((func (if pre 'config-system//initialize-pre-extension
                'config-system//initialize-extension))
        (hash (if pre config-system-all-pre-extensions
                config-system-all-post-extensions)))
    (mapc (lambda (x) (funcall func x (ht-get hash x))) ext-list)))

(defun config-system//initialize-extension (ext layers &optional pre)
  "Initialize the extension EXT from the configuration layers LAYERS.
If PRE is non nil then the extension is a pre-extensions."
  (dolist (layer layers)
    (let* ((l (assq layer config-system-layers))
           (ext-dir (plist-get (cdr l) :ext-dir))
           (init-func (intern (format "%s/init-%s" layer ext))))
      (add-to-list 'load-path (format "%s%s/" ext-dir ext))
      (spacemacs/loading-animation)
      (spacemacs/message "%s-extension: Initializing %s:%s..."
                         (if pre "Pre" "Post") layer ext)
      (if (fboundp init-func) (funcall init-func)))))

(defun config-system//initialized-packages-count ()
  "Return the number of initialized packages and extensions."
  (+ (ht-size config-system-all-packages)
     (ht-size config-system-all-pre-extensions)
     (ht-size config-system-all-post-extensions)))

(defun config-system/declare-layers ()
  "Declare default layers and user layers from the dotfile by filling the
`config-system-layers' variable."
  (setq config-system-layer-paths (config-system//discover-layers))
  (push (config-system//declare-layer 'spacemacs) config-system-layers)
  (mapc (lambda (layer) (push layer config-system-layers))
        (config-system//declare-dotspacemacs-configuration-layers)))

(defun config-system//declare-dotspacemacs-configuration-layers ()
  "Declare the configuration layer in order of appearance in list
`dotspacemacs-configuration-layers' defined in ~/.spacemacs."
  ;; (message "layer paths: %s" config-system-layer-paths)
  (let (result '())
    (if (boundp 'dotspacemacs-configuration-layers)
        (dolist (layer dotspacemacs-configuration-layers)
          (push (config-system//declare-layer layer) result)))
    result))

(defun config-system/get-layer-property (symlayer prop)
  "Return the value of the PROPerty for the given SYMLAYER symbol."
  (let* ((layer (assq symlayer config-system-layers)))
         (plist-get (cdr layer) prop)))

(defun config-system//get-packages-dependencies ()
  "Returns a hash map where key is a dependency package symbol and value is
a list of all packages which depend on it."
  (let ((result #s(hash-table size 200 data ())))
    (ht-clear result)
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (config-system//get-package-dependencies pkg-sym)))
        (dolist (dep deps)
          (let* ((dep-sym (car dep))
                 (value (ht-get result dep-sym)))
            (puthash dep-sym
                     (if value (add-to-list 'value pkg-sym) (list pkg-sym))
                     result)))))
    result))

(defun config-system//get-implicit-packages ()
  "Returns a list of all packages in `packages-alist' which are not found
in `config-system-all-packages'"
  (let ((imp-pkgs))
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (if (not (ht-contains? config-system-all-packages pkg-sym))
            (add-to-list 'imp-pkgs pkg-sym))))
    imp-pkgs))

(defun config-system//get-orphan-packages (implicit-pkgs dependencies)
  "Return a list of all orphan packages which are basically meant to be
deleted safely."
  (let ((result '()))
    (dolist (imp-pkg implicit-pkgs)
      (if (config-system//is-package-orphan imp-pkg dependencies)
          (add-to-list 'result imp-pkg)))
    result))

(defun config-system//is-package-orphan (pkg dependencies)
  "Returns not nil if PKG is an orphan package."
  (if (ht-contains? config-system-all-packages pkg)
      nil
    (if (ht-contains? dependencies pkg)
        (let ((parents (ht-get dependencies pkg)))
          (reduce (lambda (x y) (and x y))
                  (mapcar (lambda (p) (config-system//is-package-orphan
                                       p dependencies))
                          parents)
                  :initial-value t))
      (not (ht-contains? config-system-all-packages pkg)))))

(defun config-system//get-package-dependencies (package)
  "Return the dependencies alist for PACKAGE."
  (let ((pkg (assq package package-alist)))
    (cond
     ((version< emacs-version "24.4") (aref (cdr pkg) 1))
     (t (package-desc-reqs (cadr pkg))))))

(defun config-system//get-package-version (package)
  "Return the version string for PACKAGE."
  (let ((pkg (assq package package-alist)))
    (cond
     ((version< emacs-version "24.4")
      (package-version-join (aref (cdr pkg) 0)))
     (t
      (package-version-join (package-desc-version (cadr pkg)))))))

(defun config-system//package-delete (package)
  "Delete the passed PACKAGE."
  (cond
   ((version< emacs-version "24.4")
    (package-delete (symbol-name package)
                    (config-system//get-package-version package)))
   (t
    (package-delete (cadr (assq package package-alist))))))

(defun config-system/delete-orphan-packages ()
  "Delete all the orphan packages."
  (interactive)
  (let* ((dependencies (config-system//get-packages-dependencies))
         (implicit-packages (config-system//get-implicit-packages))
         (orphans (config-system//get-orphan-packages implicit-packages
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
            (config-system//package-delete orphan)
            (redisplay))
          (spacemacs/append-to-buffer "\n"))
      (spacemacs/message "No orphan package to delete."))))

(defun config-system/setup-after-init-hook ()
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
                         (config-system//initialized-packages-count)
                         elapsed)))
              )))

(provide 'config-system)
