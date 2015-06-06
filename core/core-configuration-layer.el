;;; core-configuration-layer.el --- Spacemacs Core File
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
(require 'ht)
(require 'package)
(require 'core-dotspacemacs)
(require 'core-funcs)
(require 'core-spacemacs-buffer)

(unless package--initialized
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")))
  ;; optimization, no need to activate all the packages so early
  (setq package-enable-at-startup nil)
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

(defconst configuration-layer-rollback-directory
  (expand-file-name (concat spacemacs-cache-directory ".rollback/"))
  "Spacemacs rollback directory.")

(defconst configuration-layer-rollback-info "rollback-info"
  "Spacemacs rollback information file.")

(defvar configuration-layer-error-count nil
  "Non nil indicates the number of errors occurred during the
installation of initialization.")

(defvar configuration-layer-layers '()
  "Alist of declared configuration layers.")

(defvar configuration-layer-paths (make-hash-table :size 256)
  "Hash table of layers locations. The key is a layer symbol and the value is
the path for this layer.")

(defvar configuration-layer-all-packages (make-hash-table :size 512)
  "Hash table of all declared packages in all layers where the key is a package
symbol and the value is a list of layer symbols responsible for initializing
and configuring the package.")

(defvar configuration-layer-all-packages-sorted '()
  "Sorted list of all package symbols.")

(defvar configuration-layer-packages-init-funcs '(make-hash-table :size 512)
  "Hash table of packages initialization functions. The key is a package symbol
and the value is an odered list of initialization functions to execute.")

(defvar configuration-layer-all-pre-extensions (make-hash-table :size 256)
  "Hash table of all declared pre-extensions in all layers where the key is a
extension symbol and the value is the layer symbols responsible for initializing
and configuring the package.")

(defvar configuration-layer-all-pre-extensions-sorted '()
  "Sorted list of all pre extensions symbols.")

(defvar configuration-layer-pre-extensions-init-funcs '(make-hash-table :size 256)
  "Hash table of pre-extensions initialization functions. The key is a package
symbol and the value is an odered list of initialization functions to execute.")

(defvar configuration-layer-all-post-extensions (make-hash-table :size 256)
  "Hash table of all declared post-extensions in all layers where the key is a
extension symbol and the value is the layer symbols responsible for initializing
and configuring the package.")

(defvar configuration-layer-post-extensions-init-funcs '(make-hash-table :size 256)
  "Hash table of post-extensions initialization functions. The key is a package
symbol and the value is an odered list of initialization functions to execute.")

(defvar configuration-layer-all-post-extensions-sorted '()
  "Sorted list of all post extensions symbols.")

(defvar configuration-layer-categories '()
  "List of strings corresponding to category names. A category is a
directory with a name starting with `!'.")

(defvar configuration-layer-excluded-packages '())

(defun configuration-layer/sync ()
  "Synchronize declared layers in dotfile with spacemacs."
  (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
  (configuration-layer/init-layers)
  (configuration-layer/load-layers)
  (when dotspacemacs-delete-orphan-packages
    (configuration-layer/delete-orphan-packages)))

(defun configuration-layer/create-layer ()
  "Ask the user for a configuration layer name and the layer
directory to use. Create a layer with this name in the selected
layer directory."
  (interactive)
  (let* ((current-layer-paths (mapcar (lambda (dir) (expand-file-name dir))
                                      (cl-pushnew
                               configuration-layer-private-directory
                               dotspacemacs-configuration-layer-path)))
         (other-choice "Another directory...")
         (helm-lp-source
          `((name . "Configuration Layer Paths")
            (candidates . ,(append current-layer-paths
                                   (list other-choice)))
            (action . (lambda (c) c))))
         (layer-path-sel (helm :sources helm-lp-source
                               :prompt "Configuration layer path: "))
         (layer-path (cond
                      ((string-equal layer-path-sel other-choice)
                       (read-directory-name "Other configuration layer path: " "~/" ))
                      ((member layer-path-sel current-layer-paths)
                       layer-path-sel)
                      (t
                       (error "Please select an option from the list"))))
         (name (read-from-minibuffer "Configuration layer name: " ))
         (layer-dir (concat layer-path "/" name)))
    (cond
     ((string-equal "" name)
      (message "Cannot create a configuration layer without a name."))
     ((file-exists-p layer-dir)
      (message "Cannot create configuration layer \"%s\", this layer already exists."
               name))
     (t
      (make-directory layer-dir t)
      (configuration-layer//copy-template "extensions" layer-dir)
      (configuration-layer//copy-template "packages" layer-dir)
      (message "Configuration layer \"%s\" successfully created." name)))))

(defun configuration-layer//get-private-layer-dir (name)
  "Return an absolute path the the private configuration layer with name
NAME."
  (concat configuration-layer-private-directory name "/"))

(defun configuration-layer//copy-template (template &optional layer-dir)
  "Copy and replace special values of TEMPLATE to LAYER_DIR. If
LAYER_DIR is nil, the private directory is used."
  (let ((src (concat configuration-layer-template-directory
                     (format "%s.template" template)))
        (dest (if layer-dir
                  (concat layer-dir "/" (format "%s.el" template))
                (concat (configuration-layer//get-private-layer-dir name)
                        (format "%s.el" template)))))
    (copy-file src dest)
    (find-file dest)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (re-search-forward "%LAYERNAME%" nil t)
          (replace-match name t))))
    (save-buffer)))

(defun configuration-layer//directory-type (path)
  "Return the type of directory pointed by PATH.
Possible return values:
  layer    - the directory is a layer
  category - the directory is a category
  nil      - the directory is a regular directory."
  (when (file-directory-p path)
    (if (string-match
         "^!" (file-name-nondirectory
               (directory-file-name
                (concat configuration-layer-contrib-directory path))))
        'category
      (let ((files (directory-files path)))
        ;; most frequent files encoutered in a layer are tested first
        (when (or (member "packages.el" files)
                  (member "extensions.el" files)
                  (member "config.el" files)
                  (member "keybindings.el" files)
                  (member "funcs.el" files))
          'layer)))))

(defun configuration-layer//get-category-from-path (dirpath)
  "Return a category symbol from the given DIRPATH.
The directory name must start with `!'.
Returns nil if the directory is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
                    (directory-file-name
                     (concat configuration-layer-contrib-directory
                             dirpath)))))
      (when (string-match "^!" dirname)
        (intern (substring dirname 1))))))

(defun configuration-layer//discover-layers ()
  "Return a hash table where the key is the layer symbol and the value is its
path."
  ;; load private layers at the end on purpose
  ;; we asume that the user layers must have the final word
  ;; on configuration choices.
  (let ((search-paths (append (list configuration-layer-contrib-directory)
                              dotspacemacs-configuration-layer-path
                              (list configuration-layer-private-directory)))
        (discovered '())
        (result (make-hash-table :size 256)))
    ;; depth-first search of subdirectories
    (while search-paths
      (let ((current-path (car search-paths)))
        (setq search-paths (cdr search-paths))
        (dolist (sub (directory-files current-path t nil 'nosort))
          ;; ignore ".", ".." and non-directories
          (unless (or (string-equal ".." (substring sub -2))
                      (string-equal "." (substring sub -1))
                      (not (file-directory-p sub)))
            (let ((type (configuration-layer//directory-type sub)))
              (cond
               ((eq 'category type)
                (let ((category (configuration-layer//get-category-from-path
                                 sub)))
                  (spacemacs-buffer/message "-> Discovered category: %S"
                                            category)
                  (push category configuration-layer-categories)
                  (setq search-paths (cons sub search-paths))))
               ((eq 'layer type)
                (let ((layer-name (file-name-nondirectory sub))
                      (layer-dir (file-name-directory sub)))
                  (spacemacs-buffer/message "-> Discovered configuration layer: %s"
                                            layer-name)
                  (push (cons (intern layer-name) layer-dir) discovered)))
               (t
                ;; layer not found, add it to search path
                (setq search-paths (cons sub search-paths)))))))))
    ;; add the spacemacs layer
    (puthash 'spacemacs (expand-file-name user-emacs-directory) result)
    ;; add discovered layers to hash table
    (mapc (lambda (l)
            (if (ht-contains? result (car l))
                ;; the same layer may have been discovered twice,
                ;; in which case we don't need a warning
                (unless (string-equal (ht-get result (car l)) (cdr l))
                  (spacemacs-buffer/warning
                   (concat "Duplicated layer %s detected in directory \"%s\", "
                           "keeping only the layer in directory \"%s\"")
                   (car l) (cdr l) (ht-get result (car l))))
              (puthash (car l) (cdr l) result)))
          discovered)
    result))

(defun configuration-layer/init-layers ()
  "Declare default layers and user layers from the dotfile by filling the
`configuration-layer-layers' variable."
  (setq configuration-layer-paths (configuration-layer//discover-layers))
  (if (eq 'all dotspacemacs-configuration-layers)
      (setq dotspacemacs-configuration-layers
            ;; spacemacs is contained in configuration-layer-paths
            (ht-keys configuration-layer-paths))
    (setq configuration-layer-layers
          (list (configuration-layer//declare-layer 'spacemacs))))
  (setq configuration-layer-layers
        (append (configuration-layer//declare-layers
                 dotspacemacs-configuration-layers) configuration-layer-layers)))

(defun configuration-layer//declare-layers (layers)
  "Declare the passed configuration LAYERS.
LAYERS is a list of layer symbols."
  (reduce (lambda (acc elt) (push elt acc))
          (mapcar 'configuration-layer//declare-layer (reverse layers))
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
      (spacemacs-buffer/warning "Cannot find layer %S !" name-sym)
      nil)))

(defun configuration-layer//set-layers-variables (layers)
  "Set the configuration variables for the passed LAYERS."
  (dolist (layer layers)
    (let ((variables (spacemacs/mplist-get layer :variables)))
          (while variables
            (let ((var (pop variables)))
              (if (consp variables)
                  (condition-case err
                      (set-default var (eval (pop variables)))
                    ('error
                     (configuration-layer//set-error)
                     (spacemacs-buffer/append
                      (format (concat "An error occurred while setting layer "
                                      "variable %s "
                                      "(error: %s). Be sure to quote the value "
                                      "if needed.\n") var err))))
                (spacemacs-buffer/warning "Missing value for variable %s !"
                                          var)))))))


(defun configuration-layer/package-usedp (pkg)
  "Return non-nil if PKG symbol corresponds to a used package."
  (ht-contains? configuration-layer-all-packages pkg))

(defun configuration-layer/layer-usedp (layer)
  "Return non-nil if LAYER symbol corresponds to a used layer."
  (not (null (assq layer configuration-layer-layers))))

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
    ;; first load the config files then the package files
    (configuration-layer//load-layers-files layers '("funcs.el" "config.el"))
    (configuration-layer//load-layers-files layers '("packages.el" "extensions.el"))
    ;; fill the hash tables
    (setq configuration-layer-all-packages (configuration-layer/get-packages layers))
    (setq configuration-layer-excluded-packages (configuration-layer/get-excluded-packages layers))
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
    (setq configuration-layer-packages-init-funcs
          (configuration-layer//filter-init-funcs configuration-layer-all-packages))
    (setq configuration-layer-pre-extensions-init-funcs
          (configuration-layer//filter-init-funcs configuration-layer-all-pre-extensions t))
    (setq configuration-layer-post-extensions-init-funcs
          (configuration-layer//filter-init-funcs configuration-layer-all-post-extensions t))
    ;; (message "package init-funcs: %s" configuration-layer-packages-init-funcs)
    ;; Add additional packages not tied to a layer
    (dolist (add-package dotspacemacs-additional-packages)
      (puthash add-package nil configuration-layer-all-packages))
    ;; number of chuncks for the loading screen
    (let ((total (+ (ht-size configuration-layer-all-packages)
                    (ht-size configuration-layer-all-pre-extensions)
                    (ht-size configuration-layer-all-post-extensions))))
      (setq spacemacs-loading-dots-chunk-threshold
            (/ total spacemacs-loading-dots-chunk-count)))
    ;; sort packages before initializing them
    (configuration-layer//sort-packages-and-extensions)
    ;; install and initialize packages and extensions
    (configuration-layer//initialize-pre-extensions)
    (configuration-layer//install-packages)
    (configuration-layer//initialize-packages)
    (configuration-layer//initialize-post-extensions)
    ;; restore warning level before initialization
    (setq warning-minimum-level :warning)
    (configuration-layer//load-layers-files layers '("keybindings.el"))))

(defun configuration-layer//load-layers-files (layers files)
  "Load the files of list FILES for all passed LAYERS."
  (dolist (layer layers)
    (configuration-layer//load-layer-files layer files)))

(defun configuration-layer//load-layer-files (layer files)
  "Load the files of list FILES for the given LAYER."
  (let* ((sym (car layer))
         (dir (plist-get (cdr layer) :dir)))
    (dolist (file files)
      (let ((file (concat dir file)))
        (if (file-exists-p file) (load file))))))

(defsubst configuration-layer//add-layer-to-hash (pkg layer hash)
  "Add LAYER to the list value stored in HASH with key PKG."
  (let ((list (ht-get hash pkg)))
    (eval `(push ',layer list))
    (puthash pkg list hash)))

(defun configuration-layer//filter-out-excluded (hash excluded)
  "Remove EXCLUDED packages from the hash tables HASH."
  (dolist (pkg (ht-keys hash))
    (when (or (member pkg excluded)) (ht-remove hash pkg))))

(defun configuration-layer//filter-init-funcs (hash &optional extension-p)
  "Remove from HASH packages with no corresponding initialization function and
returns a hash table of package symbols mapping to a list of initialization
functions to execute."
  (let ((result (make-hash-table :size 512)))
    (dolist (pkg (ht-keys hash))
      (let (initlayer prefuncs initfuncs postfuncs)
        (dolist (layer (ht-get hash pkg))
          (let ((initf (intern (format "%s/init-%S" layer pkg)))
                (pref (intern (format "%s/pre-init-%S" layer pkg)))
                (postf (intern (format "%s/post-init-%S" layer pkg))))
            (when (fboundp initf)
              (setq initlayer layer)
              (push initf initfuncs))
            (when (fboundp pref) (push pref prefuncs))
            (when (fboundp postf) (push postf postfuncs))))
        (if initfuncs
            (progn
              (puthash pkg (append prefuncs initfuncs postfuncs) result)
              (when extension-p
                (push (format "%s%s/"
                              (configuration-layer/get-layer-property
                               initlayer :ext-dir) pkg)
                      load-path)))
          (spacemacs-buffer/message
           (format "%s %S is ignored since it has no init function."
                   (if extension-p "Extension" "Package") pkg))
          (ht-remove hash pkg))))
    result))

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

(defun configuration-layer/get-excluded-packages (layers)
  "Read `layer-excluded-packages' lists for all passed LAYERS and return a list
of all excluded packages."
  (let (result)
    (dolist (layer layers)
      (let* ((layer-sym (car layer))
             (excl-var (intern (format "%s-excluded-packages"
                                       (symbol-name layer-sym)))))
        (when (boundp excl-var)
          (mapc (lambda (x) (push x result)) (eval excl-var)))))
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
          (unless (configuration-layer/layer-usedp layer-sym)
            (load pkg-file))
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
  (let* ((not-installed (configuration-layer//get-packages-to-install
                         configuration-layer-all-packages-sorted))
         (not-installed-count (length not-installed)))
    ;; installation
    (if not-installed
        (progn
          (spacemacs-buffer/append
           (format "Found %s new package(s) to install...\n"
                   not-installed-count))
          (spacemacs-buffer/append
           "--> fetching new package repository indexes...\n")
          (spacemacs//redisplay)
          (package-refresh-contents)
          (setq installed-count 0)
          (dolist (pkg not-installed)
            (setq installed-count (1+ installed-count))
            (let ((layer (ht-get configuration-layer-all-packages pkg)))
              (spacemacs-buffer/replace-last-line
               (format "--> installing %s%s... [%s/%s]"
                       (if layer (format "%s:" layer) "")
                       pkg installed-count not-installed-count) t))
            (unless (package-installed-p pkg)
              (condition-case err
                  (if (not (assq pkg package-archive-contents))
                      (spacemacs-buffer/append
                       (format "\nPackage %s is unavailable. Is the package name misspelled?\n"
                               pkg))
                    (dolist (dep (configuration-layer//get-package-dependencies-from-archive
                                  pkg))
                      (configuration-layer//activate-package (car dep)))
                    (package-install pkg))
                ('error
                 (configuration-layer//set-error)
                 (spacemacs-buffer/append
                  (format (concat "An error occurred while installing %s "
                                  "(error: %s)\n") pkg err)))))
            (spacemacs//redisplay))
          (spacemacs-buffer/append "\n")))))

(defun configuration-layer//filter-packages-with-deps (packages filter)
  "Filter a PACKAGES list according to a FILTER predicate.

FILTER is a function applied to each element of PACKAGES, if FILTER returns
non nil then element is removed from the list otherwise element is kept in
the list.

This function also processed recursively the package dependencies."
(when packages
    (let (result)
      (dolist (pkg packages)
        ;; recursively check dependencies
        (let* ((deps
                (configuration-layer//get-package-dependencies-from-archive pkg))
               (install-deps
                (when deps (configuration-layer//filter-packages-with-deps
                            (mapcar 'car deps) filter))))
          (when install-deps
            (setq result (append install-deps result))))
        (unless (apply filter `(,pkg))
          (add-to-list 'result pkg t)))
      (delete-dups result))))

(defun configuration-layer//get-packages-to-install (packages)
  "Return a list of packages to install given a list of PACKAGES."
  (configuration-layer//filter-packages-with-deps
   packages
   (lambda (x)
     ;; the package is already installed
     (package-installed-p x))))

(defun configuration-layer//get-packages-to-update (packages)
  "Return a list of packages to update given a list of PACKAGES."
  (configuration-layer//filter-packages-with-deps
   packages
   (lambda (x)
     ;; the package is a built-in package
     ;; or a newest version is available
     (let ((installed-ver (configuration-layer//get-package-version-string x)))
       (or (null installed-ver)
           (version<= (configuration-layer//get-latest-package-version-string x)
                      installed-ver))))))

(defun configuration-layer/update-packages ()
  "Upgrade elpa packages"
  (interactive)
  (spacemacs-buffer/insert-page-break)
  (spacemacs-buffer/append
   "\nUpdating Spacemacs... (for now only ELPA packages are updated)\n")
  (spacemacs-buffer/append
   "--> fetching new package repository indexes...\n")
  (spacemacs//redisplay)
  (package-refresh-contents)
  (let* ((update-packages (configuration-layer//get-packages-to-update
                           configuration-layer-all-packages-sorted))
         (date (format-time-string "%y-%m-%d_%H.%M.%S"))
         (rollback-dir (expand-file-name
                        (concat configuration-layer-rollback-directory
                                (file-name-as-directory date))))
         (upgrade-count (length update-packages))
         (upgraded-count 0)
         (update-packages-alist))
    (if (> upgrade-count 0)
        (if (not (yes-or-no-p (format (concat "%s package(s) to update, "
                                              "do you want to continue ? ")
                                      upgrade-count)))
            (spacemacs-buffer/append
             "Packages update has been cancelled.\n")
          ;; backup the package directory and construct an alist
          ;; variable to be cached for easy update and rollback
          (spacemacs-buffer/append
           "--> performing backup of package(s) to update...\n" t)
          (spacemacs//redisplay)
          (dolist (pkg update-packages)
            (let* ((src-dir (configuration-layer//get-package-directory pkg))
                   (dest-dir (expand-file-name
                              (concat rollback-dir
                                      (file-name-as-directory
                                       (file-name-nondirectory src-dir))))))
              (copy-directory src-dir dest-dir 'keeptime 'create 'copy-content)
              (push (cons pkg (file-name-nondirectory src-dir))
                    update-packages-alist)))
          (spacemacs/dump-vars-to-file
           '(update-packages-alist)
           (expand-file-name (concat rollback-dir
                                     configuration-layer-rollback-info)))
          (dolist (pkg update-packages)
            (setq upgraded-count (1+ upgraded-count))
            (spacemacs-buffer/replace-last-line
             (format "--> preparing update of package %s... [%s/%s]"
                     pkg upgraded-count upgrade-count) t)
            (spacemacs//redisplay)
            (configuration-layer//package-delete pkg))
          (spacemacs-buffer/append
           (format "\n--> %s package(s) to be updated.\n" upgraded-count))
          (spacemacs-buffer/append
           "\nEmacs has to be restarted to actually install the new packages.\n")
          (spacemacs//redisplay))
      (spacemacs-buffer/append "--> All packages are up to date.\n")
      (spacemacs//redisplay))))

(defun configuration-layer//ido-candidate-rollback-slot ()
  "Return a list of candidates to select a rollback slot."
  (let ((rolldir configuration-layer-rollback-directory))
    (when (file-exists-p rolldir)
      (reverse
       (delq nil (mapcar
                  (lambda (x)
                    (when (not (or (string= "." x) (string= ".." x)))
                      (let ((p (length (directory-files (file-name-as-directory
                                                         (concat rolldir x))))))
                        ;; -3 for . .. and rollback-info
                        (format "%s (%s packages)" x (- p 3)))))
                  (directory-files rolldir)))))))

(defun configuration-layer/rollback (slot)
  "Rollback all the packages in the given SLOT.
If called interactively and SLOT is nil then an ido buffers appears
to select one."
  (interactive
   (list
    (if (boundp 'slot) slot
      (let ((candidates (configuration-layer//ido-candidate-rollback-slot)))
        (when candidates
          (ido-completing-read "Rollback slots (most recent are first): "
                               candidates))))))
  (spacemacs-buffer/insert-page-break)
  (if (not slot)
      (message "No rollback slot available.")
    (string-match "^\\(.+?\\)\s.*$" slot)
    (let* ((slot-dir (match-string 1 slot))
           (rollback-dir (file-name-as-directory
                          (concat configuration-layer-rollback-directory
                                  (file-name-as-directory slot-dir))))
           (info-file (expand-file-name
                       (concat rollback-dir
                               configuration-layer-rollback-info))))
      (spacemacs-buffer/append
       (format "\nRollbacking ELPA packages from slot %s...\n" slot-dir))
      (load-file info-file)
      (let ((rollback-count (length update-packages-alist))
            (rollbacked-count 0))
        (spacemacs-buffer/append
         (format "Found %s package(s) to rollback...\n" rollback-count))
        (spacemacs//redisplay)
        (dolist (apkg update-packages-alist)
          (let* ((pkg (car apkg))
                 (pkg-dir-name (cdr apkg))
                 (installed-ver
                  (configuration-layer//get-package-version-string pkg))
                 (elpa-dir (concat user-emacs-directory "elpa/"))
                 (src-dir (expand-file-name
                           (concat rollback-dir (file-name-as-directory
                                                 pkg-dir-name))))
                 (dest-dir (expand-file-name
                            (concat elpa-dir (file-name-as-directory
                                              pkg-dir-name)))))
            (setq rollbacked-count (1+ rollbacked-count))
            (if (string-equal (format "%S-%s" pkg installed-ver) pkg-dir-name)
                (spacemacs-buffer/replace-last-line
                 (format "--> package %s already rollbacked! [%s/%s]"
                         pkg rollbacked-count rollback-count) t)
              ;; rollback the package
              (spacemacs-buffer/replace-last-line
               (format "--> rollbacking package %s... [%s/%s]"
                       pkg rollbacked-count rollback-count) t)
              (configuration-layer//package-delete pkg)
              (copy-directory src-dir dest-dir 'keeptime 'create 'copy-content))
            (spacemacs//redisplay)))
        (spacemacs-buffer/append
         (format "\n--> %s packages rollbacked.\n" rollbacked-count))
        (spacemacs-buffer/append
         "\nEmacs has to be restarted for the changes to take effect.\n")))))

(defun configuration-layer//initialize-packages ()
  "Initialize all the declared packages."
  (mapc (lambda (x)
          (spacemacs-buffer/message (format "Package: Initializing %S..." x))
          (configuration-layer//eval-init-functions
           x (ht-get configuration-layer-packages-init-funcs x)))
        configuration-layer-all-packages-sorted))

(defun configuration-layer//initialize-pre-extensions ()
  "Initialize all the declared pre-extensions."
  (mapc (lambda (x)
          (spacemacs-buffer/message (format "Pre-extension: Initializing %S..." x))
          (configuration-layer//eval-init-functions
           x (ht-get configuration-layer-pre-extensions-init-funcs x) t))
        configuration-layer-all-pre-extensions-sorted))

(defun configuration-layer//initialize-post-extensions ()
  "Initialize all the declared post-extensions."
  (mapc (lambda (x)
          (spacemacs-buffer/message (format "Post-extension: Initializing %S..." x))
          (configuration-layer//eval-init-functions
           x (ht-get configuration-layer-post-extensions-init-funcs x) t))
        configuration-layer-all-post-extensions-sorted))

(defun configuration-layer//eval-init-functions (pkg funcs &optional extension-p)
  "Initialize the package PKG by evaluating the functions of FUNCS."
  (when (or extension-p (package-installed-p pkg))
    (configuration-layer//activate-package pkg)
    (dolist (f funcs)
      (spacemacs-buffer/message (format "-> %S..." f))
      (condition-case err
          (funcall f)
        ('error
         (configuration-layer//set-error)
         (spacemacs-buffer/append
          (format (concat "An error occurred while initializing %s "
                          "(error: %s)\n") pkg err)))))
    (spacemacs-buffer/loading-animation)))

(defun configuration-layer//activate-package (pkg)
  "Activate PKG."
  (if (version< emacs-version "24.3.50")
      ;; fake version list to always activate the package
      (package-activate pkg '(0 0 0 0))
    (package-activate pkg)))

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
  (let ((result (make-hash-table :size 512)))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (configuration-layer//get-package-dependencies-from-archive pkg-sym)))
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

(defun configuration-layer//get-package-directory (pkg)
  "Return the directory path for PKG."
  (let ((pkg-desc (assq pkg package-alist)))
    (cond
     ((version< emacs-version "24.3.50")
      (let* ((version (aref (cdr pkg-desc) 0))
             (elpa-dir (concat user-emacs-directory "elpa/"))
             (pkg-dir-name (format "%s-%s.%s"
                                   (symbol-name pkg)
                                   (car version)
                                   (cadr version))))
        (expand-file-name (concat elpa-dir pkg-dir-name))))
     (t (package-desc-dir (cadr pkg-desc))))))

(defun configuration-layer//get-package-dependencies (pkg)
  "Return the dependencies alist for PKG."
  (let ((pkg-desc (assq pkg package-alist)))
    (cond
     ((version< emacs-version "24.3.50") (aref (cdr pkg-desc) 1))
     (t (package-desc-reqs (cadr pkg-desc))))))

(defun configuration-layer//get-package-dependencies-from-archive (pkg)
  "Return the dependencies alist for a PKG from the archive data."
  (let* ((pkg-arch (assq pkg package-archive-contents))
         (reqs (when pkg-arch (if (version< emacs-version "24.3.50")
                              (aref (cdr pkg-arch) 1)
                            (package-desc-reqs (cadr pkg-arch))))))
    ;; recursively get the requirements of reqs
    (dolist (req reqs)
      (let* ((pkg2 (car req))
             (reqs2 (configuration-layer//get-package-dependencies-from-archive pkg2)))
        (when reqs2 (setq reqs (append reqs2 reqs)))))
    reqs))

(defun configuration-layer//get-package-version-string (pkg)
  "Return the version string for PKG."
  (let ((pkg-desc (assq pkg package-alist)))
    (when pkg-desc
      (cond
       ((version< emacs-version "24.3.50") (package-version-join
                                            (aref (cdr pkg-desc) 0)))
       (t (package-version-join (package-desc-version (cadr pkg-desc))))))))

(defun configuration-layer//get-package-version (pkg)
  "Return the version list for PKG."
  (let ((version-string (configuration-layer//get-package-version-string pkg)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun configuration-layer//get-latest-package-version-string (pkg)
  "Return the version string for PKG."
  (let ((pkg-arch (assq pkg package-archive-contents)))
    (when pkg-arch
      (cond
       ((version< emacs-version "24.3.50") (package-version-join
                                            (aref (cdr pkg-arch) 0)))
       (t (package-version-join (package-desc-version (cadr pkg-arch))))))))

(defun configuration-layer//get-latest-package-version (pkg)
  "Return the versio list for PKG."
  (let ((version-string
         (configuration-layer//get-latest-package-version-string pkg)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun configuration-layer//package-delete (pkg)
  "Delete the passed PKG."
  (cond
   ((version< emacs-version "24.3.50")
    (let ((v (configuration-layer//get-package-version-string pkg)))
      (when v (package-delete (symbol-name pkg) v))))
   (t (let ((p (cadr (assq pkg package-alist))))
        (when p (package-delete p))))))

(defun configuration-layer//filter-used-themes (orphans)
  "Filter out used theme packages from ORPHANS candidates.
Returns the filtered list."
  (delq nil (mapcar (lambda (x)
                      (and (not (memq x spacemacs-used-theme-packages))
                           x)) orphans)))

(defun configuration-layer/delete-orphan-packages ()
  "Delete all the orphan packages."
  (interactive)
  (let* ((dependencies (configuration-layer//get-packages-dependencies))
         (implicit-packages (configuration-layer//get-implicit-packages))
         (orphans (configuration-layer//filter-used-themes
                   (configuration-layer//get-orphan-packages implicit-packages
                                                             dependencies)))
         (orphans-count (length orphans)))
    ;; (message "dependencies: %s" dependencies)
    ;; (message "implicit: %s" implicit-packages)
    ;; (message "orphans: %s" orphans)
    (if orphans
        (progn
          (spacemacs-buffer/append
           (format "Found %s orphan package(s) to delete...\n"
                   orphans-count))
          (setq deleted-count 0)
          (dolist (orphan orphans)
            (setq deleted-count (1+ deleted-count))
            (spacemacs-buffer/replace-last-line
             (format "--> deleting %s... [%s/%s]"
                     orphan
                     deleted-count
                     orphans-count) t)
            (configuration-layer//package-delete orphan)
            (spacemacs//redisplay))
          (spacemacs-buffer/append "\n"))
      (spacemacs-buffer/message "No orphan package to delete."))))

(defun configuration-layer//set-error ()
  "Set the error flag and change the mode-line color to red."
  (if configuration-layer-error-count
      (setq configuration-layer-error-count
            (1+ configuration-layer-error-count))
    (face-remap-add-relative 'mode-line '((:background "red") mode-line))
    (setq configuration-layer-error-count 1)))

(provide 'core-configuration-layer)
