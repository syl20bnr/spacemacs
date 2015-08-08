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
;;
;;; Commentary:
;;
;;; Code:

(require 'eieio)
(require 'package)
(require 'ht)
(require 'core-dotspacemacs)
(require 'core-funcs)
(require 'core-spacemacs-buffer)

(unless package--initialized
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
  ;; optimization, no need to activate all the packages so early
  (setq package-enable-at-startup nil)
  (package-initialize 'noactivate)
  ;; Emacs 24.3 and above ships with python.el but in some Emacs 24.3.1 packages
  ;; for Ubuntu, python.el seems to be missing.
  ;; This hack adds marmalade repository for this case only.
  (unless (or (package-installed-p 'python) (version< emacs-version "24.3"))
    (add-to-list 'package-archives
                 '("marmalade" . "https://marmalade-repo.org/packages/"))))

(defconst configuration-layer-template-directory
  (expand-file-name (concat spacemacs-core-directory "templates/"))
  "Configuration layer templates directory.")

(defconst configuration-layer-contrib-directory
  (expand-file-name (concat user-emacs-directory "contrib/"))
  "Spacemacs contribution layers base directory.")

(defconst configuration-layer-private-directory
  (expand-file-name (concat user-emacs-directory "private/"))
  "Spacemacs private layers base directory.")

(defconst configuration-layer-private-layer-directory
  (let ((dotspacemacs-layer-dir
         (when dotspacemacs-directory
           (expand-file-name
            (concat dotspacemacs-directory "layers/")))))
    (if (and dotspacemacs-directory
             (file-exists-p dotspacemacs-layer-dir))
        dotspacemacs-layer-dir
      configuration-layer-private-directory))
  "Spacemacs default directory for private layers.")

(defconst configuration-layer-rollback-directory
  (expand-file-name (concat spacemacs-cache-directory ".rollback/"))
  "Spacemacs rollback directory.")

(defconst configuration-layer-rollback-info "rollback-info"
  "Spacemacs rollback information file.")

(defclass cfgl-layer ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the layer.")
   (dir :initarg :dir
        :type string
        :documentation "Absolute path to the layer directory.")
   (ext-dir :initarg :ext-dir
            :type string
            :documentation "Absolute path to the exentensions directory.")
   (variables :initarg :variables
              :initform nil
              :type list
              :documentation "A list of variable-value pairs."))
  "A configuration layer.")

(defclass cfgl-package ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the package.")
   (owner :initarg :owner
          :initform nil
          :type symbol
          :documentation "The layer defining the init function.")
   (pre-layers :initarg :pre-layers
               :initform '()
               :type list
               :documentation "Layers with a pre-init function.")
   (post-layers :initarg :post-layers
               :initform '()
               :type list
               :documentation "Layers with a post-init function.")
   (location :initarg :location
             :initform elpa
             :type (satisfies (lambda (x) (member x '(local elpa recipe))))
             :documentation "Location of the package.")
   (step :initarg :step
         :initform nil
         :type (satisfies (lambda (x) (member x '(nil pre post))))
         :documentation "Initialization step.")
   (excluded :initarg :excluded
             :initform nil
             :type boolean
             :documentation "If non-nil this package is ignored.")))

(defvar configuration-layer-layers '()
  "A list of `cfgl-layer' objects.")

(defvar configuration-layer-packages '()
  "A list of `cfgl-package' objects.")

(defvar configuration-layer-error-count nil
  "Non nil indicates the number of errors occurred during the
installation of initialization.")

(defvar configuration-layer-paths (make-hash-table :size 256)
  "Hash table of layers locations. The key is a layer symbol and the value is
the path for this layer.")

(defvar configuration-layer-all-packages-sorted '()
  "Sorted list of all package symbols.")

(defvar configuration-layer-categories '()
  "List of strings corresponding to category names. A category is a
directory with a name starting with `!'.")

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
                               configuration-layer-private-layer-directory
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
                       (read-directory-name (concat "Other configuration "
                                                    "layer path: ") "~/" ))
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
      (message (concat "Cannot create configuration layer \"%s\", "
                       "this layer already exists.") name))
     (t
      (make-directory layer-dir t)
      (configuration-layer//copy-template "extensions" layer-dir)
      (configuration-layer//copy-template "packages" layer-dir)
      (message "Configuration layer \"%s\" successfully created." name)))))

(defun configuration-layer/make-layer (layer)
  "Return a `cfgl-layer' object based on LAYER."
  (let* ((name-sym (if (listp layer) (car layer) layer))
         (name-str (symbol-name name-sym))
         (base-dir (configuration-layer/get-layer-path name-sym))
         (variables (when (listp layer)
                      (spacemacs/mplist-get layer :variables))))
    (if base-dir
        (let* ((dir (format "%s%s/" base-dir name-str))
               (ext-dir (format "%sextensions/" dir)))
          (cfgl-layer name-str
                      :name name-sym
                      :dir dir
                      :ext-dir ext-dir
                      :variables variables))
      (spacemacs-buffer/warning "Cannot find layer %S !" name-sym)
      nil)))

(defun configuration-layer/make-package (pkg)
  "Return a `cfgl-package' object based on PKG."
  (let* ((name-sym (if (listp pkg) (car pkg) pkg))
         (name-str (symbol-name name-sym))
         (location (when (listp pkg) (plist-get (cdr pkg) :location)))
         (step (when (listp pkg) (plist-get (cdr pkg) :step)))
         (obj (cfgl-package name-str :name name-sym)))
    (when location (oset obj :location location))
    (when step (oset obj :step step))
    obj))

(defun configuration-layer/get-packages (layers &optional dotfile)
  "Read the package lists of LAYERS and dotfile and return a list of packages."
  (let (result)
    (dolist (layer layers)
      (let* ((name (oref layer :name))
             (dir (oref layer :dir))
             (packages-file (concat dir "packages.el"))
             (extensions-file (concat dir "extensions.el")))
        ;; packages
        (when (file-exists-p packages-file)
          ;; required for lazy-loading of unused layers
          ;; for instance for helm-spacemacs
          (unless (configuration-layer/layer-usedp name)
            (load packages-file))
          (dolist (pkg (symbol-value (intern (format "%S-packages" name))))
            (let* ((pkgname (if (listp pkg) (car pkg) pkg))
                   (init-func (intern (format "%S/init-%S"
                                              name pkgname)))
                   (pre-init-func (intern (format "%S/pre-init-%S"
                                                  name pkgname)))
                   (post-init-func (intern (format "%S/post-init-%S"
                                                   name pkgname)))
                   (obj (object-assoc pkg :name result)))
              (unless obj
                (setq obj (configuration-layer/make-package pkg))
                (push obj result))
              (when (fboundp init-func)
                ;; last owner wins over the previous one,
                ;; still warn about mutliple owners
                (when (oref obj :owner)
                  (spacemacs-buffer/warning
                   (format (concat "More than one init function found for "
                                   "package %S. Previous owner was %S, "
                                   "replacing it with layer %S.")
                           pkg (oref obj :owner) name)))
                (oset obj :owner name))
              (when (fboundp pre-init-func)
                (push name (oref obj :pre-layers)))
              (when (fboundp post-init-func)
                (push name (oref obj :post-layers)))))
          (let ((xvar (intern (format "%S-excluded-packages" name))))
            (when (boundp xvar)
              (dolist (xpkg (symbol-value xvar))
                (let ((obj (object-assoc xpkg :name result)))
                  (unless obj
                    (setq obj (configuration-layer/make-package xpkg))
                    (push obj result))
                  (oset obj :excluded t))))))
        ;; extensions (dummy duplication of the code above)
        ;; TODO remove extensions in 0.105.0
        (when (file-exists-p extensions-file)
          ;; required for lazy-loading of unused layers
          ;; for instance for helm-spacemacs
          (unless (configuration-layer/layer-usedp name)
            (load extensions-file))
          (dolist (step '(pre post))
            (let ((var (intern (format "%S-%S-extensions" name step))))
              (when (boundp var)
                (dolist (pkg (symbol-value var))
                  (let ((pkgname (if (listp pkg) (car pkg) pkg)))
                    (when (fboundp (intern (format "%S/init-%S"
                                                   name pkgname)))
                      (let ((obj (configuration-layer/make-package pkg))
                            (init-func (intern (format "%S/init-%S"
                                                       name pkgname)))
                            (pre-init-func (intern (format "%S/pre-init-%S"
                                                           name pkgname)))
                            (post-init-func (intern (format "%S/post-init-%S"
                                                            name pkgname)))
                            (obj (object-assoc pkg :name result)))
                        (unless obj
                          (setq obj (configuration-layer/make-package pkg))
                          (push obj result))
                        (when (fboundp init-func)
                          ;; last owner wins over the previous one,
                          ;; still warn about mutliple owners
                          (when (oref obj :owner)
                            (spacemacs-buffer/warning
                             (format (concat
                                      "More than one init function found for "
                                      "package %S. Previous owner was %S, "
                                      "replacing it with layer %S.")
                                     pkg (oref obj :owner) name)))
                          (oset obj :owner name))
                        (when (fboundp pre-init-func)
                          (push name (oref obj :pre-layers)))
                        (when (fboundp post-init-func)
                          (push name (oref obj :post-layers)))
                        (oset obj :location 'local)
                        (oset obj :step step)))))))))))
    ;; additional and excluded packages from dotfile
    (when dotfile
      (dolist (apkg dotspacemacs-additional-packages)
        (let ((obj (object-assoc apkg :name result)))
          (unless obj
            (setq obj (configuration-layer/make-package apkg))
            (push obj result))
          (oset obj :owner 'dotfile)))
      (dolist (xpkg dotspacemacs-excluded-packages)
        (let ((obj (object-assoc xpkg :name result)))
          (unless obj
            (setq obj (configuration-layer/make-package xpkg))
            (push obj result))
          (oset obj :excluded t))))
    result))

(defun configuration-layer//get-private-layer-dir (name)
  "Return an absolute path the the private configuration layer with name
NAME."
  (concat configuration-layer-private-layer-directory name "/"))

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
  ;; load private layers at the end on purpose we asume that the user layers
  ;; must have the final word on configuration choices. Let
  ;; `dotspacemacs-directory' override the private directory if it exists.
  (let ((search-paths (append (list configuration-layer-contrib-directory)
                              dotspacemacs-configuration-layer-path
                              (list configuration-layer-private-layer-directory)
                              (when dotspacemacs-directory
                                (list dotspacemacs-directory))))
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
                  (spacemacs-buffer/message
                   "-> Discovered configuration layer: %s" layer-name)
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
          (list (configuration-layer/make-layer 'spacemacs))))
  (setq configuration-layer-layers
        (append (configuration-layer//declare-layers
                 dotspacemacs-configuration-layers)
                configuration-layer-layers)))

(defun configuration-layer//declare-layers (layers)
  "Declare the passed configuration LAYERS.
LAYERS is a list of layer symbols."
  (reduce (lambda (acc elt) (if elt (push elt acc) acc))
          (mapcar 'configuration-layer/make-layer (reverse layers))
          :initial-value nil))

(defun configuration-layer//set-layers-variables (layers)
  "Set the configuration variables for the passed LAYERS."
  (dolist (layer layers)
    (let ((variables (oref layer :variables)))
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

(defun configuration-layer/layer-usedp (name)
  "Return non-nil if NAME is the name of a used layer."
  (not (null (object-assoc name :name configuration-layer-layers))))

(defun configuration-layer/package-usedp (name)
  "Return non-nil if NAME is the name of a used package."
  (not (null (object-assoc name :name configuration-layer-packages))))

(defun configuration-layer/load-layers ()
  "Load all declared layers."
  ;; FIFO loading instead of LIFO, this allow the user to put her layers at the
  ;; end of the list to override previous layers.
  (let ((layers (reverse configuration-layer-layers))
        (warning-minimum-level :error))
    (configuration-layer//set-layers-variables layers)
    ;; first load all the config files ...
    (configuration-layer//load-layers-files layers '("funcs.el"
                                                     "config.el"))
    ;; ... then the package files
    ;; TODO remove extensions in 0.105.0
    (configuration-layer//load-layers-files layers '("packages.el"
                                                     "extensions.el"))
    ;; fill the hash tables
    (setq configuration-layer-packages
          (configuration-layer/get-packages layers t))
    ;; number of chuncks for the loading screen
    (setq spacemacs-loading-dots-chunk-threshold
          (/ (length configuration-layer-packages)
             spacemacs-loading-dots-chunk-count))
    ;; sort packages before installing and configuring them
    (configuration-layer//sort-packages)
    (configuration-layer//install-packages)
    (configuration-layer//configure-packages)
    ;; finally load the remaining files of a layer
    (configuration-layer//load-layers-files layers '("keybindings.el"))))

(defun configuration-layer//load-layers-files (layers files)
  "Load the files of list FILES for all passed LAYERS."
  (dolist (layer layers)
    (configuration-layer//load-layer-files layer files)))

(defun configuration-layer//load-layer-files (layer files)
  "Load the files of list FILES for the given LAYER."
  (dolist (file files)
    (let ((file (concat (oref layer :dir) file)))
      (if (file-exists-p file) (load file)))))

(defsubst configuration-layer//add-layer-to-hash (pkg layer hash)
  "Add LAYER to the list value stored in HASH with key PKG."
  (let ((list (ht-get hash pkg)))
    (symbol-value `(push ',layer list))
    (puthash pkg list hash)))

(defun configuration-layer/sort-hash-table-keys (h)
  "Return a sorted list of the keys in the given hash table H."
  (mapcar 'intern (sort (mapcar 'symbol-name (ht-keys h)) 'string<)))

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
            (let ((layer (ht-get configuration-layer-packages pkg)))
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

(defun configuration-layer/update-packages (&optional always-update)
  "Upgrade elpa packages.  If called with a prefix argument ALWAYS-UPDATE, assume yes to update."
  (interactive "P")
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
        (if (and (not always-update)
                 (not (yes-or-no-p (format (concat "%s package(s) to update, "
                                                   "do you want to continue ? ")
                                           upgrade-count))))
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

(defun configuration-layer//configure-packages ()
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
  (+ (ht-size configuration-layer-packages)
     (ht-size configuration-layer-all-pre-extensions)
     (ht-size configuration-layer-all-post-extensions)))

(defun configuration-layer/get-layers-list ()
  "Return a list of all discovered layer symbols."
  (ht-keys configuration-layer-paths))

(defun configuration-layer/get-layer-property (layer slot)
  "Return the value of SLOT for the given LAYER."
  (slot-value (object-assoc layer :name configuration-layer-layers) slot))

(defun configuration-layer/get-layer-path (layer)
  "Return the path for LAYER symbol."
  (ht-get configuration-layer-paths layer))

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
in `configuration-layer-packages'"
  (let ((imp-pkgs))
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (if (not (ht-contains? configuration-layer-packages pkg-sym))
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
  (if (ht-contains? configuration-layer-packages pkg)
      nil
    (if (ht-contains? dependencies pkg)
        (let ((parents (ht-get dependencies pkg)))
          (reduce (lambda (x y) (and x y))
                  (mapcar (lambda (p) (configuration-layer//is-package-orphan
                                       p dependencies))
                          parents)
                  :initial-value t))
      (not (ht-contains? configuration-layer-packages pkg)))))

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

;;; core-configuration-layer.el ends here

