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

(require 'cl-lib)
(require 'eieio)
(require 'package)
(require 'warnings)
(require 'ht)
(require 'core-dotspacemacs)
(require 'core-funcs)
(require 'core-spacemacs-buffer)

(unless package--initialized
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
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

(defconst configuration-layer-directory
  (expand-file-name (concat user-emacs-directory "layers/"))
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
   (variables :initarg :variables
              :initform nil
              :type list
              :documentation "A list of variable-value pairs.")
   (disabled :initarg :disabled-for
             :initform nil
             :type list
             :documentation "A list of layer where this layer is disabled."))
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
             :type (satisfies (lambda (x)
                                (or (member x '(built-in local elpa))
                                    (and (listp x) (eq 'recipe (car x))))))
             :documentation "Location of the package.")
   (step :initarg :step
         :initform nil
         :type (satisfies (lambda (x) (member x '(nil pre))))
         :documentation "Initialization step.")
   (excluded :initarg :excluded
             :initform nil
             :type boolean
             :documentation
             "If non-nil this package is excluded from all layers.")))

(defvar configuration-layer--layers '()
  "A non-sorted list of `cfgl-layer' objects.")

(defvar configuration-layer--packages '()
  "An alphabetically sorted list of `cfgl-package' objects.")

(defvar configuration-layer--used-distant-packages '()
  "A list of all distant packages that are effectively used.")

(defvar configuration-layer--skipped-packages nil
  "A list of all packages that were skipped during last update attempt.")

(defvar configuration-layer-error-count nil
  "Non nil indicates the number of errors occurred during the
installation of initialization.")

(defvar configuration-layer-paths (make-hash-table :size 256)
  "Hash table of layers locations. The key is a layer symbol and the value is
the path for this layer.")

(defvar configuration-layer-categories '()
  "List of strings corresponding to category names. A category is a
directory with a name starting with `+'.")

(defun configuration-layer/sync ()
  "Synchronize declared layers in dotfile with spacemacs."
  (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
  (when (spacemacs-buffer//choose-banner)
    (spacemacs-buffer//inject-version t))
  ;; layers
  (setq configuration-layer--layers (configuration-layer//declare-layers))
  (configuration-layer//configure-layers configuration-layer--layers)
  ;; packages
  (setq configuration-layer--packages (configuration-layer//declare-packages
                                      configuration-layer--layers))
  (setq configuration-layer--used-distant-packages
        (configuration-layer//get-distant-used-packages
         configuration-layer--packages))
  (configuration-layer//load-packages configuration-layer--packages)
  (when dotspacemacs-delete-orphan-packages
    (configuration-layer/delete-orphan-packages configuration-layer--packages)))

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
      (configuration-layer//copy-template name "extensions" layer-dir)
      (configuration-layer//copy-template name "packages" layer-dir)
      (message "Configuration layer \"%s\" successfully created." name)))))

(defun configuration-layer/make-layer (layer)
  "Return a `cfgl-layer' object based on LAYER."
  (let* ((name-sym (if (listp layer) (car layer) layer))
         (name-str (symbol-name name-sym))
         (base-dir (configuration-layer/get-layer-path name-sym))
         (disabled (when (listp layer)
                     (spacemacs/mplist-get layer :disabled-for)))
         (variables (when (listp layer)
                      (spacemacs/mplist-get layer :variables))))
    (if base-dir
        (let* ((dir (format "%s%s/" base-dir name-str)))
          (cfgl-layer name-str
                      :name name-sym
                      :dir dir
                      :disabled-for disabled
                      :variables variables))
      (spacemacs-buffer/warning "Cannot find layer %S !" name-sym)
      nil)))

(defun configuration-layer//make-layers (symbols)
  "Make `cfgl-layer' objects from the passed layer SYMBOLS."
  (delq nil (mapcar 'configuration-layer/make-layer symbols)))

(defun configuration-layer/make-package (pkg &optional obj)
  "Return a `cfgl-package' object based on PKG.
If OBJ is non nil then copy PKG properties into OBJ, otherwise create
a new object.
Properties that can be copied are `:location', `:step' and `:excluded'."
  (let* ((name-sym (if (listp pkg) (car pkg) pkg))
         (name-str (symbol-name name-sym))
         (location (when (listp pkg) (plist-get (cdr pkg) :location)))
         (step (when (listp pkg) (plist-get (cdr pkg) :step)))
         (excluded (when (listp pkg) (plist-get (cdr pkg) :excluded)))
         (obj (if obj obj (cfgl-package name-str :name name-sym))))
    (when location (oset obj :location location))
    (when step (oset obj :step step))
    (oset obj :excluded excluded)
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
          (eval `(defvar ,(intern (format "%S-packages" name)) nil))
          (unless (configuration-layer/layer-usedp name)
            (load packages-file))
          (dolist (pkg (symbol-value (intern (format "%S-packages" name))))
            (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
                   (init-func (intern (format "%S/init-%S"
                                              name pkg-name)))
                   (pre-init-func (intern (format "%S/pre-init-%S"
                                                  name pkg-name)))
                   (post-init-func (intern (format "%S/post-init-%S"
                                                   name pkg-name)))
                   (obj (object-assoc pkg-name :name result)))
              (if obj
                  (setq obj (configuration-layer/make-package pkg obj))
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
          ;; TODO remove support for <layer>-excluded-packages in 0.105.0
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
            (eval `(defvar ,(intern (format "%S-%S-extensions" name step)) nil))
            (let ((var (intern (format "%S-%S-extensions" name step))))
              (when (boundp var)
                (dolist (pkg (symbol-value var))
                  (let ((pkg-name (if (listp pkg) (car pkg) pkg)))
                    (when (fboundp (intern (format "%S/init-%S"
                                                   name pkg-name)))
                      (let ((obj (configuration-layer/make-package pkg))
                            (init-func (intern (format "%S/init-%S"
                                                       name pkg-name)))
                            (pre-init-func (intern (format "%S/pre-init-%S"
                                                           name pkg-name)))
                            (post-init-func (intern (format "%S/post-init-%S"
                                                            name pkg-name)))
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
                        (oset obj :step (when (eq 'pre step) step))))))))))))
    ;; additional and excluded packages from dotfile
    (when dotfile
      (dolist (pkg dotspacemacs-additional-packages)
        (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
               (obj (object-assoc pkg-name :name result)))
          (if obj
              (setq obj (configuration-layer/make-package pkg obj))
            (setq obj (configuration-layer/make-package pkg))
            (push obj result)
            (oset obj :owner 'dotfile))))
      (dolist (xpkg dotspacemacs-excluded-packages)
        (let ((obj (object-assoc xpkg :name result)))
          (unless obj
            (setq obj (configuration-layer/make-package xpkg))
            (push obj result))
          (oset obj :excluded t))))
    result))

(defun configuration-layer//sort-packages (packages)
  "Return a sorted list of PACKAGES objects."
  (sort packages (lambda (x y) (string< (symbol-name (oref x :name))
                                        (symbol-name (oref y :name))))))

(defun configuration-layer/filter-objects (objects ffunc)
  "Return a filtered OBJECTS list where each element satisfies FFUNC."
  (reverse (reduce (lambda (acc x)
                     (if (funcall ffunc x) (push x acc) acc))
                   objects
                   :initial-value nil)))

(defun configuration-layer//get-distant-used-packages (packages)
  "Return the distant packages (ie to be intalled) that are effectively used."
  (configuration-layer/filter-objects
   packages (lambda (x) (and (not (null (oref x :owner)))
                             (not (memq (oref x :location) '(built-in local)))
                             (not (oref x :excluded))))))

(defun configuration-layer//get-private-layer-dir (name)
  "Return an absolute path to the private configuration layer string NAME."
  (file-name-as-directory
   (concat configuration-layer-private-layer-directory name)))

(defun configuration-layer//copy-template (name template &optional layer-dir)
  "Copy and replace special values of TEMPLATE to layer string NAME.
If LAYER_DIR is nil, the private directory is used."
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
         "^+" (file-name-nondirectory
               (directory-file-name
                (concat configuration-layer-directory path))))
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
The directory name must start with `+'.
Returns nil if the directory is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
                    (directory-file-name
                     (concat configuration-layer-directory
                             dirpath)))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defun configuration-layer//discover-layers ()
  "Return a hash table where the key is the layer symbol and the value is its
path."
  ;; load private layers at the end on purpose we asume that the user layers
  ;; must have the final word on configuration choices. Let
  ;; `dotspacemacs-directory' override the private directory if it exists.
  (let ((search-paths (append (list configuration-layer-directory)
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

(defun configuration-layer//declare-layers ()
  "Declare default layers and user layers declared in the dotfile."
  (setq configuration-layer--layers nil)
  (setq configuration-layer-paths (configuration-layer//discover-layers))
  (when (eq 'all dotspacemacs-configuration-layers)
    (setq dotspacemacs-configuration-layers
          (ht-keys configuration-layer-paths)))
  (dolist (layer dotspacemacs-configuration-layers)
    (let ((layer-name (if (listp layer) (car layer) layer)))
      (if (ht-contains? configuration-layer-paths layer-name)
          (unless (string-match-p "+distribution"
                                  (ht-get configuration-layer-paths layer-name))
            (push (configuration-layer/make-layer layer)
                  configuration-layer--layers))
        (spacemacs-buffer/warning "Unknown layer %s declared in dotfile."
                                  layer-name))))
  (setq configuration-layer--layers (reverse configuration-layer--layers))
  ;; distribution layer is always first
  (push (configuration-layer/make-layer dotspacemacs-distribution)
        configuration-layer--layers))

(defun configuration-layer/declare-layers (layer-names)
  "Add layer with LAYER-NAMES to used layers."
  (mapc 'configuration-layer/declare-layer layer-names))

(defun configuration-layer/declare-layer (layer-name)
  "Declare a single layer"
  (unless (object-assoc layer-name :name configuration-layer--layers)
    (let ((new-layer (configuration-layer/make-layer layer-name)))
      (push new-layer configuration-layer--layers)
      (configuration-layer//configure-layer new-layer))))

(defun configuration-layer//set-layers-variables (layers)
  "Set the configuration variables for the passed LAYERS."
  (mapc 'configuration-layer//set-layer-variables layers))

(defun configuration-layer//set-layer-variables (layer)
  "Set the configuration variables for the passed LAYER."
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
                                    var))))))

(defun configuration-layer/layer-usedp (name)
  "Return non-nil if NAME is the name of a used layer."
  (not (null (object-assoc name :name configuration-layer--layers))))

(defun configuration-layer/package-usedp (name)
  "Return non-nil if NAME is the name of a used package."
  (let ((obj (object-assoc name :name configuration-layer--packages)))
    (when obj (oref obj :owner))))

(defun configuration-layer//configure-layers (layers)
  "Configure LAYERS."
  ;; FIFO loading of layers, this allow the user to put her layers at the
  ;; end of the list to override previous layers.
  (let ((warning-minimum-level :error))
    (dolist (l layers)
      (configuration-layer//configure-layer l))))

(defun configuration-layer//configure-layer (layer)
  "Configure LAYER."
  (configuration-layer//set-layer-variables layer)
  (configuration-layer//load-layer-files layer '("funcs.el"
                                                 "config.el"
                                                 "keybindings.el")))

(defun configuration-layer//declare-packages (layers)
  "Declare all packages contained in LAYERS."
  (let ((layers2 layers)
        (warning-minimum-level :error))
    ;; TODO remove extensions in 0.105.0
    (configuration-layer//load-layers-files layers2 '("packages.el" "extensions.el"))
    ;; gather all the packages of current layer
    (configuration-layer//sort-packages (configuration-layer/get-packages
                                         layers2 t))))

(defun configuration-layer//load-packages (packages)
  "Load PACKAGES."
  ;; number of chuncks for the loading screen
  (setq spacemacs-loading-dots-chunk-threshold
        (/ (configuration-layer/configured-packages-count)
           spacemacs-loading-dots-chunk-count))
  (configuration-layer//install-packages packages)
  (configuration-layer//configure-packages packages))

(defun configuration-layer//load-layers-files (layers files)
  "Load the files of list FILES for all passed LAYERS."
  (dolist (layer layers)
    (configuration-layer//load-layer-files layer files)))

(defun configuration-layer//load-layer-files (layer files)
  "Load the files of list FILES for the given LAYER."
  (dolist (file files)
    (let ((file (concat (oref layer :dir) file)))
      (if (file-exists-p file) (load file)))))

(defun configuration-layer/configured-packages-count ()
  "Return the number of configured packages."
  (length configuration-layer--packages))

(defun configuration-layer//install-packages (packages)
  "Install PACKAGES."
  (interactive)
  (let* ((noinst-pkg-names
          (configuration-layer//get-uninstalled-packages
           (mapcar 'car
                   (object-assoc-list
                    :name configuration-layer--used-distant-packages))))
         (noinst-count (length noinst-pkg-names)))
    ;; installation
    (when noinst-pkg-names
      (spacemacs-buffer/append
       (format "Found %s new package(s) to install...\n"
               noinst-count))
      (spacemacs-buffer/append
       "--> fetching new package repository indexes...\n")
      (spacemacs//redisplay)
      (package-refresh-contents)
      (setq installed-count 0)
      (dolist (pkg-name noinst-pkg-names)
        (setq installed-count (1+ installed-count))
        (let* ((pkg (object-assoc pkg-name :name configuration-layer--packages))
               (layer (when pkg (oref pkg :owner)))
               (location (when pkg (oref pkg :location))))
          (spacemacs-buffer/replace-last-line
           (format "--> installing %s%s... [%s/%s]"
                   (if layer (format "%S:" layer) "dependency ")
                   pkg-name installed-count noinst-count) t)
          (unless (package-installed-p pkg-name)
            (condition-case err
                (cond
                 ((or (null pkg) (eq 'elpa location))
                  (configuration-layer//install-from-elpa pkg-name))
                 ((and (listp location) (eq 'recipe (car location)))
                  (configuration-layer//install-from-recipe pkg))
                 (t (spacemacs-buffer/warning "Cannot install package %S."
                                              pkg-name)))
              ('error
               (configuration-layer//set-error)
               (spacemacs-buffer/append
                (format (concat "An error occurred while installing %s "
                                "(error: %s)\n") pkg-name err))))))
        (spacemacs//redisplay))
      (spacemacs-buffer/append "\n"))))

(defun configuration-layer//install-from-elpa (pkg-name)
  "Install PKG from ELPA."
  (if (not (assq pkg-name package-archive-contents))
      (spacemacs-buffer/append
       (format (concat "\nPackage %s is unavailable. "
                       "Is the package name misspelled?\n")
               pkg-name))
    (dolist
        (dep (configuration-layer//get-package-deps-from-archive
              pkg-name))
      (configuration-layer//activate-package (car dep)))
    (package-install pkg-name)))

(defun configuration-layer//install-from-recipe (pkg)
  "Install PKG from a recipe."
  (let* ((pkg-name (oref pkg :name))
         (layer (oref pkg :owner))
         (recipe (cons pkg-name (cdr (oref pkg :location)))))
    (if recipe
        (quelpa recipe)
      (spacemacs-buffer/warning
       (concat "Cannot find any recipe for package %S! Be sure "
               "to add a recipe for it in alist %S.")
       pkg-name recipes-var))))

(defun configuration-layer//filter-packages-with-deps
    (pkg-names filter &optional use-archive)
  "Return a filtered PACKAGES list where each elements satisfies FILTER."
  (when pkg-names
    (let (result)
      (dolist (pkg-name pkg-names)
        ;; recursively check dependencies
        (let* ((deps
                (if use-archive
                    (configuration-layer//get-package-deps-from-archive
                     pkg-name)
                  (configuration-layer//get-package-deps-from-alist pkg-name)))
               (install-deps
                (when deps (configuration-layer//filter-packages-with-deps
                            (mapcar 'car deps) filter))))
          (when install-deps
            (setq result (append install-deps result))))
        (when (funcall filter pkg-name)
          (add-to-list 'result pkg-name t)))
      (delete-dups result))))

(defun configuration-layer//get-uninstalled-packages (pkg-names)
  "Return a filtered list of PKG-NAMES to install."
  (configuration-layer//filter-packages-with-deps
   pkg-names (lambda (x) (not (package-installed-p x)))))

(defun configuration-layer//package-has-recipe-p (pkg-name)
  "Return non nil if PKG-NAME is the name of a package declared with a recipe."
  (when (object-assoc pkg-name :name configuration-layer--packages)
    (let* ((pkg (object-assoc pkg-name :name configuration-layer--packages))
           (location (oref pkg :location)))
      (and (listp location) (eq 'recipe (car location))))))

(defun configuration-layer//get-package-recipe (pkg-name)
  "Return the recipe for PGK-NAME if it has one."
  (let ((pkg (object-assoc pkg-name :name configuration-layer--packages)))
    (when pkg
      (let ((location (oref pkg :location)))
        (when (and (listp location) (eq 'recipe (car location)))
          location)))))

(defun configuration-layer//new-version-available-p (pkg-name)
  "Return non nil if there is a new version available for PKG-NAME."
  (let ((recipe (configuration-layer//get-package-recipe pkg-name))
        (cur-version (configuration-layer//get-package-version-string pkg-name))
        new-version)
    (when cur-version
      (setq new-version
            (if recipe
                (quelpa-checkout recipe (expand-file-name (symbol-name pkg-name)
                                                          quelpa-build-dir))
              (configuration-layer//get-latest-package-version-string
               pkg-name)))
      ;; (message "%s: %s > %s ?" pkg-name cur-version new-version)
      (if new-version
          (version< cur-version new-version)
        (cl-pushnew pkg-name configuration-layer--skipped-packages :test #'eq)
        nil))))

(defun configuration-layer//get-packages-to-update (pkg-names)
  "Return a filtered list of PKG-NAMES to update."
  (configuration-layer//filter-packages-with-deps
   pkg-names 'configuration-layer//new-version-available-p 'use-archive))

(defun configuration-layer//configure-packages (packages)
  "Configure all passed PACKAGES honoring the steps order."
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x) (eq 'pre (oref x :step)))))
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x) (null (oref x :step))))))

(defun configuration-layer//configure-packages-2 (packages)
  "Configure all passed PACKAGES."
  (dolist (pkg packages)
    (spacemacs-buffer/loading-animation)
    (let ((pkg-name (oref pkg :name)))
      (cond
       ((oref pkg :excluded)
        (spacemacs-buffer/message
         (format "%S ignored since it has been excluded." pkg-name)))
       ((null (oref pkg :owner))
        (spacemacs-buffer/message
         (format "%S ignored since it has no owner layer." pkg-name)))
       (t
        ;; load-path
        (when (eq 'local (oref pkg :location))
          (if (eq 'dotfile (oref pkg :owner))
              ;; local packages owned by dotfile are stored in private/local
              (push (file-name-as-directory
                     (concat configuration-layer-private-directory
                             "local/"
                             (symbol-name (oref pkg :name))))
                    load-path)
            (let* ((owner (object-assoc (oref pkg :owner)
                                        :name configuration-layer--layers))
                   (dir (when owner (oref owner :dir))))
              (push (format "%slocal/%S/" dir pkg-name) load-path)
              ;; TODO remove extensions in 0.105.0
              (push (format "%sextensions/%S/" dir pkg-name) load-path))))
        ;; configuration
        (cond
         ((eq 'dotfile (oref pkg :owner))
          (configuration-layer//activate-package pkg-name)
          (spacemacs-buffer/message
           (format "%S is configured in the dotfile." pkg-name)))
         (t
          (configuration-layer//activate-package pkg-name)
          (configuration-layer//configure-package pkg))))))))

(defun configuration-layer//configure-package (pkg)
  "Configure PKG."
  (let* ((pkg-name (oref pkg :name))
         (owner (oref pkg :owner))
         (owner-layer (object-assoc owner :name configuration-layer--layers))
         (disabled-for-layers (oref owner-layer :disabled-for)))
    (spacemacs-buffer/message (format "Configuring %S..." pkg-name))
    ;; pre-init
    (mapc (lambda (layer)
            (if (memq layer disabled-for-layers)
                (spacemacs-buffer/message
                 (format "  -> ignored pre-init (%S)..." layer))
              (spacemacs-buffer/message
               (format "  -> pre-init (%S)..." layer))
              (condition-case err
                  (funcall (intern (format "%S/pre-init-%S" layer pkg-name)))
                ('error
                 (configuration-layer//set-error)
                 (spacemacs-buffer/append
                  (format
                   (concat "An error occurred while pre-configuring %S "
                           "in layer %S (error: %s)\n")
                   pkg-name layer err))))))
          (oref pkg :pre-layers))
    ;; init
    (spacemacs-buffer/message (format "  -> init (%S)..." owner))
    (funcall (intern (format "%S/init-%S" owner pkg-name)))
    ;; post-init
    (mapc (lambda (layer)
            (if (memq layer disabled-for-layers)
                (spacemacs-buffer/message
                 (format "  -> ignored post-init (%S)..." layer))
              (spacemacs-buffer/message
               (format "  -> post-init (%S)..." layer))
              (condition-case err
                  (funcall (intern (format "%S/post-init-%S" layer pkg-name)))
                ('error
                 (configuration-layer//set-error)
                 (spacemacs-buffer/append
                  (format
                   (concat "An error occurred while post-configuring %S "
                           "in layer %S (error: %s)\n")
                   pkg-name layer err))))))
          (oref pkg :post-layers))))

(defun configuration-layer/update-packages (&optional always-update)
  "Update packages.

If called with a prefix argument ALWAYS-UPDATE, assume yes to update."
  (interactive "P")
  (spacemacs-buffer/insert-page-break)
  (spacemacs-buffer/append
   "\nUpdating Spacemacs... (for now only ELPA packages are updated)\n")
  (spacemacs-buffer/append
   "--> fetching new package repository indexes...\n")
  (spacemacs//redisplay)
  (package-refresh-contents)
  (setq configuration-layer--skipped-packages nil)
  (let* ((update-packages
          (configuration-layer//get-packages-to-update
           (mapcar 'car (object-assoc-list
                         :name configuration-layer--used-distant-packages))))
         (skipped-count (length configuration-layer--skipped-packages))
         (date (format-time-string "%y-%m-%d_%H.%M.%S"))
         (rollback-dir (expand-file-name
                        (concat configuration-layer-rollback-directory
                                (file-name-as-directory date))))
         (upgrade-count (length update-packages))
         (upgraded-count 0)
         (update-packages-alist))
    (when configuration-layer--skipped-packages
      (spacemacs-buffer/append
       (format (concat "--> Warning: cannot update %s package(s), possibly due"
                       " to a temporary network problem: %s\n")
               skipped-count
               (mapconcat #'symbol-name
                          configuration-layer--skipped-packages
                          " "))))
    ;; (message "packages to udpate: %s" update-packages)
    (if (> upgrade-count 0)
        (if (and (not always-update)
                 (not (yes-or-no-p (format (concat "%s package(s) to update, "
                                                   (if (> skipped-count 0)
                                                       (format "%s package(s) skipped, "
                                                               skipped-count)
                                                     "")
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

(defun configuration-layer//activate-package (pkg)
  "Activate PKG."
  (if (version< emacs-version "24.3.50")
      ;; fake version list to always activate the package
      (package-activate pkg '(0 0 0 0))
    (package-activate pkg)))

(defun configuration-layer/get-layers-list ()
  "Return a list of all discovered layer symbols."
  (ht-keys configuration-layer-paths))

(defun configuration-layer/get-layer-property (layer slot)
  "Return the value of SLOT for the given LAYER."
  (slot-value (object-assoc layer :name configuration-layer--layers) slot))

(defun configuration-layer/get-layer-local-dir (layer)
  "Return the value of SLOT for the given LAYER."
  (concat (slot-value (object-assoc layer :name configuration-layer--layers)
                      :dir) "local/"))

(defun configuration-layer/get-layer-path (layer)
  "Return the path for LAYER symbol."
  (ht-get configuration-layer-paths layer))

(defun configuration-layer//get-packages-dependencies ()
  "Returns dependencies hash map for all packages in `package-alist'."
  (let ((result (make-hash-table :size 512)))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (configuration-layer//get-package-deps-from-alist pkg-sym)))
        (dolist (dep deps)
          (let* ((dep-sym (car dep))
                 (value (ht-get result dep-sym)))
            (puthash dep-sym
                     (if value (add-to-list 'value pkg-sym) (list pkg-sym))
                     result)))))
    result))

(defun configuration-layer//get-implicit-packages (packages)
  "Returns packages in `packages-alist' which are not found in PACKAGES."
  (let (imp-pkgs)
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (unless (object-assoc pkg-sym :name packages)
          (add-to-list 'imp-pkgs pkg-sym))))
    imp-pkgs))

(defun configuration-layer//get-orphan-packages
    (dist-pkgs implicit-pkgs dependencies)
  "Return orphan packages."
  (let (result)
    (dolist (imp-pkg implicit-pkgs)
      (when (configuration-layer//is-package-orphan
             imp-pkg dist-pkgs dependencies)
        (add-to-list 'result imp-pkg)))
    result))

(defun configuration-layer//is-package-orphan (pkg-name dist-pkgs dependencies)
  "Returns not nil if PKG-NAME is the name of an orphan package."
  (unless (object-assoc pkg-name :name dist-pkgs)
    (if (ht-contains? dependencies pkg-name)
        (let ((parents (ht-get dependencies pkg-name)))
          (reduce (lambda (x y) (and x y))
                  (mapcar (lambda (p) (configuration-layer//is-package-orphan
                                       p dist-pkgs dependencies))
                          parents)
                  :initial-value t))
      (not (object-assoc pkg-name :name dist-pkgs)))))

(defun configuration-layer//get-package-directory (pkg-name)
  "Return the directory path for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (cond
     ((version< emacs-version "24.3.50")
      (let* ((version (aref (cdr pkg-desc) 0))
             (elpa-dir (concat user-emacs-directory "elpa/"))
             (pkg-dir-name (format "%s-%s.%s"
                                   (symbol-name pkg-name)
                                   (car version)
                                   (cadr version))))
        (expand-file-name (concat elpa-dir pkg-dir-name))))
     (t (package-desc-dir (cadr pkg-desc))))))

(defun configuration-layer//get-package-deps-from-alist (pkg-name)
  "Return the dependencies alist for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (cond
       ((version< emacs-version "24.3.50") (aref (cdr pkg-desc) 1))
       (t (package-desc-reqs (cadr pkg-desc)))))))

(defun configuration-layer//get-package-deps-from-archive (pkg-name)
  "Return the dependencies alist for a PKG-NAME from the archive data."
  (let* ((pkg-arch (assq pkg-name package-archive-contents))
         (reqs (when pkg-arch (if (version< emacs-version "24.3.50")
                              (aref (cdr pkg-arch) 1)
                            (package-desc-reqs (cadr pkg-arch))))))
    ;; recursively get the requirements of reqs
    (dolist (req reqs)
      (let* ((pkg-name2 (car req))
             (reqs2 (configuration-layer//get-package-deps-from-archive
                     pkg-name2)))
        (when reqs2 (setq reqs (append reqs2 reqs)))))
    reqs))

(defun configuration-layer//get-package-version-string (pkg-name)
  "Return the version string for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (cond
       ((version< emacs-version "24.3.50") (package-version-join
                                            (aref (cdr pkg-desc) 0)))
       (t (package-version-join (package-desc-version (cadr pkg-desc))))))))

(defun configuration-layer//get-package-version (pkg-name)
  "Return the version list for package with name PKG-NAME."
  (let ((version-string (configuration-layer//get-package-version-string pkg-name)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun configuration-layer//get-latest-package-version-string (pkg-name)
  "Return the version string for package with name PKG-NAME."
  (let ((pkg-arch (assq pkg-name package-archive-contents)))
    (when pkg-arch
      (cond
       ((version< emacs-version "24.3.50") (package-version-join
                                            (aref (cdr pkg-arch) 0)))
       (t (package-version-join (package-desc-version (cadr pkg-arch))))))))

(defun configuration-layer//get-latest-package-version (pkg-name)
  "Return the versio list for package with name PKG-NAME."
  (let ((version-string
         (configuration-layer//get-latest-package-version-string pkg-name)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun configuration-layer//package-delete (pkg-name)
  "Delete package with name PKG-NAME."
  (cond
   ((version< emacs-version "24.3.50")
    (let ((v (configuration-layer//get-package-version-string pkg-name)))
      (when v (package-delete (symbol-name pkg-name) v))))
   ((version<= "25.0.50" emacs-version)
    (let ((p (cadr (assq pkg-name package-alist))))
      ;; add force flag to ignore dependency checks in Emacs25
      (when p (package-delete p t t))))
   (t (let ((p (cadr (assq pkg-name package-alist))))
        (when p (package-delete p))))))

(defun configuration-layer//filter-used-themes (orphans)
  "Filter out used theme packages from ORPHANS candidates.
Returns the filtered list."
  (delq nil (mapcar (lambda (x)
                      (and (not (memq x spacemacs-used-theme-packages))
                           x)) orphans)))

(defun configuration-layer/delete-orphan-packages (packages)
  "Delete PACKAGES if they are orphan."
  (interactive)
  (let* ((dependencies (configuration-layer//get-packages-dependencies))
         (implicit-packages (configuration-layer//get-implicit-packages
                             configuration-layer--used-distant-packages))
         (orphans (configuration-layer//filter-used-themes
                   (configuration-layer//get-orphan-packages
                    configuration-layer--used-distant-packages
                    implicit-packages
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

