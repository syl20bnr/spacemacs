;; Spacemacs Contribution System

(load (concat user-emacs-directory "ht.el"))

(defvar spacemacs-config-layers '()
  "Alist of configuration layers with the form (symbol . plist) where
SYMBOL is the name of the layer and PLIST is a property list with the following
keys:
:contrib    if t then the layer is a contribution layer.
:dir        the absolute path to the base directory of the layer.
:init-dir   the absolute path to the directory containing the init-xxx files.
:ext-dir    the absolute path to the directory containing the extensions.
")
(defvar spacemacs-all-packages #s(hash-table size 200 data ())
  "Hash table of all declared packages in all layers where the key is a package
symbol and the value is the layer symbol where to initialize the package. ")
(defvar spacemacs-all-pre-extensions #s(hash-table size 64 data ())
  "Hash table of all declared pre-extensions in all layers where the key is a
extension symbol and the value is the layer symbol where to load and
initialize the extension. ")
(defvar spacemacs-all-post-extensions #s(hash-table size 64 data ())
  "Hash table of all declared post-extensions in all layers where the key is a
extension symbol and the value is the layer symbol where to load and
initialize the extension. ")

(defun spacemacs/declare-layer (sym &optional contrib)
  "Declare a layer with SYM name (symbol). If CONTRIB is non nil then the layer
 is a contribution layer."
  (let* ((sym-name (symbol-name sym))
         (base-dir (if contrib contrib-config-directory user-emacs-directory))
         (dir (format "%s%s/" base-dir sym-name))
         (ext-dir (format "%sextensions/" dir))
         (init-dir (format "%sinit/" dir)))
    (push (cons sym (list :contrib contrib :dir dir :init-dir init-dir
                          :ext-dir ext-dir)) spacemacs-config-layers)))

(defun spacemacs/load-layers ()
  "Load all declared layers."
  (spacemacs/load-layer-files '("funcs.el" "macros.el"))
  (spacemacs/read-packages-and-extensions)
  (spacemacs/initialize-extensions spacemacs-all-pre-extensions)
  (spacemacs/install-packages)
  (spacemacs/initialize-packages)
  (spacemacs/initialize-extensions spacemacs-all-post-extensions)
  (spacemacs/load-layer-files '("keybindings.el" "config.el")))

(defun spacemacs/load-layer-files (files)
  "Load the files of list FILES from all declared layers."
  (dolist (layer (reverse spacemacs-config-layers))
    (let* ((sym (car layer))
           (dir (plist-get (cdr layer) :dir)))
      (dolist (file files)
        (load (concat dir file))))))

(defun spacemacs/read-packages-and-extensions ()
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
        (load pkg-file)
        (dolist (pkg (eval (intern (format "%s-packages" (symbol-name sym)))))
          (puthash pkg sym spacemacs-all-packages))
        ;; extensions
        (load ext-file)
        (dolist (pkg (eval (intern (format "%s-pre-extensions"
                                           (symbol-name sym)))))
          (puthash pkg sym spacemacs-all-pre-extensions))
        (dolist (pkg (eval (intern (format "%s-post-extensions"
                                           (symbol-name sym)))))
          (puthash pkg sym spacemacs-all-post-extensions))))))

(defun spacemacs/install-packages ()
  "Install the packages all the packages if there are not currently installed."
  (interactive)
  (let* ((pkgs (ht-keys spacemacs-all-packages))
         (not-installed (remove-if 'package-installed-p pkgs)))
    ;; installation
    (if not-installed
        (if (y-or-n-p (format
              "there are %d packages to be installed. install them? "
              (length not-installed)))
            (progn (package-refresh-contents)
                   (dolist (pkg pkg-list)
                     (when (not (package-installed-p pkg))
                       (package-install pkg))))))))

(defun spacemacs/initialize-packages ()
  "Initialize all the declared packages."
  (ht-each 'spacemacs/initialize-package spacemacs-all-packages))

(defun spacemacs/initialize-package (pkg lsym)
  "Initialize the package PKG from the configuration layer LSYM."
  (let* ((layer (assq lsym spacemacs-config-layers))
         (init-dir (plist-get (cdr layer) :init-dir))
         (init-file (concat init-dir (format "init-%s.el" (symbol-name pkg)))))
    (if (and (package-installed-p pkg) (file-exists-p init-file))
        (load init-file))))

(defun spacemacs/initialize-extensions (ext-list)
  "Initialize all the declared in EXT-LIST hash table."
  (ht-each 'spacemacs/initialize-extension ext-list))

(defun spacemacs/initialize-extension (ext lsym)
  "Initialize the extension EXT from the configuration layer LSYM."
  (let* ((layer (assq lsym spacemacs-config-layers))
         (ext-dir (plist-get (cdr layer) :ext-dir))
         (init-dir (plist-get (cdr layer) :init-dir))
         (init-file (concat init-dir (format "init-%s.el" ext))))
    (add-to-list 'load-path (format "%s%s/" ext-dir ext))
    (if (file-exists-p init-file)
        (load init-file))))
