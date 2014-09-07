;; Spacemacs Contribution System
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(setq warning-minimum-level :error)

(load (concat user-emacs-directory "ht.el"))

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
symbol and the value is the layer symbol where to initialize the package. ")
(defvar spacemacs-all-pre-extensions #s(hash-table size 64 data ())
  "Hash table of all declared pre-extensions in all layers where the key is a
extension symbol and the value is the layer symbol where to load and
initialize the extension. ")
(defvar spacemacs-all-post-extensions #s(hash-table size 64 data ())
  "Hash table of all declared post-extensions in all layers where the key is a
extension symbol and the value is the layer symbol where to load and
initialize the extension. ")

(defun contribsys/declare-layer (sym &optional contrib)
  "Declare a layer with SYM name (symbol). If CONTRIB is non nil then the layer
 is a contribution layer."
  (let* ((sym-name (symbol-name sym))
         (base-dir (if contrib contrib-config-directory user-emacs-directory))
         (dir (format "%s%s/" base-dir sym-name))
         (ext-dir (format "%sextensions/" dir)))
    (push (cons sym (list :contrib contrib :dir dir :ext-dir ext-dir))
          spacemacs-config-layers)))

(defun contribsys/load-layers ()
  "Load all declared layers."
  (contribsys/load-layer-files '("funcs.el" "macros.el" "config.el"))
  (contribsys/read-packages-and-extensions)
  (contribsys/initialize-extensions spacemacs-all-pre-extensions)
  (contribsys/install-packages)
  (append-to-spacemacs-buf loading-text)
  (contribsys/initialize-packages)
  (contribsys/initialize-extensions spacemacs-all-post-extensions)
  (contribsys/load-layer-files '("keybindings.el")))

(defun contribsys/load-layer-files (files)
  "Load the files of list FILES from all declared layers."
  (dolist (layer (reverse spacemacs-config-layers))
    (let* ((sym (car layer))
           (dir (plist-get (cdr layer) :dir)))
      (dolist (file files)
        (load (concat dir file))))))

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
          (puthash pkg sym spacemacs-all-post-extensions)))))
  ;; number of chuncks for the loading screen
  (let ((total (+ (ht-size spacemacs-all-packages)
                  (ht-size spacemacs-all-pre-extensions)
                  (ht-size spacemacs-all-post-extensions))))
  (setq loading-dots-chunk-threshold (/ total loading-dots-chunk-count))))

(defun contribsys/install-packages ()
  "Install the packages all the packages if there are not currently installed."
  (interactive)
  (let* ((pkg-list (ht-keys spacemacs-all-packages))
         (not-installed (remove-if 'package-installed-p pkg-list))
         (not-installed-count (length not-installed)))
    ;; installation
    (if not-installed
        (progn
          (append-to-spacemacs-buf
           (format "Found %s new package(s) to install...\n"
                   not-installed-count))
          (append-to-spacemacs-buf "--> update repositories content\n")
          (redisplay)
          (package-refresh-contents)
          (setq installed-count 0)
          (dolist (pkg not-installed)
            (when (not (package-installed-p pkg))
              (package-install pkg))
            (setq installed-count (1+ installed-count))
            (replace-last-line-of-spacemacs-buf
             (format "--> %s/%s packages installed"
                     installed-count not-installed-count))
            (redisplay))
          (append-to-spacemacs-buf "\n")))))

(defun contribsys/initialize-packages ()
  "Initialize all the declared packages."
  (ht-each 'contribsys/initialize-package spacemacs-all-packages))

(defun contribsys/initialize-package (pkg lsym)
  "Initialize the package PKG from the configuration layer LSYM."
  (let* ((layer (assq lsym spacemacs-config-layers))
         (init-func (intern (format "%s/init-%s" (symbol-name lsym) pkg))))
    (loading-animation)
    (if (and (package-installed-p pkg) (fboundp init-func))
        (funcall init-func))))

(defun contribsys/initialize-extensions (ext-list)
  "Initialize all the declared extensions in EXT-LIST hash table."
  (ht-each 'contribsys/initialize-extension ext-list))

(defun contribsys/initialize-extension (ext lsym)
  "Initialize the extension EXT from the configuration layer LSYM."
  (let* ((layer (assq lsym spacemacs-config-layers))
         (ext-dir (plist-get (cdr layer) :ext-dir))
         (init-func (intern (format "%s/init-%s" (symbol-name lsym) ext))))
       (add-to-list 'load-path (format "%s%s/" ext-dir ext))
       (loading-animation)
       (if (fboundp init-func) (funcall init-func))))

(defun contribsys/declare-configuration-layers ()
  "Declare the configuration layer in order of appearance in list
dotspacemacs-configuration-layers defined in ~/.spacemacs."
  (if (boundp 'dotspacemacs-configuration-layers)
      (dolist (layer dotspacemacs-configuration-layers)
        (contribsys/declare-layer layer t))))
