;;; helm-packages.el --- helm interface to manage packages  -*- lexical-binding: t; -*- 

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'package)
(require 'helm-utils) ; For with-helm-display-marked-candidates.


(defclass helm-packages-class (helm-source-in-buffer)
  ((coerce :initform #'helm-symbolify)
   (find-file-target :initform #'helm-packages-quit-an-find-file)
   (filtered-candidate-transformer
    :initform
    '(helm-packages-transformer
      (lambda (candidates _source)
        (sort candidates #'helm-generic-sort-fn))))
   (update :initform #'helm-packages--refresh-contents))
  "A class to define `helm-packages' sources.")

;;; Actions
;;
;;
(defun helm-packages-upgrade (_candidate)
  "Helm action for upgrading marked packages."
  (let ((mkd (helm-marked-candidates)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Upgrade %s packages? " (length mkd)))
        (mapc #'package-upgrade mkd)))))

(defun helm-packages-describe (candidate)
  "Helm action for describing package CANDIDATE."
  (describe-package candidate))

(defun helm-packages-visit-homepage (candidate)
  "Helm action for visiting package CANDIDATE home page."
  (let* ((id (package-get-descriptor candidate))
         (name (package-desc-name id))
         (extras (package-desc-extras id))
         (url (and (listp extras) (cdr-safe (assoc :url extras)))))
    (if (stringp url)
        (browse-url url)
      (message "Package %s has no homepage"
               (propertize (symbol-name name)
                           'face 'font-lock-keyword-face)))))

(defun helm-packages-package-reinstall (_candidate)
  "Helm action for reinstalling marked packages."
  (let ((mkd (helm-marked-candidates)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Reinstall %s packages? " (length mkd)))
        (mapc #'package-reinstall mkd)))))

(defun helm-packages-delete-1 (packages &optional force)
  "Run `package-delete' on PACKAGES.
If FORCE is non nil force deleting packages."
  (mapc (lambda (x)
          (package-delete (package-get-descriptor x) force))
        packages))

(defun helm-packages-uninstall (_candidate)
  "Helm action for uninstalling marked packages.
Unlike `helm-packages-delete' this will refuse to delete packages when they are
needed by others packages as dependencies."
  (let ((mkd (helm-marked-candidates)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Uninstall %s packages? " (length mkd)))
        (helm-packages-delete-1 mkd)))))

(defun helm-packages-delete (_candidate)
  "Helm action for deleting marked packages.
Unlike `helm-packages-uninstall' this delete packages even when they are needed
as dependencies."
  (let ((mkd (helm-marked-candidates)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Delete %s packages? " (length mkd)))
        (helm-packages-delete-1 mkd 'force)))))

(defun helm-packages-recompile (_candidate)
  "Helm action for recompiling marked packages."
  (let ((mkd (helm-marked-candidates)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Recompile %s packages? " (length mkd)))
        (mapc #'package-recompile mkd)))))

(defun helm-packages-install (_candidate)
  "Helm action for installing marked packages."
  (let ((mkd (helm-marked-candidates)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Install %s packages? " (length mkd)))
        (condition-case err
            (mapc #'package-install mkd)
          (error "%S:\n Please refresh package list before installing" err))))))

(defun helm-packages-isolate-1 (packages)
    "Start an Emacs with only PACKAGES loaded.
Arg PACKAGES is a list of strings."
    (let* ((name (concat "package-isolate-" (mapconcat #'identity packages "_")))
           (deps (cl-loop for p in packages
                          for sym = (intern p)
                          nconc (package--dependencies sym))))
      (apply #'start-process name nil
             (list (expand-file-name invocation-name invocation-directory)
                   "-Q" "--debug-init"
                   (format "--eval=%S"
                           `(progn
                              (require 'package)
                              (setq package-load-list
                                    ',(append (mapcar (lambda (p) (list (intern p) t))
                                                      packages)
                                              (mapcar (lambda (p) (list p t)) deps)))
                              (package-initialize)))))))

(defun helm-packages-isolate (_candidate)
  "Start a new Emacs with only marked packages loaded."
  (let* ((mkd (helm-marked-candidates))
         (pkg-names (mapcar #'symbol-name mkd))
         (isolate (if (fboundp 'package-isolate)
                      #'package-isolate
                    #'helm-packages-isolate-1)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      pkg-names
      (when (y-or-n-p "Start a new Emacs with only package(s)? ")
        (funcall isolate pkg-names)))))

(defun helm-packages-quit-an-find-file (source)
  "`find-file-target' function for `helm-packages'."
  (let* ((sel (helm-get-selection nil nil source))
         (pkg (package-get-descriptor (intern sel))))
    (if (and pkg (package-installed-p pkg))
        (expand-file-name (package-desc-dir pkg))
      package-user-dir)))

;;; Transformers
;;
;;
(defun helm-packages-transformer (candidates _source)
  "Transformer function for `helm-packages'."
  (cl-loop for c in candidates
           for sym = (intern-soft c)
           for archive = (assq sym package-archive-contents)
           for id = (package-get-descriptor sym)
           for provider = (and archive (package-desc-archive (cadr archive)))
           for status = (and id (package-desc-status id))
           for version = (and id (mapconcat #'number-to-string (package-desc-version id) "."))
           for description = (and id (package-desc-summary id))
           for disp = (format "%s%s%s%s%s%s%s%s%s"
                              ;; Package name.
                              (propertize
                               c
                               'face (if (equal status "dependency")
                                         font-lock-type-face
                                       'font-lock-keyword-face)
                               'match-part c)
                              ;; Separator.
                              (make-string (1+ (- (helm-in-buffer-get-longest-candidate)
                                                  (length c)))
                                           ? )
                              ;; Package status.
                              (propertize
                               (or status "")
                               'face (if (equal status "dependency")
                                         'bold-italic
                                       'default))
                              ;; Separator.
                              (make-string (1+ (- 10 (length status))) ? )
                              ;; Package provider.
                              (or provider "")
                              ;; Separator.
                              (make-string (1+ (- 10 (length provider))) ? )
                              ;; Package version.
                              (or version "")
                              ;; Separator.
                              (make-string (1+ (- 20 (length version))) ? )
                              ;; Package description.
                              (if description
                                  (propertize description 'face 'font-lock-warning-face)
                                ""))
           collect (cons disp c)))

(defun helm-packages-transformer-1 (candidates _source)
  "Transformer function for `helm-packages' upgrade and delete sources."
  (cl-loop for c in candidates
           collect (cons (propertize c 'face 'font-lock-keyword-face) c)))

(defvar helm-packages--updated nil)
(defun helm-packages--refresh-contents ()
  (unless helm-packages--updated (package-refresh-contents))
  (helm-set-local-variable 'helm-packages--updated t))


;;;###autoload
(defun helm-packages (&optional arg)
  "Helm interface to manage packages.

With a prefix arg ARG refresh package list.

When installing or upgrading ensure to refresh the package list
to avoid errors with outdated packages no more availables."
  (interactive "P")
  (package-initialize)
  (when arg (helm-packages--refresh-contents))
  (let ((upgrades (package--upgradeable-packages))
        (removables (package--removable-packages)))
    (helm :sources (list
                    (helm-make-source "Availables for upgrade" 'helm-packages-class
                      :init (lambda ()
                              (helm-init-candidates-in-buffer 'global upgrades))
                      :filtered-candidate-transformer #'helm-packages-transformer-1
                      :action '(("Upgrade package(s)"
                                 . helm-packages-upgrade)))
                    (helm-make-source "Packages to delete" 'helm-packages-class
                      :init (lambda ()
                              (helm-init-candidates-in-buffer 'global removables))
                      :filtered-candidate-transformer #'helm-packages-transformer-1
                      :action '(("Delete package(s)" . helm-packages-delete)))
                    (helm-make-source "Installed packages" 'helm-packages-class
                      :init (lambda ()
                              (helm-init-candidates-in-buffer 'global
                                (mapcar #'car package-alist)))
                      :action '(("Describe package" . helm-packages-describe)
                                ("Visit homepage" . helm-packages-visit-homepage)
                                ("Reinstall package(s)"
                                 . helm-packages-package-reinstall)
                                ("Recompile package(s)" . helm-packages-recompile)
                                ("Uninstall package(s)" . helm-packages-uninstall)
                                ("Isolate package(s)" . helm-packages-isolate)))
                    (helm-make-source "Available external packages" 'helm-packages-class
                      :data (cl-loop for p in package-archive-contents
                                     for sym = (car p)
                                     for id = (package-get-descriptor sym)
                                     for status = (package-desc-status id)
                                     unless (or (and id (member
                                                         status
                                                         '("installed" "dependency" "source")))
                                                (and id (assoc sym package--builtins)))
                                     nconc (list (car p)))
                      :action '(("Describe package" . helm-packages-describe)
                                ("Visit homepage" . helm-packages-visit-homepage)
                                ("Install packages(s)"
                                 . helm-packages-install)))
                    (helm-make-source "Available built-in packages" 'helm-packages-class
                      :data (cl-loop for p in package--builtins
                                     ;; Show only builtins that are available as
                                     ;; well on (m)elpa. Other builtins don't
                                     ;; have a package-descriptor, the format is
                                     ;; (sym . [version reqs summary]).
                                     when (package-desc-p (package-get-descriptor (car p)))
                                     collect (car p))
                      :action '(("Describe package" . helm-packages-describe)
                                ("Visit homepage" . helm-packages-visit-homepage)
                                ("Install packages(s)"
                                 . helm-packages-install))))
          :buffer "*helm packages*")))

(provide 'helm-packages)

;;; helm-packages ends here
