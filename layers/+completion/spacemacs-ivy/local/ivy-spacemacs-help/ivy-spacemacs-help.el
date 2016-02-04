;;; ivy-spacemacs-help.el --- Spacemacs layer exploration with `ivy'.

;; Author: Justin Burkett <justin@burkett.cc>
;; Keywords: ivy, spacemacs
;; Version: 0.1
;; Package-Requires: ((ivy "0.7"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package adds a convenient way to discover Spacemacs configuration
;; layers thanks to ivy.

;;; Code:

(require 'cl)
(require 'ht)
(require 'ivy)
(require 'core-configuration-layer)

(defvar ivy-spacemacs-help-all-layers nil
  "Alist of all configuration layers.")

(defvar ivy-spacemacs-help-all-packages nil
  "Hash table of all packages in all layers.")

(defun ivy-spacemacs-help//init (&optional arg)
  (when (or arg (null ivy-spacemacs-help-all-packages))
    (mapc (lambda (layer) (push (configuration-layer/make-layer layer)
                                ivy-spacemacs-help-all-layers))
          (configuration-layer/get-layers-list))
    (dolist (layer ivy-spacemacs-help-all-layers)
      (unless (configuration-layer/layer-usedp (oref layer :name))
        (configuration-layer//load-layer-files layer '("funcs.el"
                                                       "config.el"))))
    (setq ivy-spacemacs-help-all-packages (configuration-layer/get-packages
                                           ivy-spacemacs-help-all-layers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docs

(defun ivy-spacemacs-help//documentation-candidates ()
  (let (result file-extension)
    (dolist (filename (directory-files spacemacs-docs-directory))
      (setq file-extension (file-name-extension filename))
      (when (or (equal file-extension "md")
                (equal file-extension "org"))
        (push filename result)))

    ;; CONTRIBUTING.org is a special case as it should be at the root of the
    ;; repository to be linked as the contributing guide on Github.
    (push "CONTRIBUTING.org" result)

    ;; delete DOCUMENTATION.org to make it the first guide
    (delete "DOCUMENTATION.org" result)
    (push "DOCUMENTATION.org" result)

    ;; give each document an appropriate title
    (mapcar (lambda (r)
              (cond
               ((string-equal r "CONTRIBUTING.org")
                `("How to contribute to Spacemacs" . ,r))
               ((string-equal r "CONVENTIONS.org")
                `("Spacemacs conventions" . ,r))
               ((string-equal r "DOCUMENTATION.org")
                `("Spacemacs documentation" . ,r))
               ((string-equal r "FAQ.org")
                `("Spacemacs FAQ" . ,r))
               ((string-equal r "LAYERS.org")
                `("Tips on writing layers for Spacemacs" . ,r))
               ((string-equal r "QUICK_START.org")
                `("Quick start guide for Spacemacs" . ,r))
               ((string-equal r "VIMUSERS.org")
                `("Vim users migration guide" . ,r))
               (t
                `(r . ,r))))
            result)))

(defun ivy-spacemacs-help//documentation-action-open-file (candidate)
  "Open documentation FILE."
  (let ((file (if (string= candidate "CONTRIBUTING.org")
                  ;; CONTRIBUTING.org is a special case as it should be at the
                  ;; root of the repository to be linked as the contributing
                  ;; guide on Github.
                  (concat user-emacs-directory candidate)
                (concat spacemacs-docs-directory candidate))))
    (cond ((equal (file-name-extension file) "md")
           (condition-case nil
               (with-current-buffer (find-file-noselect file)
                 (gh-md-render-buffer)
                 (kill-this-buffer))
             ;; if anything fails, fall back to simply open file
             (find-file file)))
          ((equal (file-name-extension file) "org")
           (spacemacs/view-org-file file "^" 'all))
          (t
           (find-file file)))))

;;;###autoload
(defun ivy-spacemacs-help-docs (arg)
  (interactive "P")
  (ivy-spacemacs-help//init arg)
  (ivy-read "Spacemacs Documentation: "
            (ivy-spacemacs-help//documentation-candidates)
            :action #'ivy-spacemacs-help//documentation-action-open-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layers

(defun ivy-spacemacs-help//layer-candidates ()
  (sort (mapcar 'symbol-name (configuration-layer/get-layers-list))
        'string<))

(defun ivy-spacemacs-help//layer-action-open-file (file candidate &optional edit)
  "Open FILE of the passed CANDIDATE.  If EDIT is false, open in view mode."
  (let ((path (if (and (equalp file "README.org") (equalp candidate "spacemacs"))
                  ;; Readme for spacemacs is in the project root
                  (ht-get configuration-layer-paths (intern candidate))
                (file-name-as-directory
                 (concat (ht-get configuration-layer-paths
                                 (intern candidate))
                         candidate)))))
    (if (equal (file-name-extension file) "org")
        (if edit
            (find-file (concat path file))
          (spacemacs/view-org-file (concat path file) "^" 'all))
      (find-file (concat path file)))))

(defun ivy-spacemacs-help//layer-action-open-readme (candidate)
  "Open the `README.org' file of the passed CANDIDATE for reading."
  (ivy-spacemacs-help//layer-action-open-file "README.org" candidate))

(defun ivy-spacemacs-help//layer-action-add-layer (candidate)
  "Adds layer to dotspacemacs file and reloads configuration"
  (if (configuration-layer/layer-usedp (intern candidate))
      (message "Layer already added.")
    (let ((dotspacemacs   (find-file-noselect (dotspacemacs/location))))
      (with-current-buffer dotspacemacs
        (beginning-of-buffer)
        (let ((insert-point (re-search-forward
                             "dotspacemacs-configuration-layers *\n?.*\\((\\)")))
          (insert (format "\n%s\n" candidate))
          (indent-region insert-point (+ insert-point (length candidate)))
          (save-current-buffer)))
      (dotspacemacs/sync-configuration-layers))))

(defun ivy-spacemacs-help//layer-action-open-readme-edit (candidate)
  "Open the `README.org' file of the passed CANDIDATE for editing."
  (ivy-spacemacs-help//layer-action-open-file "README.org" candidate t))

(defun ivy-spacemacs-help//layer-action-open-packages (candidate)
  "Open the `packages.el' file of the passed CANDIDATE."
  (ivy-spacemacs-help//layer-action-open-file "packages.el" candidate))

;;;###autoload
(defun ivy-spacemacs-help-layers ()
  (interactive)
  (ivy-spacemacs-help//init)
  (ivy-read "Spacemacs Layers: "
            (ivy-spacemacs-help//layer-candidates)
            :action 'ivy-spacemacs-help//layer-action-open-readme
            :caller 'ivy-spacemacs-help-layers))

(ivy-set-actions
 'ivy-spacemacs-help-layers
 '(("a" ivy-spacemacs-help//layer-action-add-layer "add layer")
   ("e" ivy-spacemacs-help//layer-action-open-readme-edit "add readme for editing")
   ("p" ivy-spacemacs-help//layer-action-open-packages "open packages.el")
   ("r" ivy-spacemacs-help//layer-action-open-readme "open readme")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layers and packages

(defun ivy-spacemacs-help//help-candidates ()
  "Return the sorted candidates for package source."
  (let (result
        (left-column-width
         (number-to-string
          (cl-reduce
           (lambda (a x) (max (length (symbol-name (oref x :name))) a))
           ivy-spacemacs-help-all-layers :initial-value 0)))
        (owners (cl-remove-duplicates
                 (mapcar (lambda (pkg) (oref pkg :owner))
                         ivy-spacemacs-help-all-packages))))
    (dolist (pkg ivy-spacemacs-help-all-packages)
      (push (list (format (concat "%-" left-column-width "S %s %s")
                          (oref pkg :owner)
                          (propertize (symbol-name (oref pkg :name))
                                      'face 'font-lock-type-face)
                          (propertize
                           (if (package-installed-p (oref pkg :name))
                               "[installed]" "")
                           'face 'font-lock-comment-face))
                  (symbol-name (oref pkg :owner))
                  (symbol-name (oref pkg :name)))
            result))
    (dolist (layer (delq nil
                         (cl-remove-if
                          (lambda (layer)
                            (memq (oref layer :name) owners))
                          ivy-spacemacs-help-all-layers)))
      (push (list (format (concat "%-" left-column-width "S %s")
                          (oref layer :name)
                          (propertize "no packages"
                                      'face 'warning))
                  (oref layer :name)
                  nil)
            result))
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun ivy-spacemacs-help//help-action (args)
  "Open the file `packages.el' and go to the init function."
  (if (null (cadr args))
      (message "There are no packages associated with this layer.")
    (let* ((layer-str (car args))
           (layer-sym (intern layer-str))
           (package-str (cadr args))
           (path (file-name-as-directory
                  (concat (ht-get configuration-layer-paths layer-sym)
                          layer-str)))
           (filename (concat path "packages.el")))
      (find-file filename)
      (goto-char (point-min))
      (re-search-forward (format "init-%s" package-str))
      (beginning-of-line))))

(defun ivy-spacemacs-help//help-action-add-layer (args)
  (let* ((layer-str (car args))
         (layer-sym (intern layer-str))
         (package-str (cadr args))
         (path (file-name-as-directory
                (concat (ht-get configuration-layer-paths layer-sym)
                        layer-str)))
         (filename (concat path "packages.el")))
    (find-file filename)
    (goto-char (point-min))
    (re-search-forward (format "init-%s" package-str))
    (beginning-of-line)))

(defun ivy-spacemacs-help//help-action-open-packages (args)
  "Open the `packages.el' file of the passed CANDIDATE."
  (ivy-spacemacs-help//layer-action-open-file "packages.el" (car args)))

(defun ivy-spacemacs-help//help-action-open-readme (args)
  "Open the `README.org' file of the passed CANDIDATE for reading."
  (ivy-spacemacs-help//layer-action-open-file "README.org" (car args)))

(defun ivy-spacemacs-help//help-action-open-readme-edit (args)
  "Open the `README.org' file of the passed CANDIDATE for editing."
  (ivy-spacemacs-help//layer-action-open-file "README.org" (car args) t))

(defun ivy-spacemacs-help//help-action-add-layer (args)
  "Adds layer to dotspacemacs file and reloads configuration"
  (if (configuration-layer/layer-usedp (intern (car args)))
      (message "Layer already added.")
    (let ((dotspacemacs   (find-file-noselect (dotspacemacs/location))))
      (with-current-buffer dotspacemacs
        (beginning-of-buffer)
        (let ((insert-point (re-search-forward
                             "dotspacemacs-configuration-layers *\n?.*\\((\\)")))
          (insert (format "\n%s\n" (car args)))
          (indent-region insert-point (+ insert-point (length (car args))))
          (save-current-buffer)))
      (dotspacemacs/sync-configuration-layers))))

;;;###autoload
(defun ivy-spacemacs-help ()
  (interactive)
  (ivy-spacemacs-help//init)
  (ivy-read "Spacemacs Layers and Packages: "
            (ivy-spacemacs-help//help-candidates)
            :action 'ivy-spacemacs-help//help-action
            :caller 'ivy-spacemacs-help))

(ivy-set-actions
 'ivy-spacemacs-help
 '(("a" ivy-spacemacs-help//help-action-add-layer "add layer")
   ("e" ivy-spacemacs-help//help-action-open-readme-edit "add readme for editing")
   ("p" ivy-spacemacs-help//help-action-open-packages "open packages.el")
   ("r" ivy-spacemacs-help//help-action-open-readme "open readme")))

;;;###autoload
(defalias 'ivy-spacemacs-help-packages 'ivy-spacemacs-help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggles

(defun ivy-spacemacs-help//toggle-candidates ()
  "Return the sorted candidates for toggle source."
  (let (result)
    (dolist (toggle spacemacs-toggles)
      (let* ((toggle-symbol (symbol-name (car toggle)))
             (toggle-name (capitalize (replace-regexp-in-string "-" " " toggle-symbol)))
             (toggle-doc (format "%s: %s"
                                 toggle-name
                                 (propertize
                                  (or (plist-get (cdr toggle) :documentation) "")
                                  'face 'font-lock-doc-face))))
        (when (plist-member (cdr toggle) :evil-leader)
          (let ((key (key-description
                      (kbd (concat dotspacemacs-leader-key " "
                                   (plist-get (cdr toggle) :evil-leader))))))
            (setq toggle-doc
                  (format "%s (%s)"
                          toggle-doc
                          (propertize key 'face 'helm-M-x-key)))))
        (if (plist-member (cdr toggle) :documentation)
            (push `(,toggle-doc . ,toggle-symbol) result)
          (push `(,toggle-name . ,toggle-symbol) result))))
    (setq result (cl-sort result 'string< :key 'car))
    result))

(defun ivy-spacemacs-help//toggle (candidate)
  "Toggle candidate."
  (let ((toggle (assq (intern candidate) spacemacs-toggles)))
    (when toggle
      (funcall (plist-get (cdr toggle) :function)))))

;;;###autoload
(defun ivy-spacemacs-help-toggles ()
  (interactive)
  (ivy-read "Spacemacs Toggles: "
            (ivy-spacemacs-help//toggle-candidates)
            :action 'ivy-spacemacs-help//toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .spacemacs vars

(defun ivy-spacemacs-help//dotspacemacs-candidates ()
  "Return the sorted candidates for all the dospacemacs variables."
  (sort (dotspacemacs/get-variable-string-list) 'string<))

(defun ivy-spacemacs-help//go-to-dotfile-variable (candidate)
  "Go to candidate in the dotfile."
  (find-file dotspacemacs-filepath)
  (goto-char (point-min))
  ;; try to exclude comments
  (re-search-forward (format "^[a-z\s\\(\\-]*%s" candidate))
  (beginning-of-line))

;;;###autoload
(defun ivy-spacemacs-help-dotspacemacs ()
  (interactive)
  (ivy-read ".spacemacs variables: "
            (ivy-spacemacs-help//dotspacemacs-candidates)
            :action 'ivy-spacemacs-help//go-to-dotfile-variable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FAQ

;;;###autoload
(defun ivy-spacemacs-help-faq ()
  "Show FAQ and launch swiper session."
  (interactive)
  (find-file-read-only
   (expand-file-name "FAQ.org" spacemacs-docs-directory))
  (swiper "\\*\\* "))

(provide 'ivy-spacemacs-help)

;;; ivy-spacemacs-help.el ends here
