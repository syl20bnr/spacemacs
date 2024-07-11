;;; compleseus-spacemacs-help.el --- Spacemacs layer exploration with `completing-read'.

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; Keywords: consult, compleseus, spacemacs
;; Version: 0.1

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
;; layers.

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'core-configuration-layer)

(defvar compleseus-spacemacs--initialized nil
  "Non nil if compleseus-spacemacs is initialized.")

(defun compleseus-spacemacs-help//init (&optional arg)
  (when (or arg (null compleseus-spacemacs--initialized))
    (configuration-layer/make-all-packages nil t)
    (setq compleseus-spacemacs--initialized t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docs

(defun compleseus-spacemacs-help//documentation-candidates ()
  (let (result file-extension)
    (dolist (filename (directory-files spacemacs-docs-directory))
      (setq file-extension (file-name-extension filename))
      (when (or (equal file-extension "md")
                (equal file-extension "org"))
        (push filename result)))

    ;; CONTRIBUTING.org is a special case as it should be at the root of the
    ;; repository to be linked as the contributing guide on GitHub.
    (push "CONTRIBUTING.org" result)

    ;; delete DOCUMENTATION.org to make it the first guide
    (delete "DOCUMENTATION.org" result)
    (push "DOCUMENTATION.org" result)

    ;; give each document an appropriate title
    (mapcar (lambda (r)
              (cond
               ((string-equal r "BEGINNERS_TUTORIAL.org")
                `("Beginners tutorial" . ,r))
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

(defun compleseus-spacemacs-help//documentation-action-open-file (candidate)
  "Open documentation FILE."
  (message (format "thanh %s" candidate))
  (let* ((candidate (cdr candidate))
         (file (if (string= candidate "CONTRIBUTING.org")
                   ;; CONTRIBUTING.org is a special case as it should be at the
                   ;; root of the repository to be linked as the contributing
                   ;; guide on GitHub.
                   (concat spacemacs-start-directory candidate)
                 (concat spacemacs-docs-directory candidate))))
    (cond ((equal (file-name-extension file) "md")
           (condition-case-unless-debug nil
               (with-current-buffer (find-file-noselect file)
                 (gh-md-render-buffer)
                 (spacemacs/kill-this-buffer))
             ;; if anything fails, fall back to simply open file
             (find-file file)))
          ((equal (file-name-extension file) "org")
           (spacemacs/view-org-file file "^" 'all))
          (t
           (find-file file)))))

;;;###autoload
(defun compleseus-spacemacs-help-docs (arg)
  (interactive "P")
  (compleseus-spacemacs-help//init arg)
  (compleseus-spacemacs-help//documentation-action-open-file
   (let ((candidates (compleseus-spacemacs-help//documentation-candidates)))
     (assoc
      (completing-read
       "Spacemacs Documentation: "
       candidates)
      candidates
      #'string=))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layers

(defun compleseus-spacemacs-help//layer-candidates ()
  (sort (mapcar 'symbol-name (configuration-layer/get-layers-list))
        'string<))

(defun compleseus-spacemacs-help//layer-action-get-directory (candidate)
  "Get directory of layer passed CANDIDATE."
  (configuration-layer/get-layer-path (intern candidate)))

(defun compleseus-spacemacs-help//layer-action-open-file (file candidate &optional edit)
  "Open FILE of the passed CANDIDATE.
If the file does not exist and EDIT is true, create it; otherwise fall back
to opening dired at the layer directory.
If EDIT is false, open org files in view mode."
  (let* ((path (configuration-layer/get-layer-path (intern candidate)))
         (filepath (concat path file)))
    (cond ((and (equal (file-name-extension file) "org")
                (not edit)
                (file-exists-p filepath))
           (spacemacs/view-org-file filepath "^" 'all))
          ((or edit (file-exists-p filepath))
           (find-file filepath))
          (t
           (message "%s does not have %s" candidate file)
           (compleseus-spacemacs-help//layer-action-open-dired candidate)))))

(defun compleseus-spacemacs-help//layer-action-open-readme (candidate)
  "Open the `README.org' file of the passed CANDIDATE for reading."
  (compleseus-spacemacs-help//layer-action-open-file "README.org" candidate))

(defun compleseus-spacemacs-help//layer-action-add-layer (candidate)
  "Adds layer to dotspacemacs file and reloads configuration"
  (if (configuration-layer/layer-used-p (intern candidate))
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

(defun compleseus-spacemacs-help//layer-action-open-dired (candidate)
  "Open dired at the location of the passed layer CANDIDATE."
  (dired
   (compleseus-spacemacs-help//layer-action-get-directory candidate)))

(defun compleseus-spacemacs-help//layer-action-open-readme-edit (candidate)
  "Open the `README.org' file of the passed CANDIDATE for editing."
  (compleseus-spacemacs-help//layer-action-open-file "README.org" candidate t))

(defun compleseus-spacemacs-help//layer-action-open-config (candidate)
  "Open the `config.el' file of the passed CANDIDATE."
  (compleseus-spacemacs-help//layer-action-open-file "config.el" candidate))

(defun compleseus-spacemacs-help//layer-action-open-packages (candidate)
  "Open the `packages.el' file of the passed CANDIDATE."
  (compleseus-spacemacs-help//layer-action-open-file "packages.el" candidate))

(defun compleseus-spacemacs-help//layer-action-open-funcs (candidate)
  "Open the `funcs.el' file of the passed CANDIDATE."
  (compleseus-spacemacs-help//layer-action-open-file "funcs.el" candidate))

(defun compleseus-spacemacs-help//layer-action-open-layers (candidate)
  "Open the `layers.el' file of the passed CANDIDATE."
  (compleseus-spacemacs-help//layer-action-open-file "layers.el" candidate))

;;;###autoload
(defun compleseus-spacemacs-help-layers ()
  (interactive)
  (compleseus-spacemacs-help//init)
  (compleseus-spacemacs-help//layer-action-open-readme
   (completing-read
    "Spacemacs Layers: "
    (compleseus-spacemacs-help//layer-candidates))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layers and packages

(defun compleseus-spacemacs-help//help-candidates ()
  "Return the sorted candidates for package source."
  (let (result
        (left-column-width
         (number-to-string
          (cl-reduce
           (lambda (a x) (max (length (symbol-name x)) a))
           (configuration-layer/get-layers-list) :initial-value 0)))
        (owners (cl-remove-duplicates
                 (mapcar (lambda (pkg)
                           (let ((obj (configuration-layer/get-package pkg)))
                             (car (oref obj :owners))))
                         (configuration-layer/get-packages-list)))))
    (dolist (pkg-name (configuration-layer/get-packages-list))
      (let ((pkg (configuration-layer/get-package pkg-name)))
        (push (list (format (concat "%-" left-column-width "S %s %s")
                            (car (oref pkg :owners ))
                            (propertize (symbol-name (oref pkg :name))
                                        'face 'font-lock-type-face)
                            (propertize
                             (if (package-installed-p (oref pkg :name))
                                 "[installed]" "")
                             'face 'font-lock-comment-face))
                    (symbol-name
                     (car (oref pkg :owners )))
                    (symbol-name (oref pkg :name)))
              result)))
    (dolist (layer (delq nil
                         (cl-remove-if
                          (lambda (x) (memq x owners))
                          (configuration-layer/get-layers-list))))
      (push (list (format (concat "%-" left-column-width "S %s")
                          layer
                          (propertize "no packages"
                                      'face 'warning))
                  (symbol-name layer)
                  nil)
            result))
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun compleseus-spacemacs-help//help-action (args)
  "Open the file `packages.el' and go to the init function."
  (if (null (caddr args))
      (message "There are no packages associated with this layer.")
    (let* ((layer-str (cadr args))
           (layer-sym (intern layer-str))
           (package-str (caddr args))
           (path (configuration-layer/get-layer-path layer-sym))
           (filename (concat path "packages.el")))
      (find-file filename)
      (goto-char (point-min))
      (re-search-forward (format "init-%s" package-str))
      (beginning-of-line))))

(defun compleseus-spacemacs-help//help-action-describe-package (args)
  "Describe the passed package using Spacemacs describe function."
  (if (null (caddr args))
      (message "There are no packages associated with this layer.")
    (let ((package-str (caddr args)))
      (configuration-layer/describe-package (intern package-str)))))

(defun compleseus-spacemacs-help//help-action-open-dired (args)
  "Open the `packages.el' file of the passed `car' of ARGS."
  (dired
   (compleseus-spacemacs-help//layer-action-get-directory (cadr args))))

(defun compleseus-spacemacs-help//help-action-open-config (args)
  "Open the `config.el' file of the passed CANDIDATE."
  (compleseus-spacemacs-help//layer-action-open-file "config.el" (cadr args)))

(defun compleseus-spacemacs-help//help-action-open-packages (args)
  "Open the `packages.el' file of the passed CANDIDATE."
  (compleseus-spacemacs-help//layer-action-open-file "packages.el" (cadr args)))

(defun compleseus-spacemacs-help//help-action-open-readme (args)
  "Open the `README.org' file of the passed CANDIDATE for reading."
  (compleseus-spacemacs-help//layer-action-open-file "README.org" (cadr args)))

(defun compleseus-spacemacs-help//help-action-open-readme-edit (args)
  "Open the `README.org' file of the passed CANDIDATE for editing."
  (compleseus-spacemacs-help//layer-action-open-file "README.org" (cadr args) t))

(defun compleseus-spacemacs-help//help-action-add-layer (args)
  "Adds layer to dotspacemacs file and reloads configuration"
  (if (configuration-layer/layer-used-p (intern (cadr args)))
      (message "Layer already added.")
    (let ((dotspacemacs   (find-file-noselect (dotspacemacs/location))))
      (with-current-buffer dotspacemacs
        (beginning-of-buffer)
        (let ((insert-point (re-search-forward
                             "dotspacemacs-configuration-layers *\n?.*\\((\\)")))
          (insert (format "\n%s\n" (cadr args)))
          (indent-region insert-point (+ insert-point (length (cadr args))))
          (save-current-buffer)))
      (dotspacemacs/sync-configuration-layers))))

;;;###autoload
(defun compleseus-spacemacs-help ()
  (interactive)
  (compleseus-spacemacs-help//init)
  (let ((candidates (compleseus-spacemacs-help//help-candidates)))
    (compleseus-spacemacs-help//help-action
     (assoc
      (completing-read "Spacemacs Layers and Packages: " candidates)
      candidates))))


;;;###autoload
(defalias 'compleseus-spacemacs-help-packages 'compleseus-spacemacs-help)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggles

(defun compleseus-spacemacs-help//toggle-candidates ()
  "Return the sorted candidates for toggle source."
  (let (result)
    (dolist (toggle spacemacs-toggles)
      (let* ((toggle-symbol (symbol-name (car toggle)))
             (toggle-status (funcall (plist-get (cdr toggle) :predicate)))
             (toggle-name (capitalize (replace-regexp-in-string "-" " " toggle-symbol)))
             (toggle-doc (format "(%s) %s: %s"
                                 (if toggle-status "+" "-")
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
                          (propertize key 'face 'font-lock-keyword-face)))))
        (if (plist-member (cdr toggle) :documentation)
            (push `(,toggle-doc . ,toggle-symbol) result)
          (push `(,toggle-name . ,toggle-symbol) result))))
    (setq result (cl-sort result 'string< :key 'car))
    result))

(defun compleseus-spacemacs-help//toggle (candidate)
  "Toggle candidate."
  (let ((toggle (assq (intern candidate) spacemacs-toggles)))
    (when toggle
      (funcall (plist-get (cdr toggle) :function)))))

;;;###autoload
(defun compleseus-spacemacs-help-toggles ()
  (interactive)
  (compleseus-spacemacs-help//toggle
   (let ((candidates (compleseus-spacemacs-help//toggle-candidates)))
     (alist-get (completing-read "Spacemacs Toggles: " candidates)
                candidates
                nil
                nil
                #'string=))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .spacemacs vars

(defun compleseus-spacemacs-help//dotspacemacs-candidates ()
  "Return the sorted candidates for all the dospacemacs variables."
  (sort (dotspacemacs/get-variable-string-list) 'string<))

(defun compleseus-spacemacs-help//go-to-dotfile-variable (candidate)
  "Go to candidate in the dotfile."
  (find-file dotspacemacs-filepath)
  (goto-char (point-min))
  ;; try to exclude comments
  (re-search-forward (format "^[a-z\s\\(\\-]*%s" candidate))
  (beginning-of-line))

;;;###autoload
(defun compleseus-spacemacs-help-dotspacemacs ()
  (interactive)
  (compleseus-spacemacs-help//go-to-dotfile-variable
   (completing-read ".spacemacs variables: "
             (compleseus-spacemacs-help//dotspacemacs-candidates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FAQ

;;;###autoload
(defun compleseus-spacemacs-help-faq ()
  "Show FAQ and launch swiper session."
  (interactive)
  (find-file-read-only
   (expand-file-name "FAQ.org" spacemacs-docs-directory))
  (consult-imenu))

(provide 'compleseus-spacemacs-help)

;;; compleseus-spacemacs-help.el ends here
