;;; helm-spacemacs.el --- Spacemacs layer exploration with `helm'.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: helm, spacemacs
;; Version: 0.1
;; Package-Requires: ((helm "1.5"))

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
;; layers thanks to helm.

;;; Code:

(require 'cl)
(require 'ht)
(require 'helm)
(require 'core-configuration-layer)

(defvar helm-spacemacs-all-layers nil
  "Alist of all configuration layers.")

(defvar helm-spacemacs-all-packages nil
  "Hash table of all packages in all layers.")

;;;###autoload
(define-minor-mode helm-spacemacs-mode
  "Layers discovery with helm interface."
  :group 'spacemacs
  :global t)

(defun helm-spacemacs//init (&optional arg)
  (when (or arg (null helm-spacemacs-all-packages))
    (mapc (lambda (layer) (push (configuration-layer/make-layer layer)
                                helm-spacemacs-all-layers))
          (configuration-layer/get-layers-list))
    (dolist (layer helm-spacemacs-all-layers)
      (unless (configuration-layer/layer-usedp (oref layer :name))
        (configuration-layer//load-layer-files layer '("funcs.el"
                                                       "config.el"))))
    (setq helm-spacemacs-all-packages (configuration-layer/get-packages
                                       helm-spacemacs-all-layers))))

;;;###autoload
(defun helm-spacemacs (arg)
  "Layers discovery with helm interface."
  (interactive "P")
  (helm-spacemacs-mode)
  (helm-spacemacs//init arg)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs//documentation-source)
                   ,(helm-spacemacs//layer-source)
                   ,(helm-spacemacs//package-source)
                   ,(helm-spacemacs//dotspacemacs-source)
                   ,(helm-spacemacs//toggle-source))))

(defun helm-spacemacs//documentation-source ()
  "Construct the helm source for the documentation section."
  (helm-build-sync-source "Spacemacs Documentation"
    :candidates #'helm-spacemacs//documentation-candidates
    :persistent-action #'helm-spacemacs//documentation-action-open-file
    :keymap helm-map
    :action (helm-make-actions
             "Open Documentation" #'helm-spacemacs//documentation-action-open-file)))

(defun helm-spacemacs//documentation-candidates ()
  (let (result file-extension)
    (dolist (filename (directory-files spacemacs-docs-directory))
      (setq file-extension (file-name-extension filename))
      (when (or (equal file-extension "md")
                (equal file-extension "org"))
        (push filename result)))
    ;; delete DOCUMENTATION.org to make it the first guide
    (delete "DOCUMENTATION.org" result)
    (push "DOCUMENTATION.org" result)

    ;; give each document an appropriate title
    (mapcar (lambda (r)
              (cond
               ((string-equal r "CONTRIBUTE.org")
                `("How to contribute to Spacemacs" . ,r))
               ((string-equal r "CONVENTIONS.org")
                `("Spacemacs conventions" . ,r))
               ((string-equal r "DOCUMENTATION.org")
                `("Spacemacs documentation" . ,r))
               ((string-equal r "FAQ.org")
                `("Spacemacs FAQ" . ,r))
               ((string-equal r "HOWTOs.org")
                `("Quick HOW-TOs for Spacemacs" . ,r))
               ((string-equal r "LAYERS.org")
                `("Tips on writing layers for Spacemacs" . ,r))
               ((string-equal r "QUICK_START.org")
                `("Quick start guide for Spacemacs" . ,r))
               ((string-equal r "VIMUSERS.org")
                `("Vim users migration guide" . ,r))
               (t
                `(r . ,r))))
            result)))

(defun helm-spacemacs//documentation-action-open-file (candidate)
  "Open documentation FILE."
  (let ((file (concat spacemacs-docs-directory candidate)))
    (cond ((and (equal (file-name-extension file) "md")
                (not helm-current-prefix-arg))
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

(defun helm-spacemacs//layer-source ()
  "Construct the helm source for the layer section."
  `((name . "Layers")
    (candidates . ,(sort (configuration-layer/get-layers-list) 'string<))
    (candidate-number-limit)
    (action . (("Open README.org"
                . helm-spacemacs//layer-action-open-readme)
               ("Open packages.el"
                . helm-spacemacs//layer-action-open-packages)
               ;; TODO remove extensions in 0.105.0
               ("Open extensions.el"
                . helm-spacemacs//layer-action-open-extensions)))))

(defun helm-spacemacs//package-source ()
  "Construct the helm source for the packages."
  `((name . "Packages")
    (candidates . ,(helm-spacemacs//package-candidates))
    (candidate-number-limit)
    (action . (("Go to init function"
                . helm-spacemacs//package-action-goto-init-func)))))

(defun helm-spacemacs//package-candidates ()
  "Return the sorted candidates for package source."
  (let (result)
    (dolist (pkg helm-spacemacs-all-packages)
      (push (format "(%S) package: %S" (oref pkg :owner) (oref pkg :name))
            result))
    (sort result 'string<)))

(defun helm-spacemacs//toggle-source ()
  "Construct the helm source for the toggles."
  (helm-build-sync-source "Toggles"
    :candidates #'helm-spacemacs//toggle-candidates
    :persistent-action #'helm-spacemacs//toggle
    :keymap helm-map
    :action (helm-make-actions "Toggle" #'helm-spacemacs//toggle)))

(defun helm-spacemacs//toggle-candidates ()
  "Return the sorted candidates for toggle source."
  (let (result)
    (dolist (toggle spacemacs-toggles)
      (let* ((toggle-symbol (symbol-name (car toggle)))
             (toggle-name (capitalize (replace-regexp-in-string "-" " " toggle-symbol)))
             (toggle-doc (format "%s: %s" toggle-name (plist-get (cdr toggle) :documentation))))
        (if (plist-member (cdr toggle) :documentation)
            (push `(,toggle-doc . ,toggle-symbol) result)
          (push `(,toggle-name . ,toggle-symbol) result))))
    (setq result (cl-sort result 'string< :key 'car))
    result))

(defun helm-spacemacs//dotspacemacs-source ()
  `((name . "Dotfile")
    (candidates . ,(helm-spacemacs//dotspacemacs-candidates))
    (candidate-number-limit)
    (action . (("Go to variable" . helm-spacemacs//go-to-dotfile-variable)))))

(defun helm-spacemacs//dotspacemacs-candidates ()
  "Return the sorted candidates for all the dospacemacs variables."
  (sort (dotspacemacs/get-variable-string-list) 'string<))

(defun helm-spacemacs//layer-action-open-file (file candidate)
  "Open FILE of the passed CANDIDATE."
  (let ((path (if (and (equalp file "README.org") (equalp candidate "spacemacs"))
                  ;; Readme for spacemacs is in the project root
                  (ht-get configuration-layer-paths (intern candidate))
                (file-name-as-directory
                 (concat (ht-get configuration-layer-paths
                                 (intern candidate))
                         candidate)))))
    (if (and (equal (file-name-extension file) "org")
             (not helm-current-prefix-arg))
        (spacemacs/view-org-file (concat path file) "^" 'all)
      (find-file (concat path file)))))

(defun helm-spacemacs//layer-action-open-readme (candidate)
  "Open the `README.org' file of the passed CANDIDATE."
  (helm-spacemacs//layer-action-open-file "README.org" candidate))

(defun helm-spacemacs//layer-action-open-packages (candidate)
  "Open the `packages.el' file of the passed CANDIDATE."
  (helm-spacemacs//layer-action-open-file "packages.el" candidate))

;; TODO remove extensions in 0.105.0
(defun helm-spacemacs//layer-action-open-extensions (candidate)
  "Open the `extensions.el' file of the passed CANDIDATE."
  (helm-spacemacs//layer-action-open-file "extensions.el" candidate))

(defun helm-spacemacs//package-action-goto-init-func (candidate)
  "Open the file `packages.el' and go to the init function."
  (save-match-data
    (string-match "^(\\(.+\\))\s\\(.+\\):\s\\(.+\\)$" candidate)
    (let* ((layer (match-string 1 candidate))
           (type (match-string 2 candidate))
           (package (match-string 3 candidate))
           (path (file-name-as-directory
                  (concat (ht-get configuration-layer-paths (intern layer))
                          layer)))
           (filename (cond ((string-equal "package" type)
                            (concat path "packages.el"))
                           ;; TODO remove extensions in 0.105.0
                           (t (concat path "extensions.el")))))
      (find-file filename)
      (goto-char (point-min))
      (re-search-forward (format "init-%s" package))
      (beginning-of-line))))

(defun helm-spacemacs//toggle (candidate)
  "Toggle candidate."
  (let ((toggle (assq (intern candidate) spacemacs-toggles)))
    (when toggle
      (funcall (plist-get (cdr toggle) :function)))))

(defun helm-spacemacs//go-to-dotfile-variable (candidate)
  "Go to candidate in the dotfile."
  (find-file dotspacemacs-filepath)
  (goto-char (point-min))
  ;; try to exclude comments
  (re-search-forward (format "^[a-z\s\\(\\-]*%s" candidate))
  (beginning-of-line))


(provide 'helm-spacemacs)

;;; helm-spacemacs.el ends here
