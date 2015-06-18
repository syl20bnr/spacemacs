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

(require 'ht)
(require 'helm)
(require 'core-configuration-layer)

(defvar helm-spacemacs-all-layers '()
  "Alist of all configuration layers.")

(defvar helm-spacemacs-all-packages '()
  "Hash table of all packages in all layers.")

(defvar helm-spacemacs-all-pre-extensions '()
  "Hash table of all pre-extensions in all layers.")

(defvar helm-spacemacs-all-post-extensions '()
  "Hash table of all post-extensions in all layers.")

;;;###autoload
(define-minor-mode helm-spacemacs-mode
  "Layers discovery with helm interface."
  :group 'spacemacs
  :global t
  (setq helm-spacemacs-all-layers nil
        helm-spacemacs-all-packages nil)
  (if helm-spacemacs-mode
      (progn
        (mapc (lambda (layer) (push (configuration-layer//declare-layer layer)
                                    helm-spacemacs-all-layers))
              (configuration-layer/get-layers-list))
        (dolist (layer helm-spacemacs-all-layers)
          (unless (configuration-layer/layer-usedp (car layer))
            (configuration-layer//load-layer-files layer '("funcs.el"
                                                           "config.el"))))
        (setq helm-spacemacs-all-packages (configuration-layer/get-packages
                                           helm-spacemacs-all-layers))
        (setq helm-spacemacs-all-pre-extensions
              (configuration-layer/get-extensions helm-spacemacs-all-layers t))
        (setq helm-spacemacs-all-post-extensions
              (configuration-layer/get-extensions helm-spacemacs-all-layers)))))

;;;###autoload
(defun helm-spacemacs ()
  "Layers discovery with helm interface."
  (interactive)
  (helm-spacemacs-mode)
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
                `("Spacemacs starter guide" . ,r))
               ((string-equal r "HOWTOs.org")
                `("Quick HOW-TOs for Spacemacs" . ,r))
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
    (action . (("Open README.org" . helm-spacemacs//layer-action-open-readme)
               ("Open packages.el" . helm-spacemacs//layer-action-open-packages)
               ("Open extensions.el" . helm-spacemacs//layer-action-open-extensions)))))

(defun helm-spacemacs//package-source ()
  "Construct the helm source for the packages."
  `((name . "Packages")
    (candidates . ,(helm-spacemacs//package-candidates))
    (candidate-number-limit)
    (action . (("Go to init function" . helm-spacemacs//package-action-goto-init-func)))))

(defun helm-spacemacs//package-candidates ()
  "Return the sorted candidates for package source."
  (let (result)
    (ht-aeach (dolist (layer value)
                (push (format "(%s) package: %s" layer key) result))
              helm-spacemacs-all-packages)
    (ht-aeach (dolist (layer value)
                (push (format "(%s) pre-extension: %s" layer key) result))
              helm-spacemacs-all-pre-extensions)
    (ht-aeach (dolist (layer value)
                (push (format "(%s) post-extension: %s" layer key) result))
              helm-spacemacs-all-post-extensions)
    (sort result 'string<)))

(defun helm-spacemacs//toggle-source ()
  "Construct the helm source for the toggles."
  `((name . "Toggles")
    (candidates . ,(helm-spacemacs//toggle-candidates))
    (candidate-number-limit)
    (action . (("Toggle" . helm-spacemacs//toggle)))))

(defun helm-spacemacs//toggle-candidates ()
  "Return the sorted candidates for toggle source."
  (let (result)
    (dolist (toggle spacemacs-toggles)
      (push (symbol-name (car toggle)) result))
    (sort result 'string<)))

(defun helm-spacemacs//dotspacemacs-source ()
  `((name . "Dotfile")
    (candidates . ,(helm-spacemacs//dotspacemacs-candidates))
    (candidate-number-limit)
    (action . (("Go to variable" . helm-spacemacs//go-to-dotfile-variable)))))

(defun helm-spacemacs//dotspacemacs-candidates ()
  "Return the sorted candidates for all the dospacemacs variables."
  (sort (all-completions "" obarray
                         (lambda (x)
                           (and (boundp x)
                                (not (keywordp x))
                                (string-prefix-p "dotspacemacs"
                                                 (symbol-name x)))))
        'string<))

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
  "Open the `README.md' file of the passed CANDIDATE."
  (helm-spacemacs//layer-action-open-file "README.org" candidate))

(defun helm-spacemacs//layer-action-open-packages (candidate)
  "Open the `packages.el' file of the passed CANDIDATE."
  (helm-spacemacs//layer-action-open-file "packages.el" candidate))

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
