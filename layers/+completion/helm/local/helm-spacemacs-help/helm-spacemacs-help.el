;;; helm-space-macs-help.el --- Space-macs layer exploration with `helm'.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: helm, space-macs
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
;; along with GNU e-macs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package adds a convenient way to discover Space-macs configuration
;; layers thanks to helm.

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'helm)
(require 'helm-command)
(require 'core-configuration-layer)

(defvar helm-space-macs--initialized nil
  "Non nil if helm-space-macs is initialized.")

;;;###autoload
(define-minor-mode helm-space-macs-help-mode
  "Layers discovery with helm interface."
  :group 'space-macs
  :global t)

(defun helm-space-macs-help//init (&optional arg)
  (when (or arg (null helm-space-macs--initialized))
    (configuration-layer/make-all-packages nil t)
    (setq helm-space-macs--initialized t)))

;;;###autoload
(defun helm-space-macs-help (arg)
  "Layers discovery with helm interface."
  (interactive "P")
  (helm-space-macs-help-mode)
  (helm-space-macs-help//init arg)
  (helm :buffer "*helm: space-macs*"
        :sources `(,(helm-space-macs-help//documentation-source)
                   ,(helm-space-macs-help//layer-source)
                   ,(helm-space-macs-help//package-source)
                   ,(helm-space-macs-help//dotspace-macs-source)
                   ,(helm-space-macs-help//toggle-source))))

;;;###autoload
(defun helm-space-macs-help-dotspace-macs ()
  "Helm session to search for dotfile variables."
  (interactive)
  (helm-space-macs-help-mode)
  (helm :buffer "*helm: space-macs*"
        :sources `(,(helm-space-macs-help//dotspace-macs-source))))

;;;###autoload
(defun helm-space-macs-help-layers ()
  "Helm session to search for layers."
  (interactive)
  (helm-space-macs-help-mode)
  (helm :buffer "*helm: space-macs*"
        :sources `(,(helm-space-macs-help//layer-source))))

;;;###autoload
(defun helm-space-macs-help-packages (arg)
  "Helm session to search for packages."
  (interactive "P")
  (helm-space-macs-help-mode)
  (helm-space-macs-help//init arg)
  (helm :buffer "*helm: space-macs*"
        :sources `(,(helm-space-macs-help//package-source))))

;;;###autoload
(defun helm-space-macs-help-docs ()
  "Helm session to search for documentation."
  (interactive)
  (helm-space-macs-help-mode)
  (helm :buffer "*helm: space-macs*"
        :sources `(,(helm-space-macs-help//documentation-source))))

;;;###autoload
(defun helm-space-macs-help-toggles ()
  "Helm session to search for toggles."
  (interactive)
  (helm-space-macs-help-mode)
  (helm :buffer "*helm: space-macs*"
        :sources `(,(helm-space-macs-help//toggle-source))))

(defun helm-space-macs-help//documentation-source ()
  "Construct the helm source for the documentation section."
  (helm-build-sync-source "Space-macs Documentation"
    :candidates #'helm-space-macs-help//documentation-candidates
    :persistent-action #'helm-space-macs-help//documentation-action-open-file
    :keymap helm-map
    :action (helm-make-actions
             "Open Documentation" #'helm-space-macs-help//documentation-action-open-file)))

(defun helm-space-macs-help//documentation-candidates ()
  (let (result file-extension)
    (dolist (filename (directory-files space-macs-docs-directory))
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
                `("How to contribute to Space-macs" . ,r))
               ((string-equal r "CONVENTIONS.org")
                `("Space-macs conventions" . ,r))
               ((string-equal r "DOCUMENTATION.org")
                `("Space-macs documentation" . ,r))
               ((string-equal r "FAQ.org")
                `("Space-macs FAQ" . ,r))
               ((string-equal r "LAYERS.org")
                `("Tips on writing layers for Space-macs" . ,r))
               ((string-equal r "QUICK_START.org")
                `("Quick start guide for Space-macs" . ,r))
               ((string-equal r "VIMUSERS.org")
                `("Vim users migration guide" . ,r))
               (t
                `(,r . ,r))))
            result)))

(defun helm-space-macs-help//documentation-action-open-file (candidate)
  "Open documentation FILE."
  (let ((file (if (string= candidate "CONTRIBUTING.org")
                  ;; CONTRIBUTING.org is a special case as it should be at the
                  ;; root of the repository to be linked as the contributing
                  ;; guide on GitHub.
                  (concat space-macs-start-directory candidate)
                (concat space-macs-docs-directory candidate))))
    (cond ((and (equal (file-name-extension file) "md")
                (not helm-current-prefix-arg))
           (condition-case-unless-debug nil
               (with-current-buffer (find-file-noselect file)
                 (gh-md-render-buffer)
                 (space-macs/kill-this-buffer))
             ;; if anything fails, fall back to simply open file
             (find-file file)))
          ((equal (file-name-extension file) "org")
           (space-macs/view-org-file file "^" 'all))
          (t
           (find-file file)))))

(defun helm-space-macs-help//layer-source ()
  "Construct the helm source for the layer section."
  `((name . "Layers")
    (candidates . ,(sort (configuration-layer/get-layers-list) 'string<))
    (candidate-number-limit)
    (keymap . ,helm-space-macs-help--layer-map)
    (action . (("Open README.org"
                . helm-space-macs-help//layer-action-open-readme)
               ("Open packages.el"
                . helm-space-macs-help//layer-action-open-packages)
               ("Open config.el"
                . helm-space-macs-help//layer-action-open-config)
               ("Open funcs.el"
                . helm-space-macs-help//layer-action-open-funcs)
               ("Open layers.el"
                . helm-space-macs-help//layer-action-open-layers)
               ("Install Layer"
                . helm-space-macs-help//layer-action-install-layer)
               ("Open README.org (for editing)"
                . helm-space-macs-help//layer-action-open-readme-edit)))))

(defvar helm-space-macs-help--layer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<S-return>") '(lambda () (interactive)
                                          ;; Add Layer
                                          (helm-select-nth-action 3)))
    (define-key map (kbd "<M-return>") '(lambda () (interactive)
                                          ;; Open packages.el
                                          (helm-select-nth-action 1)))
    map)
  "Keymap for Space-macs Layers sources")

(defun helm-space-macs-help//package-source ()
  "Construct the helm source for the packages."
  `((name . "Packages")
    (candidates . ,(helm-space-macs-help//package-candidates))
    (candidate-number-limit)
    (action . (("Go to configuration function"
                . helm-space-macs-help//package-action-goto-config-func)
               ("Describe"
                . helm-space-macs-help//package-action-describe)
               ("Recompile"
                . helm-space-macs-help//package-action-recompile)))))

(defun helm-space-macs-help//package-candidates ()
  "Return the sorted candidates for package source."
  (let (result)
    (dolist (pkg-name (configuration-layer/get-packages-list))
      (let* ((pkg (configuration-layer/get-package pkg-name))
             (owner (cfgl-package-get-safe-owner pkg))
             ;; the notion of owner does not make sense if the layer is not used
             (init-type (if (configuration-layer/layer-used-p owner)
                            "owner" "init")))
        (when owner
          (push (format "%s (%s: %S layer)"
                        (propertize (symbol-name (oref pkg :name))
                                    'face 'font-lock-type-face)
                        init-type
                        owner)
                result))
        (dolist (initfuncs `((,(oref pkg :owners) "init")
                             (,(oref pkg :pre-layers) "pre-init")
                             (,(oref pkg :post-layers) "post-init")))
          (dolist (layer (car initfuncs))
            (unless (and owner (eq owner layer))
              (push (format "%s (%s: %S layer)"
                            (propertize (symbol-name (oref pkg :name))
                                        'face 'font-lock-type-face)
                            (cadr initfuncs)
                            layer)
                    result))))))
    (sort result 'string<)))

(defun helm-space-macs-help//toggle-source ()
  "Construct the helm source for the toggles."
  (let ((candidates (helm-space-macs-help//toggle-candidates)))
    (helm-build-sync-source "Toggles"
      :candidates candidates
      :persistent-action #'helm-space-macs-help//toggle
      :keymap helm-map
      :action (helm-make-actions "Toggle" #'helm-space-macs-help//toggle))))

(defun helm-space-macs-help//toggle-candidates ()
  "Return the sorted candidates for toggle source."
  (let (result)
    (dolist (toggle space-macs-toggles)
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
                      (kbd (concat dotspace-macs-leader-key " "
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

(defun helm-space-macs-help//dotspace-macs-source ()
  `((name . "Dotfile")
    (candidates . ,(helm-space-macs-help//dotspace-macs-candidates))
    (candidate-number-limit)
    (action . (("Go to variable" . helm-space-macs-help//go-to-dotfile-variable)))))

(defun helm-space-macs-help//dotspace-macs-candidates ()
  "Return the sorted candidates for all the dospace-macs variables."
  (sort (dotspace-macs/get-variable-string-list) 'string<))

(defun helm-space-macs-help//layer-action-open-file
    (file candidate &optional edit)
  "Open FILE of the passed CANDIDATE.  If EDIT is false, open in view mode."
  (let ((path (configuration-layer/get-layer-path (intern candidate))))
    (if (and (equal (file-name-extension file) "org")
             (not helm-current-prefix-arg))
        (if edit
            (find-file (concat path file))
          (space-macs/view-org-file (concat path file) "^" 'all))
      (let ((filepath (concat path file)))
        (if (file-exists-p filepath)
            (find-file filepath)
          (message "%s does not have %s" candidate file))))))

(defun helm-space-macs-help//layer-action-open-readme (candidate)
  "Open the `README.org' file of the passed CANDIDATE for reading."
  (helm-space-macs-help//layer-action-open-file "README.org" candidate))

(defun helm-space-macs-help//layer-action-install-layer (candidate-layer)
  "Add CANDIDATE-LAYER to dotspace-macs file and reloads configuration"
  (when (dotspace-macs/add-layer (intern candidate-layer))
    (dotspace-macs/sync-configuration-layers)))

(defun helm-space-macs-help//layer-action-open-readme-edit (candidate)
  "Open the `README.org' file of the passed CANDIDATE for editing."
  (helm-space-macs-help//layer-action-open-file "README.org" candidate t))

(defun helm-space-macs-help//layer-action-open-packages (candidate)
  "Open the `packages.el' file of the passed CANDIDATE."
  (helm-space-macs-help//layer-action-open-file "packages.el" candidate))

(defun helm-space-macs-help//layer-action-open-config (candidate)
  "Open the `config.el' file of the passed CANDIDATE."
  (helm-space-macs-help//layer-action-open-file "config.el" candidate))

(defun helm-space-macs-help//layer-action-open-funcs (candidate)
  "Open the `funcs.el' file of the passed CANDIDATE."
  (helm-space-macs-help//layer-action-open-file "funcs.el" candidate))

(defun helm-space-macs-help//layer-action-open-layers (candidate)
  "Open the `layers.el' file of the passed CANDIDATE."
  (helm-space-macs-help//layer-action-open-file "layers.el" candidate))

(defun helm-space-macs-help//package-action-describe (candidate)
  "Describe the passed package using Space-macs describe function."
  (save-match-data
    (string-match "^\\(.+\\)\s(\\(.+\\) layer)$" candidate)
    (let* ((package (match-string 1 candidate)))
      (configuration-layer/describe-package (intern package)))))

(defun helm-space-macs-help//package-action-recompile (candidate)
  "Recompile the selected e-macs package."
  (save-match-data
    (string-match "^\\(.+\\)\s(\\(.+\\) layer)$" candidate)
    (let* ((package (match-string 1 candidate))
           (package-dir (configuration-layer//get-package-directory (intern package))))
      (if package-dir
          (space-macs/recompile-elpa t package-dir)))))

(defun helm-space-macs-help//package-action-goto-config-func (candidate)
  "Open the file `packages.el' and go to the init function."
  (save-match-data
    (string-match "^\\(.+\\)\s(\\(.*\\):\s\\(.+\\) layer.*)$" candidate)
    (let* ((package (match-string 1 candidate))
           (init-type (match-string 2 candidate))
           (layer (match-string 3 candidate))
           (path (file-name-as-directory
                  (configuration-layer/get-layer-path (intern layer))))
           (filename (concat path "packages.el")))
      (when (string-match-p "owner" init-type)
        (setq init-type "init"))
      (find-file filename)
      (goto-char (point-min))
      (re-search-forward (format "%s-%s" init-type package))
      (beginning-of-line))))

(defun helm-space-macs-help//toggle (candidate)
  "Toggle candidate."
  (let ((toggle (assq (intern candidate) space-macs-toggles)))
    (when toggle
      (funcall (plist-get (cdr toggle) :function)))))

(defun helm-space-macs-help//go-to-dotfile-variable (candidate)
  "Go to candidate in the dotfile."
  (find-file dotspace-macs-filepath)
  (goto-char (point-min))
  ;; try to exclude comments
  (re-search-forward (format "^[a-z\s\\(\\-]*%s" candidate))
  (beginning-of-line))

(provide 'helm-space-macs-help)

;;; helm-space-macs-help.el ends here


