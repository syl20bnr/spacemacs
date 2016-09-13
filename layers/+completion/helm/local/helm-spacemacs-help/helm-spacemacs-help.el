;;; helm-spacemacs-help.el --- Spacemacs layer exploration with `helm'.

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
(require 'helm-command)
(require 'helm-org)
(require 'core-configuration-layer)

(defvar helm-spacemacs--initialized nil
  "Non nil if helm-spacemacs is initialized.")

;;;###autoload
(define-minor-mode helm-spacemacs-help-mode
  "Layers discovery with helm interface."
  :group 'spacemacs
  :global t)

(defun helm-spacemacs-help//init (&optional arg)
  (when (or arg (null helm-spacemacs--initialized))
    (let ((configuration-layer--load-packages-files t)
          (configuration-layer--package-properties-read-onlyp t)
          (configuration-layer--inhibit-warnings t))
      (configuration-layer/discover-layers)
      (configuration-layer/declare-layers (configuration-layer/get-layers-list))
      (configuration-layer/make-all-packages)
      (setq helm-spacemacs--initialized t))))

;;;###autoload
(defun helm-spacemacs-help (arg)
  "Layers discovery with helm interface."
  (interactive "P")
  (helm-spacemacs-help-mode)
  (helm-spacemacs-help//init arg)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs-help//documentation-source)
                   ,(helm-spacemacs-help//layer-source)
                   ,(helm-spacemacs-help//package-source)
                   ,(helm-spacemacs-help//dotspacemacs-source)
                   ,(helm-spacemacs-help//toggle-source)
                   ,(helm-spacemacs-help//faq-source))))

;;;###autoload
(defun helm-spacemacs-help-dotspacemacs ()
  "Helm session to search for dotfile variables."
  (interactive)
  (helm-spacemacs-help-mode)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs-help//dotspacemacs-source))))

;;;###autoload
(defun helm-spacemacs-help-layers ()
  "Helm session to search for layers."
  (interactive)
  (helm-spacemacs-help-mode)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs-help//layer-source))))

;;;###autoload
(defun helm-spacemacs-help-packages (arg)
  "Helm session to search for packages."
  (interactive "P")
  (helm-spacemacs-help-mode)
  (helm-spacemacs-help//init arg)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs-help//package-source))))

;;;###autoload
(defun helm-spacemacs-help-docs ()
  "Helm session to search for documentation."
  (interactive)
  (helm-spacemacs-help-mode)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs-help//documentation-source))))

;;;###autoload
(defun helm-spacemacs-help-toggles ()
  "Helm session to search for toggles."
  (interactive)
  (helm-spacemacs-help-mode)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs-help//toggle-source))))

;;;###autoload
(defun helm-spacemacs-help-faq ()
  "Helm session to search for the FAQ."
  (interactive)
  (helm-spacemacs-help-mode)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs-help//faq-source))))

(defun helm-spacemacs-help//documentation-source ()
  "Construct the helm source for the documentation section."
  (helm-build-sync-source "Spacemacs Documentation"
    :candidates #'helm-spacemacs-help//documentation-candidates
    :persistent-action #'helm-spacemacs-help//documentation-action-open-file
    :keymap helm-map
    :action (helm-make-actions
             "Open Documentation" #'helm-spacemacs-help//documentation-action-open-file)))

(defun helm-spacemacs-help//documentation-candidates ()
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
                `(,r . ,r))))
            result)))

(defun helm-spacemacs-help//documentation-action-open-file (candidate)
  "Open documentation FILE."
  (let ((file (if (string= candidate "CONTRIBUTING.org")
                  ;; CONTRIBUTING.org is a special case as it should be at the
                  ;; root of the repository to be linked as the contributing
                  ;; guide on Github.
                  (concat spacemacs-start-directory candidate)
                (concat spacemacs-docs-directory candidate))))
    (cond ((and (equal (file-name-extension file) "md")
                (not helm-current-prefix-arg))
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

(defun helm-spacemacs-help//layer-source ()
  "Construct the helm source for the layer section."
  `((name . "Layers")
    (candidates . ,(sort (configuration-layer/get-layers-list) 'string<))
    (candidate-number-limit)
    (keymap . ,helm-spacemacs-help--layer-map)
    (action . (("Open README.org"
                . helm-spacemacs-help//layer-action-open-readme)
               ("Open packages.el"
                . helm-spacemacs-help//layer-action-open-packages)
               ("Open config.el"
                . helm-spacemacs-help//layer-action-open-config)
               ("Install Layer"
                . helm-spacemacs-help//layer-action-install-layer)
               ("Open README.org (for editing)"
                . helm-spacemacs-help//layer-action-open-readme-edit)))))

(defvar helm-spacemacs-help--layer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<S-return>") '(lambda () (interactive)
                                          ;; Add Layer
                                          (helm-select-nth-action 3)))
    (define-key map (kbd "<M-return>") '(lambda () (interactive)
                                          ;; Open packages.el
                                          (helm-select-nth-action 1)))
    map)
  "Keymap for Spacemacs Layers sources")

(defun helm-spacemacs-help//package-source ()
  "Construct the helm source for the packages."
  `((name . "Packages")
    (candidates . ,(helm-spacemacs-help//package-candidates))
    (candidate-number-limit)
    (action . (("Go to configuration function"
                . helm-spacemacs-help//package-action-goto-config-func)
               ("Describe"
                . helm-spacemacs-help//package-action-decribe)))))

(defun helm-spacemacs-help//package-candidates ()
  "Return the sorted candidates for package source."
  (let (result)
    (dolist (pkg-name (configuration-layer/get-packages-list))
      (let* ((pkg (configuration-layer/get-package pkg-name))
             (owner (cfgl-package-get-safe-owner pkg))
             ;; the notion of owner does not make sense if the layer is not used
             (init-type (if (configuration-layer/layer-usedp owner)
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

(defun helm-spacemacs-help//toggle-source ()
  "Construct the helm source for the toggles."
  (let ((candidates (helm-spacemacs-help//toggle-candidates)))
    (helm-build-sync-source "Toggles"
      :candidates candidates
      :persistent-action #'helm-spacemacs-help//toggle
      :keymap helm-map
      :action (helm-make-actions "Toggle" #'helm-spacemacs-help//toggle))))

(defun helm-spacemacs-help//toggle-candidates ()
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
                          (propertize key 'face 'helm-M-x-key)))))
        (if (plist-member (cdr toggle) :documentation)
            (push `(,toggle-doc . ,toggle-symbol) result)
          (push `(,toggle-name . ,toggle-symbol) result))))
    (setq result (cl-sort result 'string< :key 'car))
    result))

(defun helm-spacemacs-help//dotspacemacs-source ()
  `((name . "Dotfile")
    (candidates . ,(helm-spacemacs-help//dotspacemacs-candidates))
    (candidate-number-limit)
    (action . (("Go to variable" . helm-spacemacs-help//go-to-dotfile-variable)))))

(defun helm-spacemacs-help//dotspacemacs-candidates ()
  "Return the sorted candidates for all the dospacemacs variables."
  (sort (dotspacemacs/get-variable-string-list) 'string<))

(defun helm-spacemacs-help//layer-action-open-file
    (file candidate &optional edit)
  "Open FILE of the passed CANDIDATE.  If EDIT is false, open in view mode."
  (let ((path (configuration-layer/get-layer-path (intern candidate))))
    (if (and (equal (file-name-extension file) "org")
             (not helm-current-prefix-arg))
        (if edit
            (find-file (concat path file))
          (spacemacs/view-org-file (concat path file) "^" 'all))
      (let ((filepath (concat path file)))
        (if (file-exists-p filepath)
            (find-file filepath)
          (message "%s does not have %s" candidate file))))))

(defun helm-spacemacs-help//layer-action-open-readme (candidate)
  "Open the `README.org' file of the passed CANDIDATE for reading."
  (helm-spacemacs-help//layer-action-open-file "README.org" candidate))

(defun helm-spacemacs-help//layer-action-install-layer (candidate-layer)
  "Add CANDIDATE-LAYER to dotspacemacs file and reloads configuration"
  (when (dotspacemacs/add-layer (intern candidate-layer))
    (dotspacemacs/sync-configuration-layers)))

(defun helm-spacemacs-help//layer-action-open-readme-edit (candidate)
  "Open the `README.org' file of the passed CANDIDATE for editing."
  (helm-spacemacs-help//layer-action-open-file "README.org" candidate t))

(defun helm-spacemacs-help//layer-action-open-packages (candidate)
  "Open the `packages.el' file of the passed CANDIDATE."
  (helm-spacemacs-help//layer-action-open-file "packages.el" candidate))

(defun helm-spacemacs-help//layer-action-open-config (candidate)
  "Open the `config.el' file of the passed CANDIDATE."
  (helm-spacemacs-help//layer-action-open-file "config.el" candidate))

(defun helm-spacemacs-help//package-action-decribe (candidate)
  "Describe the passed package using Spacemacs describe function."
  (save-match-data
    (string-match "^\\(.+\\)\s(\\(.+\\) layer)$" candidate)
    (let* ((package (match-string 1 candidate)))
      (configuration-layer/describe-package (intern package)))))

(defun helm-spacemacs-help//package-action-goto-config-func (candidate)
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

(defun helm-spacemacs-help//toggle (candidate)
  "Toggle candidate."
  (let ((toggle (assq (intern candidate) spacemacs-toggles)))
    (when toggle
      (funcall (plist-get (cdr toggle) :function)))))

(defun helm-spacemacs-help//go-to-dotfile-variable (candidate)
  "Go to candidate in the dotfile."
  (find-file dotspacemacs-filepath)
  (goto-char (point-min))
  ;; try to exclude comments
  (re-search-forward (format "^[a-z\s\\(\\-]*%s" candidate))
  (beginning-of-line))

(defvar helm-spacemacs-help--faq-filename
  (concat spacemacs-docs-directory "FAQ.org")
  "Location of the FAQ file.")

(defun helm-spacemacs-help//faq-source ()
  "Construct the helm source for the FAQ."
  `((name . "FAQ")
    (candidates . ,(helm-spacemacs-help//faq-candidates))
    (candidate-number-limit)
    (action . (("Go to question" . helm-spacemacs-help//faq-goto-marker)))))

(defun helm-spacemacs-help//faq-candidate (cand)
  (let ((str (substring-no-properties (car cand))))
    (when (string-match "\\`.*/\\([^/]*\\)/\\(.*\\)\\'" str)
      (cons (concat (propertize
                     (match-string 1 str)
                     'face 'font-lock-type-face)
                    ": " (match-string 2 str))
            (cdr cand)))))

(defun helm-spacemacs-help//faq-candidates ()
  (let* ((helm-org-format-outline-path nil)
         (cands (helm-org-get-candidates (list helm-spacemacs-help--faq-filename)))
         section result)
    (dolist (c cands)
      (let ((str (substring-no-properties (car c))))
        (when (string-match "\\`\\* \\(.*\\)\\'" str)
          (setq section (match-string 1 str)))
        (when (string-match "\\`\\*\\* \\(.*\\)\\'" str)
          (push (cons (concat (propertize section 'face 'font-lock-type-face)
                              ": " (match-string 1 str))
                      (cdr c))
                result))))
    result))

(defun helm-spacemacs-help//faq-goto-marker (marker)
  (find-file helm-spacemacs-help--faq-filename)
  (goto-char marker)
  (org-show-context)
  (org-show-entry))

(provide 'helm-spacemacs-help)

;;; helm-spacemacs-help.el ends here
