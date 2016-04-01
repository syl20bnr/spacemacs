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
        (configuration-layer//load-layer-files helm-spacemacs-all-layers
                                               '("funcs.el" "config.el"))
        (setq helm-spacemacs-all-packages (configuration-layer/get-packages
                                           helm-spacemacs-all-layers)))))

;;;###autoload
(defun helm-spacemacs ()
  "Layers discovery with helm interface."
  (interactive)
  (helm-spacemacs-mode)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs//layer-source)
                   ,(helm-spacemacs//package-source)
                   ,(helm-spacemacs//toggle-source))))

(defun helm-spacemacs//layer-source ()
  "Construct the helm source for the layer section."
  `((name . "Layers")
    (candidates . ,(sort (configuration-layer/get-layers-list) 'string<))
    (action . (("Open README.md" . helm-spacemacs//layer-action-open-readme)
               ("Open packages.el" . helm-spacemacs//layer-action-open-packages)
               ("Open extensions.el" . helm-spacemacs//layer-action-open-extensions)))))

(defun helm-spacemacs//package-source ()
  "Construct the helm source for the packages."
  `((name . "Packages")
    (candidates . ,(helm-spacemacs//package-candidates))
    (action . (("Go to init function" . helm-spacemacs//package-action-goto-init-func)))))

(defun helm-spacemacs//package-candidates ()
  "Return the sorted candidates for package source."
  (let (result)
    (ht-aeach (dolist (layer value)
                (push (format "(%s) %s" layer key) result))
              helm-spacemacs-all-packages)
    (sort result 'string<)))

(defun helm-spacemacs//toggle-source ()
  "Construct the helm source for the toggles."
  `((name . "Toggles")
    (candidates . ,(helm-spacemacs//toggle-candidates))
    (action . (("Toggle" . helm-spacemacs//toggle)))))

(defun helm-spacemacs//toggle-candidates ()
  "Return the sorted candidates for toggle source."
  (let (result)
    (dolist (toggle spacemacs-toggles)
      (push (symbol-name (car toggle)) result))
    (sort result 'string<)))

(defun helm-spacemacs//layer-action-open-file (file candidate)
  "Open FILE of the passed CANDIDATE."
  (let ((path (file-name-as-directory
               (concat (ht-get configuration-layer-paths
                               (intern candidate))
                       candidate))))
    (find-file (concat path file))))

(defun helm-spacemacs//layer-action-open-readme (candidate)
  "Open the `README.md' file of the passed CANDIDATE."
  (helm-spacemacs//layer-action-open-file "README.md" candidate))

(defun helm-spacemacs//layer-action-open-packages (candidate)
  "Open the `packages.el' file of the passed CANDIDATE."
  (helm-spacemacs//layer-action-open-file "packages.el" candidate))

(defun helm-spacemacs//layer-action-open-extensions (candidate)
  "Open the `extensions.el' file of the passed CANDIDATE."
  (helm-spacemacs//layer-action-open-file "extensions.el" candidate))

(defun helm-spacemacs//package-action-goto-init-func (candidate)
  "Open the file `packages.el' and go to the init function."
  (save-match-data
    (string-match "^(\\(.+\\))\s\\(.+\\)$" candidate)
    (let* ((layer (match-string 1 candidate))
           (package (match-string 2 candidate))
           (path (file-name-as-directory
                  (concat (ht-get configuration-layer-paths (intern layer))
                          layer)))
           (filename (concat path "packages.el")))
      (find-file filename)
      (goto-char (point-min))
      (re-search-forward (format "init-%s" package))
      (beginning-of-line))))

(defun helm-spacemacs//toggle (candidate)
  "Toggle candidate."
  (let ((toggle (assq (intern candidate) spacemacs-toggles)))
    (when toggle
      (funcall (plist-get (cdr toggle) :function)))))

(provide 'helm-spacemacs)

;;; helm-spacemacs.el ends here
