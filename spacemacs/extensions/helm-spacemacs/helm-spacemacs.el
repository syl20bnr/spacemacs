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

(require 'config-system)
(require 'helm)

;;;###autoload
(defun helm-spacemacs ()
  "Layers discovery with helm interface."
  (interactive)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs//layer-source))))

(defun helm-spacemacs//layer-source ()
  "Construct the helm source for the layer section."
  `((name . "Layers")
    (candidates . ,(sort (ht-keys config-system-layer-paths) 'string<))
    (action . (("Open README.md" . helm-spacemacs//layer-action-open-readme)
               ("Open packages.el" . helm-spacemacs//layer-action-open-packages)
               ("Open extensions.el" . helm-spacemacs//layer-action-open-extensions)))))

(defun helm-spacemacs//layer-action-open-file (file candidate)
  "Open FILE of the passed CANDIDATE."
  (let ((path (file-name-as-directory
               (concat (ht-get config-system-layer-paths
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

(provide 'helm-spacemacs)

;;; helm-spacemacs.el ends here
