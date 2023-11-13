;;; biblio-download.el --- Lookup bibliographic information and open access records from Dissemin -*- lexical-binding: t -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/cpitclaudel/biblio.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Download scientific papers directly from Emacs.
;;
;; This package plugs into `biblio-selection-mode' by adding an entry to the
;; extended actions menu (`x').

;;; Code:

(require 'biblio-core)

(defcustom biblio-download-directory nil
  "Where to put downloaded papers."
  :group 'biblio
  :type 'directory)

(defun biblio-download--action (record)
  "Retrieve a RECORD from Dissemin, and display it.
RECORD is a formatted record as expected by `biblio-insert-result'."
  (let-alist record
    (if .direct-url
        (let* ((fname (concat .identifier ".pdf"))
               (target (read-file-name "Save as (see also biblio-download-directory): "
                                       biblio-download-directory fname nil fname)))
          (url-copy-file .direct-url (expand-file-name target biblio-download-directory)))
      (user-error "This record does not contain a direct URL (try arXiv or HAL)"))))

;;;###autoload
(defun biblio-download--register-action ()
  "Add download to list of `biblio-selection-mode' actions."
  (add-to-list 'biblio-selection-mode-actions-alist
               '("Download this article" . biblio-download--action)))

;;;###autoload
(add-hook 'biblio-selection-mode-hook #'biblio-download--register-action)

(provide 'biblio-download)
;;; biblio-download.el ends here
