;;; org-ref-scifinder.el --- Emacs interface to SciFinder

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

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

;;; Commentary:
;; Hopefully this eventually integrates the Scifinder API into Emacs. For now,
;; there is just one function that opens Scifinder for you.

;;; Code:

;;;###autoload
(defun scifinder ()
  "Open https://scifinder.cas.org/scifinder/view/scifinder/scifinderExplore.jsf in a browser."
  (interactive)
  (browse-url "https://scifinder.cas.org/scifinder/view/scifinder/scifinderExplore.jsf"))

(provide 'org-ref-scifinder)

;;; org-ref-scifinder.el ends here
