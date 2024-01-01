;;; funcs.el --- search-engine Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun spacemacs//search-engine-source (engines)
  "return a source for helm selection"
  `((name . "Search Engines")
    (candidates . ,(mapcar (lambda (engine)
                             (cons (plist-get (cdr engine) :name)
                                   (intern (format "engine/search-%S"
                                                   (car engine)))))
                           engines))
    (action . (lambda (candidate) (call-interactively candidate)))))

(defun spacemacs/helm-search-engine-select ()
  "Set search engine to use with helm."
  (interactive)
  (helm :sources (list (spacemacs//search-engine-source
                        search-engine-alist))))

(defun spacemacs/ivy-search-engine-select ()
  "Set search engine to use with ivy."
  (interactive)
  (ivy-read "Search Engines: "
            (mapcar (lambda (engine)
                      (cons (plist-get (cdr engine) :name)
                            (intern (format "engine/search-%S"
                                            (car engine)))))
                    search-engine-alist)
            :action (lambda (candidate) (call-interactively (cdr candidate)))))

(defun spacemacs/search-engine-select ()
  "Set search engine to use."
  (interactive)
  (if (configuration-layer/layer-used-p 'ivy)
      (call-interactively 'spacemacs/ivy-search-engine-select)
    (call-interactively 'spacemacs/helm-search-engine-select)))
