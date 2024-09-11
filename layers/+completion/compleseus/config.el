;;; config.el --- compleseus configuration File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
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


(defvar compleseus-engine 'vertico
  "Options are `selectrum', and `vertico' to use as completion
  engine.")

(defvar compleseus-buffer-search-restrict-project t
  "If non-nil, `spacemacs/consult-line-multi' and `spacemacs/consult-line-multi-symbol'
will be restricted to buffers of the current project.
This is the default behaviour of `consult-line-multi', but it can be overriden
by using a prefix argument.

If nil, we invert the default behaviour, and thus restrict to buffers
of the current project only when a prefix argument is used.

To restrict the commands to buffers of the current layout, customize
the variable `spacemacs-layouts-restricted-functions'.")

(defcustom compleseus-switch-to-buffer-sources
  `(consult--source-hidden-buffer
    compleseus--source-buffers-hidden
    compleseus--source-persp-buffers
    compleseus--source-persp-modified-buffers
    consult--source-recent-file
    consult--source-bookmark
    consult--source-project-buffer-hidden
    consult--source-project-recent-file-hidden)
  "Sources used by `spacemacs/compleseus-switch-to-buffer'
when persp-mode is used.
See also `consult-buffer-sources'.
See `consult--multi' for a description
of the source data structure."
  :type '(repeat symbol))

(defvar compleseus--source-buffers-hidden nil
  "Like `consult--source-buffer' but hidden by default
and with narrowing key \"B\".")
(with-eval-after-load 'consult
  (setq compleseus--source-buffers-hidden
        `(:name "Buffers (all layouts)"
          :hidden t
          :narrow (?B . "Buffers")
          ,@consult--source-buffer)))

(defvar compleseus--source-persp-modified-buffers
  `(:name "Modified Buffers"
          :narrow   (?* . "Modified Layout Buffers")
          :hidden   t
          :category buffer
          :face     consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :items
          ,(lambda ()
             (consult--buffer-query ;; :sort 'visibility
              :predicate (lambda (buff)
                           (and (compleseus//persp-contain-buffer-p buff)
                                (buffer-file-name buff)
                                (buffer-modified-p buff)))
              ;; :directory 'project
              :as #'consult--buffer-pair)))
  "Per-perspective modified buffer source.")
(define-obsolete-variable-alias 'consult--source-modified-persp-buffers
  'compleseus--source-persp-modified-buffers "2024-09")

(defvar compleseus--source-persp-buffers
  `(
    :name     "Layout Buffers"
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items
    ,(lambda ()
       (consult--buffer-query
        :sort 'visibility
        :predicate #'compleseus//persp-contain-buffer-p
        :as #'consult--buffer-pair)))
  "Per-perspective buffer source.")
(define-obsolete-variable-alias 'consult--source-persp-buffers
  'compleseus--source-persp-buffers "2024-09")
