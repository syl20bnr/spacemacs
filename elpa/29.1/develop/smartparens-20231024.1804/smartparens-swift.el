;;; smartparens-swift.el --- Additional configuration for Swift language buffers.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Wilfred Hughes,
;;               2022 Josh Caswell

;; Created: 4 April 2022
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of Smartparens.

;; Smartparens is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Smartparens is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Smartparens.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides some additional configuration for Swift.  To use
;; it, simply add:
;;
;; (require 'smartparens-config)
;;
;; alternatively, you can explicitly load these preferences:
;;
;; (require 'smartparens-swift)
;;
;; in your configuration.

;; For more info, see github readme at
;; https://github.com/Fuco1/smartparens

;;; Code:
(require 'smartparens)

(declare-function swift-mode "swift-mode")

(defun sp-swift-skip-match-angle-bracket (_ms _mb me)
  "Non-nil if we should ignore the bracket as valid delimiter."
  (save-excursion
    (goto-char me)
    (let ((on-fn-return-type
           (sp--looking-back-p (rx "->") nil))
          (on-range-operator
           (sp--looking-back-p (rx "..<") nil))
          (on-comparison
           (sp--looking-back-p (rx (or
                                    (seq space "<")
                                    (seq space ">")
                                    (seq space "<<")
                                    (seq space ">>")))
                               nil)))
      (or on-comparison on-fn-return-type on-range-operator))))

(defun sp-swift-filter-angle-brackets (_id action context)
  "Non-nil if we should allow ID's ACTION in CONTEXT for angle brackets."
  ;; See the docstring for `sp-pair' for the possible values of ID,
  ;; ACTION and CONTEXT.
  (cond
   ;; Inside strings, don't do anything with < or >.
   ((eq context 'string)
    nil)
   ((or (eq context 'comment)
        (eq context 'code))
    (let ((on-fn-return-type
           (looking-back (rx "->") nil))
          (on-range-operator
           (looking-back (rx "..<") nil))
          (on-comparison
           (looking-back (rx (or
                              (seq space "<")
                              (seq space ">")
                              (seq space "<<")
                              (seq space ">>")))
                         nil)))
      (cond
       ;; Only insert a matching > if we're not looking at one of the operators.
       ((eq action 'insert)
        (and (not on-comparison) (not on-fn-return-type) (not on-range-operator)))
       ;; Allow wrapping in a pair if the region is active and we're not on a
       ;; range operator.
       ((eq action 'wrap)
        (not on-range-operator))
       ;; When pressing >, autoskip if we're not looking at one of the
       ;; operators.
       ((eq action 'autoskip)
        (and (not on-comparison) (not on-fn-return-type) (not on-range-operator)))
       ;; Allow navigation, highlighting and strictness checks if it's
       ;; not one of the operators.
       ((eq action 'navigate)
        (and (not on-comparison) (not on-fn-return-type) (not on-range-operator))))))))

(sp-with-modes '(swift-mode)
  (sp-local-pair "<" ">"
                 :when '(sp-swift-filter-angle-brackets)
                 :skip-match 'sp-swift-skip-match-angle-bracket)
  (sp-local-pair "\"\"\"" "\"\"\""))

;; Swift has no sexp suffices.  This fixes slurping
;; (|foo).bar -> (foo.bar)
(add-to-list 'sp-sexp-suffix (list #'swift-mode 'regexp ""))

(provide 'smartparens-swift)

;;; smartparens-swift.el ends here
