;;; delim-pad.el --- control space padding around delimiters

;; this file is not part of Emacs

;; Copyright (C) 2013 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: control space padding around delimiters
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Fri Jan  4 19:50:51 2013 (+0800)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 4
;; URL: https://github.com/lewang/delim-pad
;; Keywords:
;; Compatibility: only tested with Emacs 24, but send PR and I'll consider it.

;;; Installation:

;;      (require 'delim-pad)
;;      (delim-pad-mode 1)

;;; Commentary:

;; When manipulating padding space in front of a list with SPC and DEL, make
;; matching ending padding the same:
;;
;; e.g.
;;
;;     (    foo, bar    )
;;      ^^^^        ####
;;
;;   In (^) region, make (#) region match.


;; Usage example:
;;
;;      {a=>1, b=>2}
;;       ^
;;       <SPC>
;;      { a=>1, b=>2 }
;;        ^
;;       <DEL>
;;      {a=>1, b=>2}
;;
;; I made it to conform to some coding standards, but I actually haven't found
;; an instance where I wanted to add an uneven spacing inside delimiters; so
;; it's become a global minor-mode that I just leave enabled.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))


;;; BUGS:
;;;
;;; - When contents is entirely spaces it gets a bit icky, but that's not a
;;;   common use-case.

(defun delim-pad-orig-command ()
  "command mapped to current key seuqence if delim-pad-mode was off."
  (let* ((key (this-single-command-keys))
         (command (delim-pad-orig-command-1 key)))
    ;; (message "before:::binding: %S key: %S" command key)
    (setq command (or command
                      (when (equal key [backspace])
                        (delim-pad-orig-command-1 (kbd "DEL")))))
    ;; (message "after:::binding: %S key: %S" command key)
    command))

(defun delim-pad-orig-command-1 (key)
  "command mapped to KEY seuqence if delim-pad-mode was off."
  (let* ((delim-pad-mode nil)
         (command (key-binding key)))
    command))

(defun delim-pad-get-paddings (my-p)
  "return padding count (beg-count end-count point-in-front) of parent list around point *if*
  MY-P is in the front or back padding else NIL."
  (or (ignore-errors
        (let* ((list-begin (nth 1 (syntax-ppss my-p)))
               list-begin-inner
               list-end
               my-p-in-front-padding
               all-spaces
               front-padding-count
               back-padding-count
               should-fixup-spaces)
          (when list-begin
            (setq list-end (progn
                             (goto-char list-begin)
                             (forward-sexp)
                             (point))
                  list-begin-inner (1+ list-begin))
            (goto-char list-begin-inner)
            (skip-chars-forward " " list-end)
            (setq my-p-in-front-padding (<= my-p (point))
                  all-spaces (= (1+ (point)) list-end))
            (setq my-p-in-back-padding (and (not my-p-in-front-padding)
                                            (progn
                                              (goto-char (1- list-end))
                                              (skip-chars-backward " " my-p)
                                              (<= (point) my-p)))
                  should-fixup-spaces (and (or my-p-in-front-padding
                                               my-p-in-back-padding)
                                           (<= (point-at-bol) list-begin)
                                           (<= list-end (point-at-eol))))
      (when should-fixup-spaces
        (goto-char list-begin-inner)
        (setq front-padding-count (skip-chars-forward
                                   " "
                                   (if all-spaces
                                       my-p
                                     list-end)))
        (goto-char list-end)
        (backward-char 1)
        (setq back-padding-count (- (skip-chars-backward " " (if all-spaces
                                                                 my-p
                                                               list-begin))))

        (list front-padding-count back-padding-count list-begin my-p-in-front-padding)))))
      (list nil nil nil nil)))

(defun delim-pad-cmd (arg)
  "when space is pressed right after an opening delimiter, pad
the close with space as well."
  (interactive "*p")
  (let* ((delim-pad-mode nil)
         (orig-command (delim-pad-orig-command))
         (orig-p (copy-marker (point) t))
         (new-p orig-p))
    (unwind-protect
      (destructuring-bind (front-padding
                           back-padding
                           list-begin
                           orig-p-in-front-padding)
          (delim-pad-get-paddings orig-p)
        (goto-char orig-p)
        (call-interactively orig-command)
        (setq new-p (copy-marker (point)))
        (when front-padding
          (destructuring-bind (new-front-padding
                               new-back-padding
                               new-list-begin
                               new-point-in-front)
              (delim-pad-get-paddings orig-p)
            ;; make sure list structure hasn't changed
            (when (and new-list-begin
                       (= list-begin new-list-begin))
              (when orig-p-in-front-padding
                  (let ((diff (- new-front-padding new-back-padding)))
                    (goto-char list-begin)
                    (forward-sexp)
                    (backward-char)
                    (cond ((< diff 0)
                           (delete-region (+ (point) diff) (point)))
                          ((> diff 0)
                           (insert (make-string diff ? ))))))))))
      (goto-char new-p))))

(define-minor-mode delim-pad-mode
  "When pressing control space padding at beginning of delimited pair"
  :init-value nil
  :global t
  :lighter nil
  :version "0.1"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") 'delim-pad-cmd)
            (define-key map (kbd "DEL") 'delim-pad-cmd)
            (define-key map (kbd "C-d") 'delim-pad-cmd)
            map)
  :group 'delim-pad)

(defun delim-pad-promote-map ()
  (let ((delim-pad-mode-binding (assq 'delim-pad-mode minor-mode-map-alist)))
    (setq minor-mode-map-alist (delete delim-pad-mode-binding minor-mode-map-alist))
    (push delim-pad-mode-binding minor-mode-map-alist)))

;;; we need priority over paredit, which dumbly maps "DEL"
(eval-after-load "paredit"
  '(delim-pad-promote-map))



(provide 'delim-pad)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delim-pad.el ends here

