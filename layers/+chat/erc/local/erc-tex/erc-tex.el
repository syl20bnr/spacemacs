;;; erc-tex.el --- LaTeX mathematical expressions rendering for ERC

;; Copyright (C) 2009-2023 David Vazquez

;; Last-modified: <2009-09-14 02:11:53 david>

;; Authors: David Vazquez <davazp@es.gnu.org>
;; Created: 12 Sep 2009
;; Keywords: comm, tex

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

;; erc-tex is a tiny ERC module which render LaTeX mathematical expressions
;; in your ERC chats. You will need both latex and dvipng in order to use this
;; module.
;;
;; Once erc-tex is avalaible for your Emacs, you can use `erc-tex-mode' to
;; toggle the module. This will render the text between $...$ as a LaTeX
;; expression. Indeed, you can use `erc-tex-image-edit', bound to `RET' on TeX
;; formulas to edit the TeX code in the prompt and resend the image.

;;; TODO:
;;    - Highlight the formulas according to ERC faces as erc-track.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'erc)


(defvar erc-tex-latex-program "latex"
  "Program name for invoking LaTeX.")

(defvar erc-tex-dvipng-program "dvipng"
  "Program name for invoking dvipng.")

(defvar erc-tex-image-size 1.2
  "Ratio of magnification.")


;; Error condition signaled when it cannot render a LaTeX expression.
(put 'erc-tex-bad-expression-error
     'error-conditions '(error erc-tex-bad-expression-error))


(defsubst erc-tex-run-latex (&rest arguments)
  "Launch LaTeX program with some arguments."
  (unless (zerop (apply #'call-process erc-tex-latex-program nil nil nil arguments))
    (signal 'erc-tex-bad-expression-error nil)))

(defsubst erc-tex-run-dvipng (&rest arguments)
  "Launch dvipng program with some arguments."
  (unless (zerop (apply #'call-process erc-tex-dvipng-program nil nil nil arguments))
    (signal 'erc-tex-bad-expression-error nil)))

;; Call to latex and dvipng in order to build a PNG image from the LaTeX
;; expression MATH-STRING. Return the image descriptor if it was sucessful,
;; NIL otherwise.
(defun erc-tex-make-image (math-expression fg bg)
  (condition-case nil
      (let* ((prefix   (concat temporary-file-directory (make-temp-name "erc-tex-")))
             (tex-file (concat prefix ".tex"))
             (dvi-file (concat prefix ".dvi"))
             (png-file (concat prefix ".png")))

        (with-temp-file tex-file
          (insert "\\documentclass{article}\n"
                  "\\pagestyle{empty}\n"
                  "\\usepackage{amsmath, amssymb, amsthm}\n"
                  "\\begin{document}\n"
                  "\\par\n"
                  "$"  math-expression "$"
                  "\\end{document}\n"))

        (erc-tex-run-latex (concat "-output-directory=" temporary-file-directory) tex-file)

        (cl-flet ((colorize (color)
                 ;; Return a string which stand for COLOR in the format that
                 ;; dvipng understands.
                 (let ((max (car (color-values "#ffffff"))))
                   (cl-destructuring-bind (r g b)
                       (color-values color)
                     (format "rgb %.02f %.02f %.02f"
                             (/ (float r) max)
                             (/ (float g) max)
                             (/ (float b) max))))))

          (erc-tex-run-dvipng "-x" (number-to-string (floor (* 1000 erc-tex-image-size)))
                               "-T" "tight"
                               "-fg" (colorize fg)
                               "-bg" (colorize bg)
                               "-o" png-file
                               dvi-file))
        (delete-file tex-file)
        (delete-file dvi-file)
        (create-image png-file 'png nil :margin '(0 . 5) :ascent 'center))

    ((erc-tex-bad-expression-error)
     ;; We do not delete auxiliary files if any error ocurred.
     )))



(defvar erc-tex-image-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") 'erc-tex-image-edit)
    keymap))

(defun erc-tex-image-edit ()
  (interactive)
  (let* ((start (point))
         (i start)
         (prop (get-char-property i 'display)))
    (while (eq prop (get-char-property i 'display))
      (setq i (1+ i)))
    (goto-char (point-max))
    (insert (buffer-substring-no-properties start i))))

(defun erc-tex-render (&optional fg bg)
  (let ((fg (or fg (face-foreground 'default)))
        (bg (or bg (face-background 'default))))
    (goto-char (point-min))
    (while (re-search-forward "\\$[^$]*\\$" nil t)
      (let* ((match (match-string-no-properties 0))
             (descp (erc-tex-make-image match fg bg)))
        (when descp
          (let (start end)
            (delete-region (match-beginning 0) (match-end 0))
            (setq start (point))
            (insert-image descp match)
            (setq end (point))
            (put-text-property start end 'keymap erc-tex-image-keymap)))))))


;;; Minor mode

(defun erc-tex-render-insert ()
  (erc-tex-render))

(defun erc-tex-render-send ()
  (erc-tex-render
   (face-foreground 'erc-input-face)
   (face-background 'erc-input-face)))

(define-erc-module tex latex
  "Display LaTeX mathematical expressions as images in ERC."
  ((add-hook 'erc-insert-modify-hook 'erc-tex-render-insert t)
   (add-hook 'erc-send-modify-hook 'erc-tex-render-send t))
  ((remove-hook 'erc-insert-modify-hook 'erc-tex-render-insert)
   (remove-hook 'erc-send-modify-hook 'erc-tex-render-send)))


(provide 'erc-tex)

;; Local variables:
;; fill-column: 78
;; indent-tabs-mode: nil
;; time-stamp-pattern: "10/^;; Last-modified: <%%>$"
;; End:

;;; erc-tex.el ends here
