;;; funcs.el --- Perl5 Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
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


(defun spacemacs//perl5-setup-company ()
  "Conditionally setup company based on backend."
  (pcase perl5-backend
    ('lsp
     (spacemacs|add-company-backends ;; Activate lsp company explicitly to activate
       :backends 'company-capf       ;; standard backends as well
       :modes cperl-mode))
    ('company-plsense
     (spacemacs|add-company-backends
       :backends 'company-plsense
       :modes cperl-mode))))

(defun spacemacs//perl5-setup-backend ()
  "Conditionally setup perl5 backend."
  (when (eq perl5-backend 'lsp)
    (lsp)))

(defun spacemacs//perl5-setup-dap ()
  "Conditionally setup perl5 DAP integration."
  ;; currently DAP is only available using LSP
  (when (eq perl5-backend 'lsp)
    (add-to-list 'spacemacs--dap-supported-modes 'cperl-mode)
    (add-hook 'cperl-mode-hook #'dap-mode)))

(defun spacemacs//perl5-setup-binding (common-binding plsense-prefix)
  "Conditionally setup prefix and binding."
  (apply 'spacemacs/set-leader-keys-for-major-mode
         'cperl-mode common-binding)
  (add-hook 'cperl-mode-local-vars-hook
            (lambda ()
              (when (eq perl5-backend 'company-plsense)
                (cl-loop for x on plsense-prefix
                         by 'cddr
                         do (apply 'spacemacs/declare-prefix-for-mode
                                   'cperl-mode
                                   (list (car x) (cadr x))))))))

(defun spacemacs//perl5-smartparens-enable ()
  (define-key cperl-mode-map "{" nil))

(defun spacemacs//perl5-spartparens-disable ()
  (define-key cperl-mode-map "{" 'cperl-electric-lbrace))

(defun spacemacs/perltidy-format ()
  "Format Perl code with perltidy.
If region is active, operate on it, else operate on line."
  (interactive)
  (let ((old-point (point))
        (pos
         (if (use-region-p)
             (cons (region-beginning)
                   (if (char-equal ?\n (char-before (region-end)))
                       (region-end)
                     (save-excursion ;; must including terminating newline
                       (goto-char (region-end))
                       (1+ (line-end-position)))))
           (cons (line-beginning-position)
                 (1+ (line-end-position))))))
    (apply #'call-process-region (car pos) (cdr pos) perl5-perltidy-executable t '(t nil)
           "--quiet"
           "--standard-error-output"
           perl5-perltidy-options)
    (goto-char old-point)))

(defun spacemacs/perltidy-format-buffer ()
  "Format current buffer with perltidy."
  (interactive)
  (let ((old-point (point))
        (old-window-start (window-start)))
    (mark-whole-buffer)
    (spacemacs/perltidy-format)
    (goto-char old-point)
    (set-window-start (selected-window) old-window-start)))

(defun spacemacs/perltidy-format-function ()
  "Format current function with perltidy."
  (interactive)
  (let ((old-point (point))
        (old-window-start (window-start)))
    (mark-defun)
    (spacemacs/perltidy-format)
    (goto-char old-point)
    (set-window-start (selected-window) old-window-start)))

;; Keep this chunk always at the bottom.
;; It's killing the eys.
(defun spacemacs//perl5-custom-face ()
  "Some opinionated custmizations."
  ;; Don't highlight arrays and hashs in comments
  (font-lock-remove-keywords
   'cperl-mode
   '(("\\(\\([@%]\\|\\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
      (if (eq (char-after (match-beginning 2)) 37)
          'cperl-hash-face 'cperl-array-face) t)
     ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)" 1
      (if (= (- (match-end 2) (match-beginning 2)) 1)
          (if (eq (char-after (match-beginning 3)) 123)
              'cperl-hash-face 'cperl-array-face)
        font-lock-variable-name-face) t)
     ("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
      (2 font-lock-string-face t)
      ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}" nil nil
       (1 font-lock-string-face t)))
     ("[[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1 font-lock-string-face t)))

  (font-lock-add-keywords
   'cperl-mode
   '(("\\(\\([@%]\\|\\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
      (if (nth 4 (syntax-ppss))
          'font-lock-comment-face
        (if (eq (char-after (match-beginning 2)) ?%)
            'cperl-hash-face
          'cperl-array-face)) t)
     ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)" 1
      (if (nth 4 (syntax-ppss))
          'font-lock-comment-face
        (if (= (- (match-end 2) (match-beginning 2)) 1)
            (if (eq (char-after (match-beginning 3)) ?{)
                'cperl-hash-face
              'cperl-array-face)
          font-lock-variable-name-face)) t)
     ("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
      (2 (if (nth 4 (syntax-ppss))
             'font-lock-comment-face
           'font-lock-string-face) t)
      ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}" nil nil
       (1 (if (nth 4 (syntax-ppss))
              'font-lock-comment-face
            'font-lock-string-face) t)))
     ("[[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1
      (if (nth 4 (syntax-ppss))
          'font-lock-comment-face
        'font-lock-string-face) t)))

  ;; Use less horrible colors for cperl arrays and hashes
  (set-face-attribute 'cperl-array-face nil
                      :foreground  "#DD7D0A"
                      :background  'unspecified
                      :weight 'unspecified)
  (set-face-attribute 'cperl-hash-face nil
                      :foreground "OrangeRed3"
                      :background 'unspecified
                      :weight 'unspecified))
