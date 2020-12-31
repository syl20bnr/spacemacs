;;; funcs.el --- Auctex Layer Functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//latex-backend ()
  "Returns selected backend."
  (if latex-backend
      latex-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-auctex))))

(defun spacemacs//latex-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//latex-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes LaTeX-mode))
    (_ (when (configuration-layer/package-used-p 'company-auctex)
         (if (configuration-layer/package-used-p 'company-math)
             (spacemacs|add-company-backends
               :backends
               company-auctex-environments
               company-auctex-macros
               company-math-symbols-unicode
               company-math-symbols-latex
               :modes LaTeX-mode)
           (spacemacs|add-company-backends
             :backends
             (company-auctex-macros)
             company-auctex-symbols
             company-auctex-environments
             :modes LaTeX-mode)))
       (when (configuration-layer/package-used-p 'company-reftex)
         (spacemacs|add-company-backends
           :backends
           company-reftex-labels
           company-reftex-citations
           :modes LaTeX-mode)))))

(defun spacemacs//latex-setup-backend ()
  "Conditionally setup latex backend."
  (pcase (spacemacs//latex-backend)
    (`lsp (require 'lsp-latex)
          (lsp))))

(defun latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))
;; (setq build-proc (TeX-command latex-build-command 'TeX-master-file -1))
;; ;; Sometimes, TeX-command returns nil causing an error in set-process-sentinel
;; (when build-proc
;;   (set-process-sentinel build-proc 'latex//build-sentinel))))

(defun latex//build-sentinel (process event)
  (if (string= event "finished\n")
      (TeX-view)
    (message "Errors! Check with C-`")))

(defun latex//autofill ()
  "Check whether the pointer is currently inside one of the
environments described in `latex-nofill-env' and if so, inhibits
the automatic filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment latex-nofill-env))))
    (when do-auto-fill
      (do-auto-fill))))

(defun latex/auto-fill-mode ()
  "Toggle auto-fill-mode using the custom auto-fill function."
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'latex//autofill))

;; Rebindings for TeX-font
(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-medium () (interactive) (TeX-font nil ?\C-m))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-calligraphic () (interactive) (TeX-font nil ?\C-a))
(defun latex/font-small-caps () (interactive) (TeX-font nil ?\C-c))
(defun latex/font-sans-serif () (interactive) (TeX-font nil ?\C-f))
(defun latex/font-normal () (interactive) (TeX-font nil ?\C-n))
(defun latex/font-serif () (interactive) (TeX-font nil ?\C-r))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))
(defun latex/font-upright () (interactive) (TeX-font nil ?\C-u))
