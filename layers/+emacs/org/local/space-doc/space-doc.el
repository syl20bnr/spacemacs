;;; space-doc.el --- Spacemacs documentation minor mode.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Code:
(require 'face-remap)
(require 'org)

;;;###autoload
(define-minor-mode space-doc-mode
  "Buffer local minor mode for Spacemacs documentation files.
The mode hides `org-mode' meta tags like #+TITLE: while
keeping their content visible."
  :init-value nil
  :lighter " ❤"
  (if (eq major-mode 'org-mode)
      (if space-doc-mode
          (let ((bg (face-attribute 'default :background)))
            (progn
              ;; Make `org-mode' meta tags invisible.
              (set (make-local-variable
                    'spacemacs--org-face-remap-cookie-org-tag)
                   (face-remap-add-relative 'org-tag
                                            `(:foreground ,bg)))
              (set (make-local-variable
                    'spacemacs--org-face-remap-cookie-org-meta-line)
                   (face-remap-add-relative 'org-meta-line
                                            `(:foreground ,bg)))
              (set (make-local-variable
                    'spacemacs--org-face-remap-cookie-org-block-begin-line)
                   (face-remap-add-relative 'org-block-begin-line
                                            `(:foreground ,bg)))
              (set (make-local-variable
                    'spacemacs--org-face-remap-cookie-org-document-info-keyword)
                   (face-remap-add-relative 'org-document-info-keyword
                                            `(:foreground ,bg)))))
        (progn
          ;; Make `org-mode' meta tags visible.
          (face-remap-remove-relative
           spacemacs--org-face-remap-cookie-org-tag)
          (face-remap-remove-relative
           spacemacs--org-face-remap-cookie-org-meta-line)
          (face-remap-remove-relative
           spacemacs--org-face-remap-cookie-org-block-begin-line)
          (face-remap-remove-relative
           spacemacs--org-face-remap-cookie-org-document-info-keyword)
          (setq spacemacs--org-face-remap-p nil)))
    (progn (message (format "space-doc-mode error:%s isn't an org-mode buffer"
                            (buffer-name)))
           (setq org-mode nil))))

(provide 'space-doc)
;;; space-doc.el ends here
