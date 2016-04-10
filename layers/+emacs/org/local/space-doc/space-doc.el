;;; space-doc.el --- Spacemacs documentation minor mode.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; Description:
;; This package provides:
;;   - `space-doc-mode' - buffer local minor mode
;; for viewing the Spacemacs documentation files.
;; The mode hides org meta tags to improve readability.
;;   - `org-mode' link-type "https" that opens the local
;; copies of the Spacemacs documentation files with
;; `spacemacs/view-org-file' and supports GitHub style
;; heading links.
;;
;; For example, the link:
;;  https://github.com/syl20bnr/spacemacs/blob/develop/layers/org/README.org#links
;; Will be handled similary to as if it was:
;; file:~/.emacs.d/layers/org/README.org::*links
;; Also the `space-doc' mode will be applied.

;;; License: GPLv3
;;; Code:
(require 'face-remap)
(require 'org)

;;;###autoload
(define-minor-mode space-doc-mode
  "Buffer local minor mode for Spacemacs documentation files.
This mode:
 - hides `org-mode' meta tags like #+TITLE: while
keeping their content visible.
 - enables buffer local link  opening with `spacemacs//space-doc-open'."
  :init-value nil
  :lighter " SD"
  (if (derived-mode-p 'org-mode)
      (if space-doc-mode
          (let ((bg (face-attribute 'default :background)))
            ;; Make `space-doc' https link opener buffer local
            ;; and enable it only when `space-doc' mode is enabled.
            (make-local-variable 'org-link-types)
            (make-local-variable 'org-link-protocols)
            (org-add-link-type "https" 'spacemacs//space-doc-open)

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
                                          `(:foreground ,bg))))

        (kill-local-variable 'org-link-types)
        (kill-local-variable 'org-link-protocols)
        ;; Trigger `org-mode' internal updates.
        (org-add-link-type nil)

        ;; Make `org-mode' meta tags visible.
        (face-remap-remove-relative
         spacemacs--org-face-remap-cookie-org-tag)
        (face-remap-remove-relative
         spacemacs--org-face-remap-cookie-org-meta-line)
        (face-remap-remove-relative
         spacemacs--org-face-remap-cookie-org-block-begin-line)
        (face-remap-remove-relative
         spacemacs--org-face-remap-cookie-org-document-info-keyword)
        (setq spacemacs--org-face-remap-p nil))

    (message (format "space-doc-mode error:%s isn't an org-mode buffer"
                     (buffer-name)))
    (setq org-mode nil)))

(defun spacemacs//space-doc-open (path)
  "If the `path' argument is a link to an .org file that is located
in the Spacemacs GitHub repository - Visit the local copy
of the file with `spacemacs/view-org-file'.
Open all other links with `browse-url'."
  (let ((git-url-root-regexp
         (concat "\\/\\/github\\.com\\/syl20bnr"
                 "\\/spacemacs\\/blob\\/[^/]+\\/\\(.*\\.org\\)\\(\\#.*\\)?")))
    (if (string-match git-url-root-regexp path)
        (spacemacs/view-org-file (concat user-emacs-directory
                                         (match-string 1 path))
                                 (or (match-string 2 path)
                                     "^")
                                 'subtree)
      (browse-url (concat "https://" path)))))

(provide 'space-doc)
;;; space-doc.el ends here
