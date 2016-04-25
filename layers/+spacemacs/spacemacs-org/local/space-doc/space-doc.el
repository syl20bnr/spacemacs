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

(define-minor-mode space-doc-mode
  "Buffer local minor mode for Spacemacs documentation files.
This mode:
 - hides `org-mode' meta tags like #+TITLE: while
keeping their content visible.
 - Improves emphasized region apparence.
 - enables buffer local link  opening with `spacemacs//space-doc-open'."
  :init-value nil
  :lighter " SD"
  (if (derived-mode-p 'org-mode)
      (dolist (modificator spacemacs--space-doc-modificators)
        (funcall modificator space-doc-mode))
    (message (format "space-doc-mode error:%s isn't an org-mode buffer"
                     (buffer-name)))
    (setq space-doc-mode nil)))

(defconst spacemacs--space-doc-modificators
  '(spacemacs//space-doc-set-space-doc-cache
    spacemacs//space-doc-modf-emphasis-overlays
    spacemacs//space-doc-modf-meta-tags-overlays
    spacemacs//space-doc-modf-link-protocol
    spacemacs//space-doc-modf-org-block-line-face-remap
    spacemacs//space-doc-modf-org-kbd-face-remap
    spacemacs//space-doc-modf-advice-org-do-emphasis-faces
    (lambda (flag) (spacemacs//space-doc-run-modfs-deferred
               '()
               flag )))
  "List of `space-doc' modificators for `org-mode' buffers.
The functions work with a current buffer and accept ENABLE(flag) argument.
If the argument has non-nil value - enable the modifications introduced
by the function. Otherwise - disable.")

(cl-defstruct spacemacs//space-doc-cache
  marker-face
  btn-marker-face
  kbd-marker)

(defvar-local spacemacs--space-doc-cache nil
  "Global variable of struct `spacemacs//space-doc-cache'.
It is set by `spacemacs//space-doc-set-space-doc-cache'.")

(defun spacemacs//space-doc-set-space-doc-cache (&optional flag)
  "Set `spacemacs--space-doc-cache' to filled
`spacemacs//space-doc-cache' structure."

  (setq spacemacs--space-doc-cache
        (if flag
            (let* ((kbd-bg (or (face-background 'org-kbd)
                               (face-background 'region)
                               'unspecified))
                   (table-bg (or (face-background 'org-table)
                                 (face-background 'default)
                                 'unspecified))
                   (marker-face
                    `(:inherit     org-table
                      :foreground ,table-bg))
                   (btn-marker-face
                    `(:inherit             org-kbd
                      :distant-foreground ,kbd-bg
                      :foreground         ,kbd-bg))
                   (kbd-marker
                    (dolist (el org-emphasis-alist)
                      (when (member 'org-kbd el)
                        (return (car el))))))

              (make-spacemacs//space-doc-cache
               :marker-face     marker-face
               :btn-marker-face btn-marker-face
               :kbd-marker      kbd-marker)))))

(defun spacemacs//space-doc-org-do-emphasis-faces-advice (found)
  "If FOUND has non-nil value - modify emphasized regions
appearances in the current buffer. The function uses
`match-data' set by `org-do-emphasis-faces' function."

  ;; `org-do-emphasis-faces' returns non-nil value when it
  ;; found a region to emphasize.
  (when (and found space-doc-mode)
    (spacemacs//space-doc-emphasis-region
     (match-beginning 2)
     (match-end 2)))
  found)

(defun spacemacs//space-doc-modf-advice-org-do-emphasis-faces (&optional enable)
  "If ENABLE has non-nil value - advice `org-do-emphasis-faces' function
with `spacemacs//space-doc-org-do-emphasis-faces-advice'.
NOTE: `org-do-emphasis-faces' is lazy and will emphasize only part of the
current buffer so piggybacking it should be pretty performant solution."

  (when enable
    (advice-add 'org-do-emphasis-faces
                :after
                #'spacemacs//space-doc-org-do-emphasis-faces-advice)))

(defun spacemacs//space-doc-emphasis-region (begin end)
  "Emphasis region based on its leading character.
The character should be one of the markers
from `org-emphasis-alist'."

  (let* ((beginnig-marker-overlay nil)
         (ending-marker-overlay nil))

    (setq beginnig-marker-overlay
          (make-overlay begin (1+ begin))
          ending-marker-overlay
          (make-overlay (1- end) end))

    (if (string= (buffer-substring-no-properties begin
                                                 (1+ begin))
                 (spacemacs//space-doc-cache-kbd-marker
                  spacemacs--space-doc-cache))
        (progn
          (overlay-put beginnig-marker-overlay
                       'face
                       (spacemacs//space-doc-cache-btn-marker-face
                        spacemacs--space-doc-cache))
          (overlay-put ending-marker-overlay
                       'face
                       (spacemacs//space-doc-cache-btn-marker-face
                        spacemacs--space-doc-cache)))

      ;; If inside table.
      (if (save-excursion
            (goto-char begin)
            (beginning-of-line)
            (looking-at-p org-table-any-line-regexp))
          (progn
            (overlay-put beginnig-marker-overlay
                         'face
                         (spacemacs//space-doc-cache-marker-face
                          spacemacs--space-doc-cache))
            (overlay-put ending-marker-overlay
                         'face
                         (spacemacs//space-doc-cache-marker-face
                          spacemacs--space-doc-cache)))

        (overlay-put beginnig-marker-overlay
                     'invisible t)
        (overlay-put ending-marker-overlay
                     'invisible t)))

    (overlay-put beginnig-marker-overlay
                 'space-doc-emphasis-overlay t)
    (overlay-put ending-marker-overlay
                 'space-doc-emphasis-overlay t)))

(defun spacemacs//space-doc-modf-emphasis-overlays (&optional enable)
  "If ENABLE has non-nil value - overlay regions which have
already been emphasized by `org-do-emphasis-faces'
in the current buffer. Otherwise remove all overlays
with property `space-doc-emphasis-overlay'."

  ;; Remove overlays.
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'space-doc-emphasis-overlay)
      (delete-overlay overlay)))

  (when enable
      (dolist (emphasized-region
               (spacemacs//space-doc-find-regions-by-text-property
                'org-emphasis t))
        (spacemacs//space-doc-emphasis-region
         (car  emphasized-region)
         (cadr emphasized-region)))))

(defun spacemacs//space-doc-modf-org-kbd-face-remap (&optional enable)
  "If ENABLE has non-nil value - removes boxes from the `org-kbd'
face in the current `org-mode' buffer. Otherwise - reverts them to
default."
  (if enable
      (set (make-local-variable
            'spacemacs--space-doc-org-kbd-face-remap-cookie)
           (face-remap-add-relative 'org-kbd
                                    `(:box nil)))
    (face-remap-remove-relative
     spacemacs--space-doc-org-kbd-face-remap-cookie)))

(defun spacemacs//space-doc-modf-meta-tags-overlays (&optional enable)
  "If ENABLE has non-nil value - modify `org-mode' meta tags
appearance in the current buffer. Otherwise - disable."

  (if enable
      ;; TODO add more types of tags or meta-line if needed.
      (let* ((invisible-org-meta-tags-list
              `(;; Hide TITLE tag.
                ("\\([ \t]*\\#\\+TITLE\\:\[ \t]*\\)"
                 invisible t)
                ;; Hide CAPTION logo meta line.
                ("\\(\n.*\\#\\+CAPTION\\:.*\\)"
                 invisible t)
                ;; Hide TOC-ORG tag and spaces before it.
                ;; Use modified `toc-org-toc-org-regexp' because
                ;; the original one matches whole string.
                (,(concat "\\([ \t]*:toc\\([@_][0-9]\\|\\([@_][0-9]"
                          "[@_][a-zA-Z]+\\)\\)?:\\($\\|[^ ]*:$\\)\\)")
                 invisible t)
                ;; Hide empty line before #+BEGIN_SRC tag if
                ;; background color of the `org-block-begin-line'
                ;; face is unspecified.
                ,(unless (face-background 'org-block-begin-line)
                   '("\n\\(\n\\)[ \t]*\\#\\+begin_src.*$"
                     invisible t))
                ;; Hide empty line after #+END_SRC tag if
                ;; background color of the `org-block-end-line'
                ;; face is unspecified and the next line isn't
                ;;an org headline.
                ,(unless (face-background 'org-block-end-line)
                   '("^[ \t]*\\#\\+end_src.*\n\\(\n\\)[^\\*]"
                     invisible t)))))

        ;; Remove nils.
        (setq invisible-org-meta-tags-list
              (remove nil invisible-org-meta-tags-list))

        ;; Make `org-mode' meta tags invisible.
        (dolist (tag invisible-org-meta-tags-list)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (car tag) nil t)
              (let ((new-overlay
                     (make-overlay (match-beginning 1)
                                   (match-end 1))))
                (overlay-put new-overlay  (cadr tag) (cddr tag))
                (overlay-put new-overlay 'space-doc-tag-overlay t))))))

    ;; Remove overlays.
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'space-doc-tag-overlay)
        (delete-overlay overlay)))))

(defun spacemacs//space-doc-modf-org-block-line-face-remap (&optional enable)
  "If ENABLE has non-nil value - hide text of the code block meta lines
in the current buffer. If the blocks have background color text won't be
masked because it makes them look ugly with some themes.
If ENABLE has nil value - revert to the default."

  (if enable
      (let* ((default-bg (or (face-background 'default)
                             'unspecified))
             (org-bb-bg (or (face-background 'org-block-begin-line)
                            (face-background 'org-meta-line)))
             (hide-bb-text-face `(:inherit org-block-begin-line
                                  :foreground         ,default-bg
                                  :distant-foreground ,default-bg))
             (org-bn-bg (or (face-background 'org-block-end-line)
                            (face-background 'org-meta-line)))
             (hide-bn-text-face `(:inherit org-block-end-line
                                  :foreground         ,default-bg
                                  :distant-foreground ,default-bg)))
        (unless org-bb-bg
          (set (make-local-variable
                'spacemacs--space-doc-org-block-begin-line-face-remap-cookie)
               (face-remap-add-relative 'org-block-begin-line
                                        hide-bb-text-face)))
        (unless org-bn-bg
          (set (make-local-variable
                'spacemacs--space-doc-org-block-end-line-face-remap-cookie)
               (face-remap-add-relative 'org-block-end-line
                                        hide-bn-text-face))))
    (face-remap-remove-relative
     spacemacs--space-doc-org-block-begin-line-face-remap-cookie)
    (face-remap-remove-relative
     spacemacs--space-doc-org-block-end-line-face-remap-cookie)))

(defun spacemacs//space-doc-modf-link-protocol (&optional enable)
  "If ENABLE has non-nil value - use `spacemacs//space-doc-open' to
open 'https' links in the current `org-mode' buffer. Otherwise open
them in the browser(default behavior)."
  (if enable
      (progn
        ;; Make `space-doc' https link opener buffer local
        ;; and enable it only when `space-doc' mode is enabled.
        (make-local-variable 'org-link-types)
        (make-local-variable 'org-link-protocols)
        (org-add-link-type "https" 'spacemacs//space-doc-open))

    (kill-local-variable 'org-link-types)
    (kill-local-variable 'org-link-protocols))
  ;; Trigger `org-mode' internal updates.
  ;; NOTE: might be unnecessary.
  (org-add-link-type nil))

(defun spacemacs//space-doc-open (path)
  "If PATH argument is a link to an .org file that is located
in the Spacemacs GitHub repository - Visit the local copy
of the file with `spacemacs/view-org-file'.
Open all other links with `browse-url'."
  (let ((git-url-root-regexp
         (concat "\\/\\/github\\.com\\/syl20bnr\\/spacemacs\\/blob"
                 "\\/[^/]+\\/\\(.*\\.org\\)\\(\\#.*\\)?")))
    (if (string-match git-url-root-regexp path)
        (spacemacs/view-org-file (concat user-emacs-directory
                                         (match-string 1 path))
                                 (or (match-string 2 path)
                                     "^")
                                 'subtree)
      (browse-url (concat "https://" path)))))

(defun spacemacs//space-doc-run-modfs-deferred (modfs &optional flag)
  "Run each modf function from the MODFS list in the `run-with-idle-timer'
callback. This way heavy modfs won't affect document opening time.
FLAG is passed through."
  (run-with-idle-timer 0 nil (lambda (modfs flag)
                               (dolist (modf modfs)
                                 (funcall modf flag)))
                       modfs flag))

(defun spacemacs//space-doc-find-regions-by-text-property
    (property value &optional start end)
  "Return a list of pairs (region-beginning region-end) in
the current buffer. If START or END has non-nil value - use them as
boundaries.
NOTE: It can find only fontified regions."

  (let ((p-min (or start (point-min)))
        (p-max (or end (point-max)))
        (r-end nil)
        (ret (list)))
    (while (not (= p-min p-max))
      (setq p-min (or (text-property-any p-min p-max property value)
                      (point-max))
            r-end (or (text-property-not-all p-min p-max property value)
                      (point-max))
            ret (append ret (unless (= p-min r-end)
                              (list(list p-min r-end))))
            p-min r-end))
    ret))

(provide 'space-doc)
;;; space-doc.el ends here.
