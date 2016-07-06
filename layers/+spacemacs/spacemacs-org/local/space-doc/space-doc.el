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
(require 'centered-buffer-mode)

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
      (dolist (modificator (append '(spacemacs//space-doc-set-cache
                                     spacemacs//space-doc-runs-deferred)
                                   spacemacs-space-doc-modificators))
        (funcall modificator space-doc-mode))
    (message (format "space-doc-mode error:%s isn't an org-mode buffer"
                     (buffer-name)))
    (setq space-doc-mode nil)))

;; NOTE: Dont forget to update Spacemacs FAQ if you modify this list!
(defvar spacemacs-space-doc-modificators
  '(spacemacs//space-doc-center-buffer-mode
    spacemacs//space-doc-org-indent-mode
    spacemacs//space-doc-view-mode
    spacemacs//space-doc-hide-line-numbers
    spacemacs//space-doc-emphasis-overlays
    spacemacs//space-doc-meta-tags-overlays
    spacemacs//space-doc-link-protocol
    spacemacs//space-doc-org-block-line-face-remap
    spacemacs//space-doc-org-kbd-face-remap
    spacemacs//space-doc-resize-inline-images
    spacemacs//space-doc-advice-org-do-emphasis-faces)
  "List of `space-doc' modificator functions for `org-mode' buffers.
The functions work with a current buffer and accept ENABLE(flag) argument.
If the argument has non-nil value - enable the modifications introduced
by the function. Otherwise - disable.")

;; NOTE: Dont forget to update Spacemacs FAQ if you modify this list!
(defvar spacemacs-space-doc-modificators-deferred
  '()
  "Same as `spacemacs-space-doc-modificators' but the modificators will
be run deferred.")

(defun spacemacs//space-doc-center-buffer-mode (&optional flag)
  "Enable `spacemacs-centered-buffer-mode' if flag is non nil, disable it otherwise.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
  ;;FIXME: Need to redesign this.. One day.
  (if flag
      (progn
        ;; HACK: Hide the original buffer from `spacemacs/previous-useful-buffer'.
        (unless (and (string-prefix-p "*" (buffer-name))
                     (string-suffix-p "*" (buffer-name)))
          (rename-buffer (format "*%s*" (buffer-name))))
        (set (make-local-variable 'spacemacs--space-doc-origin-fringe-color)
             (face-background 'fringe))
        ;; HACK: Fix glitchy fringe color.
        (face-remap-add-relative 'fringe :background
                                 spacemacs-centered-buffer-mode-fringe-color)
        ;; HACK: Needed to get proper content width.
        (run-with-idle-timer 0 nil 'spacemacs-centered-buffer-mode +1))
    (when spacemacs-centered-buffer-mode
      (set-window-buffer
       (selected-window)
       spacemacs--centered-buffer-mode-origin-buffer)
      (rename-buffer (substring (buffer-name) 1 (1- (length (buffer-name)))))
      (kill-buffer spacemacs--centered-buffer-mode-indirect-buffer)
      ;; HACK: Now we call it for the original buffer.
      (space-doc-mode -1))
    (when (bound-and-true-p spacemacs--space-doc-origin-fringe-color)
      ;; HACK: Removing or reseting doesn't work.
      (face-remap-add-relative 'fringe :background
                               spacemacs--space-doc-origin-fringe-color))))

(defun spacemacs//space-doc-org-indent-mode (&optional flag)
  "Enable `org-indent-mode' if flag is non nil, disable it otherwise.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
  (org-indent-mode (if flag 1 -1)))

(defun spacemacs//space-doc-view-mode (&optional flag)
  "Enable `view-mode' if flag is non nil, disable it otherwise.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
  (view-mode (if flag 1 -1)))

(cl-defstruct spacemacs--space-doc-cache-struct
  marker-face
  btn-marker-face
  kbd-marker)

(defvar-local spacemacs--space-doc-cache nil
  "Global variable of struct `spacemacs-space-doc-cache-struct'.
It is set by `spacemacs//space-doc-set-cache'.")

(defun spacemacs//space-doc-set-cache (&optional flag)
  "Set `spacemacs--space-doc-cache'.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
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
              (make-spacemacs--space-doc-cache-struct
               :marker-face     marker-face
               :btn-marker-face btn-marker-face
               :kbd-marker      kbd-marker)))))

(defun spacemacs//space-doc-hide-line-numbers (&optional enable)
  "If ENABLE is non-nil then toggle off the line numbers.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
  (if enable
      (spacemacs/toggle-line-numbers-off)
    (when dotspacemacs-line-numbers
      (spacemacs/toggle-line-numbers-on))))

(defun spacemacs//space-doc-org-do-emphasis-faces-advice (found)
  "If FOUND has non-nil value then modify emphasized regions
appearances in the current buffer. The function uses
`match-data' set by `org-do-emphasis-faces' function."
  ;; `org-do-emphasis-faces' returns non-nil value when it
  ;; found a region to emphasize.
  (when (and found
             space-doc-mode
             (not (and
                   (match-string 4)
                   (string-empty-p
                    (replace-regexp-in-string "\\*+"
                                              ""
                                              (match-string 4))))))
    (spacemacs//space-doc-emphasis-region
     (match-beginning 2)
     (match-end 2)))
  found)

(defun spacemacs//space-doc-advice-org-do-emphasis-faces (&optional enable)
  "Advise org-do-emphasis-faces.
If ENABLE is non-nil, add advice `org-do-emphasis-faces' function with
`spacemacs//space-doc-org-do-emphasis-faces-advice'.
NOTE: `org-do-emphasis-faces' is lazy and will emphasize only part of the
current buffer so piggybacking it should be pretty performant solution.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
  (when enable
    (advice-add 'org-do-emphasis-faces
                :after
                #'spacemacs//space-doc-org-do-emphasis-faces-advice)))

(defun spacemacs//space-doc-emphasis-region (begin end)
  "Emphasis region based on its leading character.
The character should be one of the markers from `org-emphasis-alist'."
  (let* ((beginning-marker-overlay nil)
         (ending-marker-overlay nil))
    (setq beginning-marker-overlay
          (make-overlay begin (1+ begin))
          ending-marker-overlay
          (make-overlay (1- end) end))
    (if (string= (buffer-substring-no-properties begin
                                                 (1+ begin))
                 (spacemacs--space-doc-cache-struct-kbd-marker
                  spacemacs--space-doc-cache))
        (progn
          (overlay-put beginning-marker-overlay
                       'face
                       (spacemacs--space-doc-cache-struct-btn-marker-face
                        spacemacs--space-doc-cache))
          (overlay-put ending-marker-overlay
                       'face
                       (spacemacs--space-doc-cache-struct-btn-marker-face
                        spacemacs--space-doc-cache)))
      ;; If inside table.
      (if (save-excursion
            (goto-char begin)
            (beginning-of-line)
            (looking-at-p org-table-any-line-regexp))
          (progn
            (overlay-put beginning-marker-overlay
                         'face
                         (spacemacs--space-doc-cache-struct-marker-face
                          spacemacs--space-doc-cache))
            (overlay-put ending-marker-overlay
                         'face
                         (spacemacs--space-doc-cache-struct-marker-face
                          spacemacs--space-doc-cache)))
        (overlay-put beginning-marker-overlay
                     'invisible t)
        (overlay-put ending-marker-overlay
                     'invisible t)))
    (overlay-put beginning-marker-overlay
                 'space-doc-emphasis-overlay t)
    (overlay-put ending-marker-overlay
                 'space-doc-emphasis-overlay t)))

(defun spacemacs//space-doc-emphasis-overlays (&optional enable)
  "Emphasis overlays.
If ENABLE is non-nil, overlay regions which have already been emphasized by
`org-do-emphasis-faces'in the current buffer.
Otherwise remove all overlays with property `space-doc-emphasis-overlay'.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
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

(defun spacemacs//space-doc-org-kbd-face-remap (&optional enable)
  "Remove boxes from key bindings.
If ENABLE is non-nil, removes boxes from the `org-kbd'face in the current
`org-mode' buffer.
Otherwise, reverts them to default.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
  (if enable
      (set (make-local-variable
            'spacemacs--space-doc-org-kbd-face-remap-cookie)
           (face-remap-add-relative 'org-kbd
                                    `(:box nil)))
    (when (bound-and-true-p spacemacs--space-doc-org-kbd-face-remap-cookie)
      (face-remap-remove-relative
       spacemacs--space-doc-org-kbd-face-remap-cookie))))

(defun spacemacs//space-doc-resize-inline-images (&optional enable)
  "Resize inline images.
If ENABLE is non nil then resize inline images.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
  ;; resizing is always performed even when the image is smaller
  ;; so we don't resize in README.org buffers for now
  (let ((org-image-actual-width
         (and enable
              (not (string-match-p ".*README.org\\'" (buffer-file-name)))
              600)))
    (org-display-inline-images)))

(defun spacemacs//space-doc-meta-tags-overlays (&optional enable)
  "Modify meta tag appearance.
If ENABLE is non-nil, modify `org-mode' meta tags appearance in the current
buffer.
Otherwise, disable modifcations.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
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

(defun spacemacs//space-doc-org-block-line-face-remap (&optional enable)
  "Hide drawers.
If ENABLE is non-nil, hide text of the code block meta lines in the current
buffer. If the blocks have background color text won't be masked because it
makes them look ugly with some themes.
If ENABLE has nil, revert to the default.
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
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
    (when (bound-and-true-p
           spacemacs--space-doc-org-block-begin-line-face-remap-cookie)
      (face-remap-remove-relative
       spacemacs--space-doc-org-block-begin-line-face-remap-cookie))
    (when (bound-and-true-p
           spacemacs--space-doc-org-block-end-line-face-remap-cookie)
      (face-remap-remove-relative
       spacemacs--space-doc-org-block-end-line-face-remap-cookie))))

(defun spacemacs//space-doc-link-protocol (&optional enable)
  "Open HTTPS links in the curren buffer.
If ENABLE is non-nil, use `spacemacs//space-doc-open' to open HTTPS links
in the current `org-mode' buffer.
Otherwise open them in the browser(default behavior).
This functions is aimed to be used with `spacemacs-space-doc-modificators'."
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
  "Open PATH link.
If PATH argument is a link to an .org file that is located in the Spacemacs
GitHub repository then visit the local copy of the file with
`spacemacs/view-org-file'.
Open all other links with `browse-url'."
  (let ((git-url-root-regexp
         (concat "\\/\\/github\\.com\\/syl20bnr\\/spacemacs\\/blob"
                 "\\/[^/]+\\/\\(.*\\.org\\)\\(\\#.*\\)?")))
    (if (string-match git-url-root-regexp path)
        (spacemacs/view-org-file (concat spacemacs-start-directory
                                         (match-string 1 path))
                                 (or (match-string 2 path)
                                     "^")
                                 'subtree)
      (browse-url (concat "https://" path)))))

(defun spacemacs//space-doc-runs-deferred (&optional flag)
  "Run each modificator function from the
`spacemacs-space-doc-modificators-deferred' list
in the next command loop. This way heavy modificator functions
won't affect document opening time. FLAG is passed through."
  (run-with-idle-timer 0 nil (lambda (flag)
                               (dolist (modf spacemacs-space-doc-modificators-deferred)
                                 (funcall modf flag)))
                       flag))

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
