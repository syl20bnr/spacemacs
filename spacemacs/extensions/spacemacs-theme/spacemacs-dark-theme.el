;;; spacemacs-theme.el --- Emacs 24 theme with a dark background.

;; Copyright (C) 2014 , Nasser Alshammari

;; Author: Nasser Alshammari
;; https://github.com/nashamri/spacemacs-theme
;;
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Initially created with the help of emacs-theme-generator, https://github.com/mswift42/theme-creator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.


(deftheme spacemacs-dark)
(let ((class '((class color) (min-colors 89)))
      (base "#b2b2b2");;                     GUI       TER
      (bg1        (if (display-graphic-p) "#292b2e" "#262626"))
      (bg2        (if (display-graphic-p) "#212026" "#1c1c1c"))
      (bg3        (if (display-graphic-p) "#100a14" "#121212"))
      (bg4        (if (display-graphic-p) "#0a0814" "#080808"))
      (key1       (if (display-graphic-p) "#4f97d7" "#4f97d7"))
      (key2       (if (display-graphic-p) "#277bb8" "#277bb8"))
      (builtin    (if (display-graphic-p) "#1f71ab" "#268bd2"))
      (keyword    (if (display-graphic-p) "#237fbf" "#268bd2"))
      (const      (if (display-graphic-p) "#a45bad" "#d75fd7"))
      (comment    (if (display-graphic-p) "#2aa198" "#2aa198"))
      (comment-bg (if (display-graphic-p) "#282a32" "#282a32"))
      (func       (if (display-graphic-p) "#bc6ec5" "#d75fd7"))
      (str        (if (display-graphic-p) "#2aa198" "#2aa198"))
      (type       (if (display-graphic-p) "#c56ec3" "#d75fd7"))
      (var        (if (display-graphic-p) "#adaab3" "#adaab3"))
      (err        (if (display-graphic-p) "#e0211d" "#e0211d"))
      (war        (if (display-graphic-p) "#dc752f" "#dc752f"))
      (inf        (if (display-graphic-p) "#2f96dc" "#2f96dc"))
      (suc        (if (display-graphic-p) "#86dc2f" "#86dc2f"))
      (green      (if (display-graphic-p) "#67b11d" "#67b11d"))
      (yellow     (if (display-graphic-p) "#b1951d" "#875f00"))
      (cyan       (if (display-graphic-p) "#28def0" "#00ffff"))
      (violet     (if (display-graphic-p) "#a31db1" "#af00df"))
      (red        (if (display-graphic-p) "#f2241f" "#d70000"))
      (active1    (if (display-graphic-p) "#222226" "#121212"))
      (active2    (if (display-graphic-p) "#304060" "#444444"))
      (inactive   (if (display-graphic-p) "#304060" "#111111"))
      (highlight  (if (display-graphic-p) "#433f4d" "#444444")))

  (custom-theme-set-faces
   'spacemacs-dark

;;;;; basics
   `(default ((,class (:background ,bg1 :foreground ,base))))
   `(vertical-border ((,class (:foreground ,bg4))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
   `(match ((,class (:background ,bg1 :foreground ,inf :weight bold))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,comment :underline t))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func :bold t))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type :bold t))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-preprocessor-face ((,class (:foreground ,func))))
   `(font-lock-warning-face ((,class (:foreground ,war :background ,bg2))))
   `(region ((,class (:background ,highlight))))
   `(italic ((,class (:underline nil))))
   `(highlight ((,class (:foreground ,base :background ,bg3))))
   `(hl-line ((,class (:background ,bg2))))
   `(fringe ((,class (:background ,bg1 :foreground ,base))))
   `(cursor ((,class (:background ,bg3))))
   `(show-paren-match-face ((,class (:background ,suc))))
   `(secondary-selection ((,class (:background ,bg3))))
   `(isearch ((,class (:bold t :foreground ,bg1 :background ,inf))))
   `(success ((,class (:foreground ,suc))))
   `(warning ((,class (:foreground ,war ))))
   `(error ((,class (:foreground ,err))))
   `(lazy-highlight ((,class (:foreground ,bg1 :background ,inf :weight normal))))
   `(page-break-lines ((,class (:foreground ,active2))))
   `(mode-line
     ((,class (:foreground ,base
                           :background ,active1))))
   `(mode-line-inactive
     ((,class (:foreground ,base
                           :background ,bg1
                           :box (:color ,inactive :line-width 1)))))
   `(mode-line-buffer-id ((,class (:bold t :foreground ,func))))

;;;;; powerline
   `(powerline-active1 ((,class (:background ,active2 :foreground ,base))))
   `(powerline-active2 ((,class (:background ,active2 :foreground ,base))))
   `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,base))))
   `(powerline-inactive2 ((,class (:background ,bg2 :foreground ,base))))

;;;;; guide-key
   `(guide-key/highlight-command-face ((,class (:foreground ,base))))
   `(guide-key/key-face ((,class (:foreground ,key1))))
   `(guide-key/prefix-command-face ((,class (:foreground ,key2 :weight bold))))

;;;;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,err)))
      (,class (:foreground ,base :background ,err :weight bold :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,war)))
      (,class (:foreground ,base :background ,war :weight bold :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,inf)))
      (,class (:foreground ,base :background ,inf :weight bold :underline t))))

   `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
   `(flycheck-fringe-error ((,class (:foreground ,err :weight bold))))
   `(flycheck-fringe-warning ((,class (:foreground ,war :weight bold))))
   `(flycheck-fringe-info ((,class (:foreground ,inf :weight bold))))

;;;;; anzu-mode
   `(anzu-mode-line ((,class (:foreground ,yellow :weight bold))))

;;;;; smartparens
   `(sp-show-pair-match-face
     ((,class (:foreground ,red :weight bold))))

;;;;; git-gutter-fr
   `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,war :weight bold))))
   `(git-gutter-fr:modified ((,class (:foreground ,inf :weight bold))))

;;;;; helm
   `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
   `(helm-bookmark-file ((,class (:foreground ,base))))
   `(helm-bookmark-gnus ((,class (:foreground ,type))))
   `(helm-bookmark-info ((,class (:foreground ,type))))
   `(helm-bookmark-man ((,class (:foreground ,type))))
   `(helm-bookmark-w3m ((,class (:foreground ,type))))
   `(helm-buffer-directory ((,class (:foreground ,base :background ,bg1))))
   `(helm-buffer-file ((,class (:foreground ,base :background ,bg1))))
   `(helm-buffer-not-saved ((,class (:foreground ,type :background ,bg1))))
   `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
   `(helm-buffer-saved-out ((,class (:foreground ,base :background ,bg1))))
   `(helm-buffer-size ((,class (:foreground ,base :background ,bg1))))
   `(helm-candidate-number ((,class (:background ,bg1 :foreground ,inf :bold t))))
   `(helm-header ((,class (:foreground ,base :background ,bg1 :underline nil :box nil))))
   `(helm-ff-directory ((,class (:foreground ,key1 :background ,bg1 :weight bold))))
   `(helm-ff-executable ((,class (:foreground ,suc :background ,bg1 :weight normal))))
   `(helm-ff-file ((,class (:foreground ,base :background ,bg1 :weight normal))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,red :background ,bg1 :weight bold))))
   `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
   `(helm-ff-symlink ((,class (:foreground ,cyan :background ,bg1 :weight bold))))
   `(helm-grep-cmd-line ((,class (:foreground ,base :background ,bg1))))
   `(helm-grep-file ((,class (:foreground ,base :background ,bg1))))
   `(helm-grep-finish ((,class (:foreground ,base :background ,bg1))))
   `(helm-grep-lineno ((,class (:foreground ,base :background ,bg1))))
   `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((,class (:foreground ,func :background ,bg1))))
   `(helm-source-header ((,class (:background ,type :foreground ,bg1 :bold t))))
   `(helm-selection ((,class (:background ,highlight))))
   `(helm-selection-line ((,class (:background ,bg2))))
   `(helm-separator ((,class (:foreground ,type :background ,bg1))))
   `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
   `(helm-time-zone-home ((,class (:foreground ,type :background ,bg1))))
   `(helm-match ((,class (:inherit match))))
   `(helm-match-item ((,class (:inherit match))))
   `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg1))))
   `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))

;;;;; helm-swoop
   `(helm-swoop-target-line-block-face ((,class (:foreground ,base :background ,highlight))))
   `(helm-swoop-target-line-face ((,class (:foreground ,base :background ,highlight))))
   `(helm-swoop-target-word-face ((,class (:foreground ,bg1 :background ,suc))))

;;;;; company
   `(company-echo-common ((,class (:background ,base :foreground ,bg1))))
   `(company-preview ((,class (:background ,bg1 :foreground ,key1))))
   `(company-preview-common ((,class (:background ,bg2 :foreground ,keyword))))
   `(company-preview-search ((,class (:background ,bg2 :foreground ,green))))
   `(company-scrollbar-bg ((,class (:background ,bg3))))
   `(company-scrollbar-fg ((,class (:background ,type))))
   `(company-tooltip ((,class (:foreground ,base :background ,bg2 :bold t))))
   `(company-tooltip-annotation ((,class (:background ,bg2 :foreground ,inf))))
   `(company-tooltip-common ((,class ( :foreground ,base))))
   `(company-tooltip-common-selection ((,class (:foreground ,keyword))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-search ((,class (:inherit match))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,base))))
   `(company-template-field ((,class (:inherit region))))

;;;;; rainbow-delimiters
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,war)))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,inf)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,func)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,str)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,green)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,yellow)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,inf)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,func)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,str)))

;;;;; dired
   `(dired-directory ((,class (:foreground ,key1 :background ,bg1 :weight bold))))
   `(dired-flagged ((,class (:foreground ,red))))
   `(dired-header ((,class (:foreground ,type :weight bold))))
   `(dired-ignored ((,class (:inherit shadow))))
   `(dired-mark ((,class (:foreground ,type :weight bold))))
   `(dired-marked ((,class (:foreground ,violet :weight bold))))
   `(dired-perm-write ((,class (:foreground ,base :underline t))))
   `(dired-symlink ((,class (:foreground ,cyan :background ,bg1 :weight bold))))
   `(dired-warning ((,class (:foreground ,war))))

;;;;; eshell
   `(eshell-prompt ((,class (:foreground ,keyword :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,red :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((,class (:foreground ,inf :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,suc :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,base))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((,class (:foreground ,yellow :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,cyan :weight bold))))

;;;;; neotree
   `(neo-root-dir-face ((,class (:foreground ,func :weight bold))))
   `(neo-dir-link-face ((,class (:foreground ,inf :weight bold))))
   `(neo-file-link-face ((,class (:foreground ,base))))
   `(neo-expand-btn-face ((,class (:foreground ,base))))

;;;;; linum-mode
   `(linum ((,class (:foreground ,str :background ,bg2))))

;;;;; magit
   `(magit-blame-sha1 ((,class :background ,bg3 :foreground ,func)))
   `(magit-blame-time ((,class :background ,bg3 :foreground ,key1)))
   `(magit-blame-header ((,class :background ,bg3 :foreground ,key1)))
   `(magit-blame-subject ((,class :background ,bg3 :foreground ,base)))
   `(magit-blame-culprit ((,class :background ,bg3 :foreground ,str)))
   `(magit-branch ((,class (:foreground ,const :weight bold))))
   `(magit-diff-hunk-header ((,class (:background nil :foreground ,builtin))))
   `(magit-diff-file-header ((,class (:background nil :foreground ,str))))
   `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,base))))
   `(magit-diffstat-added   ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-hash ((,class (:foreground ,base))))
   `(magit-hunk-heading           ((,class (:background ,bg3))))
   `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
   `(magit-log-author ((,class (:foreground ,base))))
   `(magit-log-head-label-remote ((,class (:background ,suc :foreground ,bg1 :bold t))))
   `(magit-log-head-label-local ((,class (:background ,inf :foreground ,bg1 :bold t))))
   `(magit-log-head-label-tags ((,class (:background ,violet :foreground ,bg1 :bold t))))
   `(magit-log-head-label-wip ((,class (:background ,cyan :foreground ,bg1 :bold t))))
   `(magit-log-head-label-head ((,class (:background ,yellow :foreground ,bg1 :bold t))))
   `(magit-log-sha1 ((,class (:foreground ,str))))
   `(magit-item-highlight ((,class :background ,bg2)))
   `(magit-process-ok ((,class (:foreground ,func :weight bold))))
   `(magit-process-ng ((,class (:foreground ,war :weight bold))))
   `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
   `(magit-section-highlight      ((,class (:background ,bg2))))
   `(magit-section-title ((,class (:background ,bg1 :foreground ,builtin :weight bold))))

;;;;; org
   `(org-agenda-structure ((,class (:weight bold :foreground ,type))))
   `(org-agenda-clocking ((,class (:foreground ,type))))
   `(org-agenda-date ((,class (:foreground ,var :height 1.1))))
   `(org-agenda-date-weekend ((,class (:weight normal :foreground ,base))))
   `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.3))))
   `(org-agenda-done ((,class (:foreground ,suc :bold t))))
   `(org-block ((,class (:foreground ,base))))
   `(org-code ((,class (:foreground ,cyan))))
   `(org-column ((,class (:background ,highlight))))
   `(org-column-title ((,class (:background ,highlight))))
   `(org-clock-overlay ((,class (:foreground ,type))))
   `(org-date ((,class (:underline t :foreground ,var) )))
   `(org-date-selected ((,class (:background ,func :foreground ,bg1) )))
   `(org-document-info-keyword ((,class (:foreground ,func))))
   `(org-document-title ((,class (:foreground ,key1 :weight bold :height 1.4))))
   `(org-done ((,class (:foreground ,suc :bold t :underline t))))
   `(org-ellipsis ((,class (:foreground ,builtin))))
   `(org-footnote  ((,class (:underline t :foreground ,base))))
   `(org-hide ((,class (:foreground ,base))))
   `(org-level-1 ((,class (:bold t :foreground ,inf :height 1.3))))
   `(org-level-2 ((,class (:bold t :foreground ,str :height 1.2))))
   `(org-level-3 ((,class (:bold nil :foreground ,green :height 1.1))))
   `(org-level-4 ((,class (:bold nil :foreground ,yellow :height 1.0))))
   `(org-level-5 ((,class (:bold nil :foreground ,inf :height 1.0))))
   `(org-level-6 ((,class (:bold nil :foreground ,str :height 1.0))))
   `(org-level-7 ((,class (:bold nil :foreground ,green :height 1.0))))
   `(org-level-8 ((,class (:bold nil :foreground ,yellow :height 1.0))))
   `(org-link ((,class (:underline t :foreground ,comment))))
   `(org-mode-line-clock-overrun ((,class (:foreground ,err))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-priority ((,class (:foreground ,war :bold t))))
   `(org-table ((,class (:foreground ,base))))
   `(org-todo ((,class (:foreground ,war :bold t :underline t))))
   `(org-scheduled ((,class (:foreground ,type))))
   `(org-scheduled-today ((,class (:foreground ,func :weight bold :height 1.2))))
   `(org-sexp-date ((,class (:foreground ,base))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-warning ((,class (:foreground ,err))))
   `(org-verbatim ((,class (:foreground ,base))))
   `(org-verse ((,class (:inherit org-block :slant italic))))

;;;;; other, need more work
   `(custom-button ((,class (:background ,yellow))))
   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,key2 :italic t))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
   `(ido-only-match ((,class (:foreground ,war :underline nil))))
   `(ido-first-match ((,class (:foreground ,keyword :bold t :underline nil))))
   `(ido-vertical-match-face ((,class (:foreground ,type :underline nil))))
   `(flx-highlight-face ((,class (:foreground ,type :underline nil))))
   `(gnus-header-content ((,class (:foreground ,keyword))))
   `(gnus-header-from ((,class (:foreground ,var))))
   `(gnus-header-name ((,class (:foreground ,type))))
   `(gnus-header-subject ((,class (:foreground ,func :bold t))))
   `(mu4e-view-url-number-face ((,class (:foreground ,type))))
   `(mu4e-cited-1-face ((,class (:foreground ,base))))
   `(mu4e-cited-7-face ((,class (:foreground ,base))))
   `(mu4e-header-marks-face ((,class (:foreground ,type))))
   `(ffap ((,class (:foreground ,base))))
   `(js2-private-function-call ((,class (:foreground ,const))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,key1))))
   `(js2-external-variable ((,class (:foreground ,type  ))))
   `(js2-function-param ((,class (:foreground ,const))))
   `(js2-jsdoc-value ((,class (:foreground ,str))))
   `(js2-private-member ((,class (:foreground ,base))))
   `(js3-warning-face ((,class (:underline ,keyword))))
   `(js3-error-face ((,class (:underline ,war))))
   `(js3-external-variable-face ((,class (:foreground ,var))))
   `(js3-function-param-face ((,class (:foreground ,key2))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
   `(js3-instance-member-face ((,class (:foreground ,const))))
   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))
   `(icompletep-determined ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,base)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
   `(trailing-whitespace ((,class :foreground nil :background ,err)))
   `(term ((,class (:foreground ,base :background ,bg1))))
   `(term-color-black ((,class (:foreground ,bg3))))
   `(term-color-blue ((,class (:foreground ,inf))))
   `(term-color-red ((,class (:foreground ,red))))
   `(term-color-green ((,class (:foreground ,green))))
   `(term-color-yellow ((,class (:foreground ,yellow))))
   `(term-color-magenta ((,class (:foreground ,builtin))))
   `(term-color-cyan ((,class (:foreground ,cyan))))
   `(term-color-white ((,class (:foreground ,base))))
   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-keyword-face ((,class (:foreground ,keyword))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-string-face ((,class (:foreground ,str))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
   `(web-mode-html-tag-face ((,class (:foreground ,builtin))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'spacemacs-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; spacemacs-dark-theme.el ends here
