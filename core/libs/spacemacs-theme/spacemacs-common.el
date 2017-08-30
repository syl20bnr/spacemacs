;;; spacemacs-common.el --- Color theme with a dark and light versions.

;; Copyright (C) 2015-2016 Nasser Alshammari

;; Author: Nasser Alshammari
;; URL: <https://github.com/nashamri/spacemacs-theme>
;;
;; Version: 0.1
;; Keywords: color, theme
;; Package-Requires: ((emacs "24"))

;; Initially created with the help of emacs-theme-generator, <https://github.com/mswift42/theme-creator>.

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

;;; Commentary:

;; This is a color theme for spacemacs <https://github.com/syl20bnr/spacemacs>.
;; It comes with two versions, dark and light and should work well in
;; a 256 color terminal.

;;; Code:

(defmacro dyn-let (varlist fn setfaces setvars)
  (list 'let (append varlist (funcall fn)) setfaces setvars))

(defgroup spacemacs-theme nil
  "Spacemacs-theme options."
  :group 'faces)

(defcustom spacemacs-theme-comment-bg t
  "Use a background for comment lines."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-comment-italic nil
  "Enable italics for comments and also disable background."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-org-agenda-height t
  "Use varying text heights for org agenda."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'spacemacs-theme)

(defcustom spacemacs-theme-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'spacemacs-theme)

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun custom-colors-override ()
  (mapcar (lambda (x) (list (car x) (cdr x)))
          spacemacs-theme-custom-colors))

(defun create-spacemacs-theme (variant theme-name)
  (dyn-let ((class '((class color) (min-colors 89))) ;;              ~~ Dark ~~                              ~~ Light ~~
        ;;                                                          GUI       TER                           GUI       TER
        ;; generic
        (act1          (if (eq variant 'dark) (if (true-color-p) "#222226" "#121212") (if (true-color-p) "#e7e5eb" "#d7dfff")))
        (act2          (if (eq variant 'dark) (if (true-color-p) "#5d4d7a" "#444444") (if (true-color-p) "#d3d3e7" "#afafd7")))
        (base          (if (eq variant 'dark) (if (true-color-p) "#b2b2b2" "#b2b2b2") (if (true-color-p) "#655370" "#5f5f87")))
        (base-dim      (if (eq variant 'dark) (if (true-color-p) "#686868" "#585858") (if (true-color-p) "#a094a2" "#afafd7")))
        (bg1           (if (eq variant 'dark) (if (true-color-p) "#292b2e" "#262626") (if (true-color-p) "#fbf8ef" "#ffffff")))
        (bg2           (if (eq variant 'dark) (if (true-color-p) "#212026" "#1c1c1c") (if (true-color-p) "#efeae9" "#e4e4e4")))
        (bg3           (if (eq variant 'dark) (if (true-color-p) "#100a14" "#121212") (if (true-color-p) "#e3dedd" "#d0d0d0")))
        (bg4           (if (eq variant 'dark) (if (true-color-p) "#0a0814" "#080808") (if (true-color-p) "#d2ceda" "#bcbcbc")))
        (border        (if (eq variant 'dark) (if (true-color-p) "#5d4d7a" "#111111") (if (true-color-p) "#b3b9be" "#b3b9be")))
        (cblk          (if (eq variant 'dark) (if (true-color-p) "#cbc1d5" "#b2b2b2") (if (true-color-p) "#655370" "#5f5f87")))
        (cblk-bg       (if (eq variant 'dark) (if (true-color-p) "#2f2b33" "#262626") (if (true-color-p) "#e8e3f0" "#ffffff")))
        (cblk-ln       (if (eq variant 'dark) (if (true-color-p) "#827591" "#af5faf") (if (true-color-p) "#9380b2" "#af5fdf")))
        (cblk-ln-bg    (if (eq variant 'dark) (if (true-color-p) "#373040" "#333333") (if (true-color-p) "#ddd8eb" "#dfdfff")))
        (cursor        (if (eq variant 'dark) (if (true-color-p) "#e3dedd" "#d0d0d0") (if (true-color-p) "#100a14" "#121212")))
        (const         (if (eq variant 'dark) (if (true-color-p) "#a45bad" "#d75fd7") (if (true-color-p) "#4e3163" "#8700af")))
        (comment       (if (eq variant 'dark) (if (true-color-p) "#2aa1ae" "#008787") (if (true-color-p) "#2aa1ae" "#008787")))
        (comment-light (if (eq variant 'dark) (if (true-color-p) "#2aa1ae" "#008787") (if (true-color-p) "#a49da5" "#008787")))
        (comment-bg    (if (eq variant 'dark) (if (true-color-p) "#292e34" "#262626") (if (true-color-p) "#ecf3ec" "#ffffff")))
        (comp          (if (eq variant 'dark) (if (true-color-p) "#c56ec3" "#d75fd7") (if (true-color-p) "#6c4173" "#8700af")))
        (err           (if (eq variant 'dark) (if (true-color-p) "#e0211d" "#e0211d") (if (true-color-p) "#e0211d" "#e0211d")))
        (func          (if (eq variant 'dark) (if (true-color-p) "#bc6ec5" "#d75fd7") (if (true-color-p) "#6c3163" "#8700af")))
        (head1         (if (eq variant 'dark) (if (true-color-p) "#4f97d7" "#268bd2") (if (true-color-p) "#3a81c3" "#268bd2")))
        (head1-bg      (if (eq variant 'dark) (if (true-color-p) "#293239" "#262626") (if (true-color-p) "#edf1ed" "#ffffff")))
        (head2         (if (eq variant 'dark) (if (true-color-p) "#2d9574" "#2aa198") (if (true-color-p) "#2d9574" "#2aa198")))
        (head2-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
        (head3         (if (eq variant 'dark) (if (true-color-p) "#67b11d" "#67b11d") (if (true-color-p) "#67b11d" "#5faf00")))
        (head3-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
        (head4         (if (eq variant 'dark) (if (true-color-p) "#b1951d" "#875f00") (if (true-color-p) "#b1951d" "#875f00")))
        (head4-bg      (if (eq variant 'dark) (if (true-color-p) "#32322c" "#262626") (if (true-color-p) "#f6f1e1" "#ffffff")))
        (highlight     (if (eq variant 'dark) (if (true-color-p) "#444155" "#444444") (if (true-color-p) "#d3d3e7" "#d7d7ff")))
        (highlight-dim (if (eq variant 'dark) (if (true-color-p) "#3b314d" "#444444") (if (true-color-p) "#e7e7fc" "#d7d7ff")))
        (keyword       (if (eq variant 'dark) (if (true-color-p) "#4f97d7" "#268bd2") (if (true-color-p) "#3a81c3" "#268bd2")))
        (lnum          (if (eq variant 'dark) (if (true-color-p) "#44505c" "#444444") (if (true-color-p) "#a8a8bf" "#af87af")))
        (mat           (if (eq variant 'dark) (if (true-color-p) "#86dc2f" "#86dc2f") (if (true-color-p) "#ba2f59" "#af005f")))
        (meta          (if (eq variant 'dark) (if (true-color-p) "#9f8766" "#af875f") (if (true-color-p) "#da8b55" "#df5f5f")))
        (str           (if (eq variant 'dark) (if (true-color-p) "#2d9574" "#2aa198") (if (true-color-p) "#2d9574" "#2aa198")))
        (suc           (if (eq variant 'dark) (if (true-color-p) "#86dc2f" "#86dc2f") (if (true-color-p) "#42ae2c" "#00af00")))
        (ttip          (if (eq variant 'dark) (if (true-color-p) "#9a9aba" "#888888") (if (true-color-p) "#8c799f" "#5f5f87")))
        (ttip-sl       (if (eq variant 'dark) (if (true-color-p) "#5e5079" "#333333") (if (true-color-p) "#c8c6dd" "#afafff")))
        (ttip-bg       (if (eq variant 'dark) (if (true-color-p) "#34323e" "#444444") (if (true-color-p) "#e2e0ea" "#dfdfff")))
        (type          (if (eq variant 'dark) (if (true-color-p) "#ce537a" "#df005f") (if (true-color-p) "#ba2f59" "#af005f")))
        (var           (if (eq variant 'dark) (if (true-color-p) "#7590db" "#8787d7") (if (true-color-p) "#715ab1" "#af5fd7")))
        (war           (if (eq variant 'dark) (if (true-color-p) "#dc752f" "#dc752f") (if (true-color-p) "#dc752f" "#dc752f")))

        ;; colors
        (aqua          (if (eq variant 'dark) (if (true-color-p) "#2d9574" "#2aa198") (if (true-color-p) "#2d9574" "#2aa198")))
        (aqua-bg       (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
        (green         (if (eq variant 'dark) (if (true-color-p) "#67b11d" "#67b11d") (if (true-color-p) "#67b11d" "#5faf00")))
        (green-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
        (green-bg-s    (if (eq variant 'dark) (if (true-color-p) "#29422d" "#262626") (if (true-color-p) "#dae6d0" "#ffffff")))
        (cyan          (if (eq variant 'dark) (if (true-color-p) "#28def0" "#00ffff") (if (true-color-p) "#21b8c7" "#008080")))
        (red           (if (eq variant 'dark) (if (true-color-p) "#f2241f" "#d70000") (if (true-color-p) "#f2241f" "#d70008")))
        (red-bg        (if (eq variant 'dark) (if (true-color-p) "#3c2a2c" "#262626") (if (true-color-p) "#faede4" "#ffffff")))
        (red-bg-s      (if (eq variant 'dark) (if (true-color-p) "#512e31" "#262626") (if (true-color-p) "#eed9d2" "#ffffff")))
        (blue          (if (eq variant 'dark) (if (true-color-p) "#4f97d7" "#268bd2") (if (true-color-p) "#3a81c3" "#268bd2")))
        (blue-bg       (if (eq variant 'dark) (if (true-color-p) "#293239" "#262626") (if (true-color-p) "#edf1ed" "#d7d7ff")))
        (magenta       (if (eq variant 'dark) (if (true-color-p) "#a31db1" "#af00df") (if (true-color-p) "#a31db1" "#800080")))
        (yellow        (if (eq variant 'dark) (if (true-color-p) "#b1951d" "#875f00") (if (true-color-p) "#b1951d" "#875f00")))
        (yellow-bg     (if (eq variant 'dark) (if (true-color-p) "#32322c" "#262626") (if (true-color-p) "#f6f1e1" "#ffffff")))
        )

        custom-colors-override

    (custom-theme-set-faces
     theme-name

;;;;; basics
     `(cursor ((,class (:background ,cursor))))
     `(custom-button ((,class :background ,bg2 :foreground ,base :box (:line-width 2 :style released-button))))
     `(default ((,class (:background ,bg1 :foreground ,base))))
     `(default-italic ((,class (:italic t))))
     `(error ((,class (:foreground ,err))))
     `(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg1))))
     `(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg1))))
     `(font-lock-builtin-face ((,class (:foreground ,keyword))))
     `(font-lock-comment-face ((,class (:foreground ,(if spacemacs-theme-comment-italic comment-light comment) :background ,(when spacemacs-theme-comment-bg comment-bg) :slant ,(if spacemacs-theme-comment-italic 'italic 'normal)))))
     `(font-lock-constant-face ((,class (:foreground ,const))))
     `(font-lock-doc-face ((,class (:foreground ,comment))))
     `(font-lock-function-name-face ((,class (:foreground ,func :inherit bold))))
     `(font-lock-keyword-face ((,class (:inherit bold :foreground ,keyword))))
     `(font-lock-negation-char-face ((,class (:foreground ,const))))
     `(font-lock-preprocessor-face ((,class (:foreground ,func))))
     `(font-lock-reference-face ((,class (:foreground ,const))))
     `(font-lock-string-face ((,class (:foreground ,str))))
     `(font-lock-type-face ((,class (:foreground ,type :inherit bold))))
     `(font-lock-variable-name-face ((,class (:foreground ,var))))
     `(font-lock-warning-face ((,class (:foreground ,war :background ,bg1))))
     `(fringe ((,class (:background ,bg1 :foreground ,base))))
     `(header-line ((,class :background ,bg4)))
     `(highlight ((,class (:foreground ,base :background ,highlight))))
     `(hl-line ((,class (:background ,bg2))))
     `(isearch ((,class (:foreground ,bg1 :background ,mat))))
     `(lazy-highlight ((,class (:background ,green-bg-s :weight normal))))
     `(link ((,class (:foreground ,comment :underline t))))
     `(link-visited ((,class (:foreground ,comp :underline t))))
     `(match ((,class (:background ,highlight :foreground ,mat))))
     `(minibuffer-prompt ((,class (:inherit bold :foreground ,keyword))))
     `(page-break-lines ((,class (:foreground ,act2))))
     `(region ((,class (:background ,highlight))))
     `(secondary-selection ((,class (:background ,bg3))))
     `(shadow ((,class (:foreground ,base-dim))))
     `(success ((,class (:foreground ,suc))))
     `(tooltip ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))
     `(vertical-border ((,class (:foreground ,border))))
     `(warning ((,class (:foreground ,war))))

;;;;; ahs
     `(ahs-face ((,class (:background ,highlight))))
     `(ahs-plugin-whole-buffer-face ((,class (:background ,mat :foreground ,bg1))))

;;;;; anzu-mode
     `(anzu-mode-line ((,class (:foreground ,yellow :inherit bold))))

;;;;; auto-complete
     `(ac-completion-face ((,class (:background ,ttip-bg :foreground ,ttip))))

;;;;; avy
     `(avy-lead-face   ((,class (:background ,blue-bg :foreground ,magenta))))
     `(avy-lead-face-0 ((,class (:background ,blue-bg :foreground ,blue))))
     `(avy-lead-face-1 ((,class (:background ,blue-bg :foreground ,magenta))))
     `(avy-lead-face-2 ((,class (:background ,blue-bg :foreground ,blue))))

;;;;; cider
     `(cider-enlightened ((,class (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))))
     `(cider-enlightened-local ((,class (:foreground ,yellow))))
     `(cider-instrumented-face ((,class (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))))
     `(cider-result-overlay-face ((,class (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))))
     `(cider-test-error-face ((,class (:background ,war :foreground ,bg1))))
     `(cider-test-failure-face ((,class (:background ,err :foreground ,bg1))))
     `(cider-test-success-face ((,class (:background ,suc :foreground ,bg1))))
     `(cider-traced-face ((,class :box (:color ,cyan :line-width -1 :style nil))))

;;;;; company
     `(company-echo-common ((,class (:background ,base :foreground ,bg1))))
     `(company-preview ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(company-preview-common ((,class (:background ,ttip-bg :foreground ,base))))
     `(company-preview-search ((,class (:inherit match))))
     `(company-scrollbar-bg ((,class (:background ,bg2))))
     `(company-scrollbar-fg ((,class (:background ,act2))))
     `(company-template-field ((,class (:inherit region))))
     `(company-tooltip ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(company-tooltip-annotation ((,class (:foreground ,keyword))))
     `(company-tooltip-common ((,class (:background ,ttip-bg :foreground ,base))))
     `(company-tooltip-common-selection ((,class (:foreground ,base))))
     `(company-tooltip-mouse ((,class (:inherit highlight))))
     `(company-tooltip-search ((,class (:inherit match))))
     `(company-tooltip-selection ((,class (:background ,ttip-sl :foreground ,base))))

;;;;; diff
     `(diff-added             ((,class :background nil :foreground ,green)))
     `(diff-changed           ((,class :background nil :foreground ,keyword)))
     `(diff-header            ((,class :background ,cblk-ln-bg :foreground ,func)))
     `(diff-file-header       ((,class :background ,cblk-ln-bg :foreground ,cblk)))
     `(diff-indicator-added   ((,class :background nil :foreground ,green)))
     `(diff-indicator-changed ((,class :background nil :foreground ,keyword)))
     `(diff-indicator-removed ((,class :background nil :foreground ,red)))
     `(diff-refine-added      ((,class :background ,green :foreground ,bg4)))
     `(diff-refine-changed    ((,class :background ,keyword :foreground ,bg4)))
     `(diff-refine-removed    ((,class :background ,red :foreground ,bg4)))
     `(diff-removed           ((,class :background nil :foreground ,red)))

;;;;; diff-hl
     `(diff-hl-change ((,class :background ,blue-bg :foreground ,blue)))
     `(diff-hl-delete ((,class :background ,red-bg :foreground ,red)))
     `(diff-hl-insert ((,class :background ,green-bg :foreground ,green)))

;;;;; dired
     `(dired-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(dired-flagged ((,class (:foreground ,red))))
     `(dired-header ((,class (:foreground ,comp :inherit bold))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,comp :inherit bold))))
     `(dired-marked ((,class (:foreground ,magenta :inherit bold))))
     `(dired-perm-write ((,class (:foreground ,base :underline t))))
     `(dired-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
     `(dired-warning ((,class (:foreground ,war))))

;;;;; ediff
     `(ediff-current-diff-A ((,class(:background ,red-bg-s :foreground ,red))))
     `(ediff-current-diff-Ancestor ((,class(:background ,aqua-bg :foreground ,aqua))))
     `(ediff-current-diff-B ((,class(:background ,green-bg-s :foreground ,green))))
     `(ediff-current-diff-C ((,class(:background ,blue-bg :foreground ,blue))))
     `(ediff-even-diff-A ((,class(:background ,bg3))))
     `(ediff-even-diff-Ancestor ((,class(:background ,bg3))))
     `(ediff-even-diff-B ((,class(:background ,bg3))))
     `(ediff-even-diff-C ((,class(:background ,bg3))))
     `(ediff-fine-diff-A ((,class(:background nil :inherit bold))))
     `(ediff-fine-diff-Ancestor ((,class(:background nil :inherit bold))))
     `(ediff-fine-diff-B ((,class(:background nil :inherit bold))))
     `(ediff-fine-diff-C ((,class(:background nil :inherit bold))))
     `(ediff-odd-diff-A ((,class(:background ,bg4))))
     `(ediff-odd-diff-Ancestor ((,class(:background ,bg4))))
     `(ediff-odd-diff-B ((,class(:background ,bg4))))
     `(ediff-odd-diff-C ((,class(:background ,bg4))))

;;;;; ein
     `(ein:cell-input-area((,class (:background ,bg2))))
     `(ein:cell-input-prompt ((,class (:foreground ,suc))))
     `(ein:cell-output-prompt ((,class (:foreground ,err))))
     `(ein:notification-tab-normal ((,class (:foreground ,keyword))))
     `(ein:notification-tab-selected ((,class (:foreground ,suc :inherit bold))))

;;;;; eldoc
     `(eldoc-highlight-function-argument ((,class (:foreground ,mat :inherit bold))))

;;;;; elfeed
     `(elfeed-search-date-face ((,class (:foreground ,head2))))
     `(elfeed-search-feed-face ((,class (:foreground ,blue))))
     `(elfeed-search-tag-face ((,class (:foreground ,func))))
     `(elfeed-search-title-face ((,class (:foreground ,base-dim))))
     `(elfeed-search-unread-title-face ((,class (:foreground ,base))))

;;;;; enh-ruby
     `(enh-ruby-op-face ((,class (:background ,bg1 :foreground ,base))))
     `(enh-ruby-string-delimiter-face ((,class (:foreground ,str))))

;;;;; erc
     `(erc-input-face ((,class (:foreground ,func))))
     `(erc-my-nick-face ((,class (:foreground ,keyword))))
     `(erc-nick-default-face ((,class (:foreground ,keyword))))
     `(erc-nick-prefix-face ((,class (:foreground ,yellow))))
     `(erc-notice-face ((,class (:foreground ,str))))
     `(erc-prompt-face ((,class (:foreground ,mat :inherit bold))))
     `(erc-timestamp-face ((,class (:foreground ,keyword))))

;;;;; eshell
     `(eshell-ls-archive ((,class (:foreground ,red :inherit bold))))
     `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-directory ((,class (:foreground ,keyword :inherit bold))))
     `(eshell-ls-executable ((,class (:foreground ,suc :inherit bold))))
     `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
     `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
     `(eshell-ls-special ((,class (:foreground ,yellow :inherit bold))))
     `(eshell-ls-symlink ((,class (:foreground ,cyan :inherit bold))))
     `(eshell-ls-unreadable ((,class (:foreground ,base))))
     `(eshell-prompt ((,class (:foreground ,keyword :inherit bold))))

;;;;; evil
     `(evil-ex-substitute-matches ((,class (:background ,red-bg :foreground ,red))))
     `(evil-ex-substitute-replacement ((,class (:background ,green-bg :foreground ,green))))

;;;;; flycheck
     `(flycheck-error
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,err)))
        (,class (:foreground ,base :background ,err :inherit bold :underline t))))
     `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
     `(flycheck-fringe-error ((,class (:foreground ,err :inherit bold))))
     `(flycheck-fringe-info ((,class (:foreground ,keyword :inherit bold))))
     `(flycheck-fringe-warning ((,class (:foreground ,war :inherit bold))))
     `(flycheck-info
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,keyword)))
        (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
     `(flycheck-warning
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,war)))
        (,class (:foreground ,base :background ,war :inherit bold :underline t))))

;;;;; jabber
     `(jabber-activity-face ((,class (:inherit bold :foreground ,red))))
     `(jabber-activity-personal-face ((,class (:inherit bold :foreground ,blue))))
     `(jabber-chat-error ((,class (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-foreign ((,class (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-local ((,class (:inherit bold :foreground ,blue))))
     `(jabber-chat-prompt-system ((,class (:inherit bold :foreground ,green))))
     `(jabber-chat-text-foreign ((,class (:foreground ,base))))
     `(jabber-chat-text-local ((,class (:foreground ,base))))
     `(jabber-rare-time-face ((,class (:foreground ,green))))
     `(jabber-roster-user-away ((,class (:foreground ,yellow))))
     `(jabber-roster-user-chatty ((,class (:inherit bold :foreground ,green))))
     `(jabber-roster-user-dnd ((,class (:foreground ,red))))
     `(jabber-roster-user-error ((,class (:foreground ,err))))
     `(jabber-roster-user-offline ((,class (:foreground ,base))))
     `(jabber-roster-user-online ((,class (:inherit bold :foreground ,green))))
     `(jabber-roster-user-xa ((,class (:foreground ,aqua))))

;;;;; git-gutter-fr
     `(git-gutter-fr:added ((,class (:foreground ,green :inherit bold))))
     `(git-gutter-fr:deleted ((,class (:foreground ,war :inherit bold))))
     `(git-gutter-fr:modified ((,class (:foreground ,keyword :inherit bold))))

;;;;; git-timemachine
     `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :inherit bold :background ,blue-bg))))

;;;;; gnus
     `(gnus-emphasis-highlight-words ((,class (:background ,suc :foreground ,bg1))))
     `(gnus-header-content ((,class (:foreground ,keyword))))
     `(gnus-header-from ((,class (:foreground ,var))))
     `(gnus-header-name ((,class (:foreground ,comp))))
     `(gnus-header-subject ((,class (:foreground ,func :inherit bold))))
     `(gnus-summary-cancelled ((,class (:background ,war :foreground ,bg1))))

;;;;; guide-key
     `(guide-key/highlight-command-face ((,class (:foreground ,base))))
     `(guide-key/key-face ((,class (:foreground ,keyword))))
     `(guide-key/prefix-command-face ((,class (:foreground ,keyword :inherit bold))))

;;;;; helm
     `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((,class (:foreground ,base))))
     `(helm-bookmark-gnus ((,class (:foreground ,comp))))
     `(helm-bookmark-info ((,class (:foreground ,comp))))
     `(helm-bookmark-man ((,class (:foreground ,comp))))
     `(helm-bookmark-w3m ((,class (:foreground ,comp))))
     `(helm-buffer-directory ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-file ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-not-saved ((,class (:foreground ,comp :background ,bg1))))
     `(helm-buffer-process ((,class (:foreground ,keyword :background ,bg1))))
     `(helm-buffer-saved-out ((,class (:foreground ,base :background ,bg1))))
     `(helm-buffer-size ((,class (:foreground ,base :background ,bg1))))
     `(helm-candidate-number ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))
     `(helm-ff-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(helm-ff-dotted-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
     `(helm-ff-dotted-symlink-directory ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
     `(helm-ff-executable ((,class (:foreground ,suc :background ,bg1 :weight normal))))
     `(helm-ff-file ((,class (:foreground ,base :background ,bg1 :weight normal))))
     `(helm-ff-invalid-symlink ((,class (:foreground ,red :background ,bg1 :inherit bold))))
     `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
     `(helm-ff-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
     `(helm-grep-cmd-line ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-file ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-finish ((,class (:foreground ,base :background ,bg1))))
     `(helm-grep-lineno ((,class (:foreground ,type :background ,bg1 :inherit bold))))
     `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
     `(helm-header ((,class (:foreground ,base :background ,bg1 :underline nil :box nil))))
     `(helm-header-line-left-margin ((,class (:foreground ,keyword :background ,nil))))
     `(helm-match ((,class (:background ,head1-bg :foreground ,head1))))
     `(helm-match-item ((,class (:background ,head1-bg :foreground ,head1))))
     `(helm-moccur-buffer ((,class (:foreground ,var :background ,bg1))))
     `(helm-selection ((,class (:background ,highlight))))
     `(helm-selection-line ((,class (:background ,bg2))))
     `(helm-separator ((,class (:foreground ,comp :background ,bg1))))
     `(helm-source-header ((,class (:background ,comp :foreground ,bg1 :inherit bold))))
     `(helm-time-zone-current ((,class (:foreground ,keyword :background ,bg1))))
     `(helm-time-zone-home ((,class (:foreground ,comp :background ,bg1))))
     `(helm-visible-mark ((,class (:foreground ,keyword :background ,bg3))))

;;;;; helm-swoop
     `(helm-swoop-target-line-block-face ((,class (:foreground ,base :background ,highlight))))
     `(helm-swoop-target-line-face ((,class (:background ,highlight))))
     `(helm-swoop-target-word-face ((,class (:background ,highlight :foreground ,mat))))

;;;;; highlights
     `(hi-green  ((,class (:foreground ,green :background ,green-bg))))
     `(hi-yellow ((,class (:foreground ,yellow :background ,yellow-bg))))

;;;;; highlight-indentation
     `(highlight-indentation-face ((,class (:background ,comment-bg))))

;;;;; highlight-symbol
     `(highlight-symbol-face ((,class (:background ,bg2))))

;;;;; hydra
     `(hydra-face-blue ((,class (:foreground ,blue))))
     `(hydra-face-red ((,class (:foreground ,red))))

;;;;; ido
     `(ido-first-match ((,class (:foreground ,comp :inherit bold))))
     `(ido-only-match ((,class (:foreground ,mat :inherit bold))))
     `(ido-subdir ((,class (:foreground ,keyword))))
     `(ido-vertical-match-face ((,class (:foreground ,comp :underline nil))))

;;;;; info
     `(info-header-xref ((,class (:foreground ,func :underline t))))
     `(info-menu ((,class (:foreground ,suc))))
     `(info-node ((,class (:foreground ,func :inherit bold))))
     `(info-quoted-name ((,class (:foreground ,keyword))))
     `(info-reference-item ((,class (:background nil :underline t :inherit bold))))
     `(info-string ((,class (:foreground ,str))))
     `(info-title-1 ((,class (:height 1.4 :inherit bold))))
     `(info-title-2 ((,class (:height 1.3 :inherit bold))))
     `(info-title-3 ((,class (:height 1.3))))
     `(info-title-4 ((,class (:height 1.2))))

;;;;; ivy
     `(ivy-current-match ((,class (:background ,highlight :inherit bold))))
     `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
     `(ivy-minibuffer-match-face-2 ((,class (:foreground ,head1 :underline t))))
     `(ivy-minibuffer-match-face-3 ((,class (:foreground ,head4 :underline t))))
     `(ivy-minibuffer-match-face-4 ((,class (:foreground ,head3 :underline t))))
     `(ivy-remote ((,class (:foreground ,cyan))))

;;;;; latex
     `(font-latex-bold-face ((,class (:foreground ,comp))))
     `(font-latex-italic-face ((,class (:foreground ,keyword :italic t))))
     `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
     `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
     `(font-latex-sectioning-0-face ((,class (:inherit bold :foreground ,head3 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-1-face ((,class (:inherit bold :foreground ,head4 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head4-bg)))))
     `(font-latex-sectioning-2-face ((,class (:inherit bold :foreground ,head1 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head1-bg)))))
     `(font-latex-sectioning-3-face ((,class (:inherit bold :foreground ,head2 :height ,(if spacemacs-theme-org-height 1.2 1.0) :background ,(when spacemacs-theme-org-highlight head2-bg)))))
     `(font-latex-sectioning-4-face ((,class (:bold nil :foreground ,head3 :height ,(if spacemacs-theme-org-height 1.1 1.0) :background ,(when spacemacs-theme-org-highlight head3-bg)))))
     `(font-latex-sectioning-5-face ((,class (:bold nil :foreground ,head4 :background ,(when spacemacs-theme-org-highlight head4-bg)))))
     `(font-latex-string-face ((,class (:foreground ,str))))
     `(font-latex-warning-face ((,class (:foreground ,war))))

;;;;; ledger-mode
     `(ledger-font-directive-face ((,class (:foreground ,meta))))
     `(ledger-font-posting-amount-face ((,class (:foreground ,yellow))))
     `(ledger-font-posting-date-face ((,class (:foreground ,head1))))
     `(ledger-occur-xact-face ((,class (:background ,bg2))))

;;;;; linum-mode
     `(linum ((,class (:foreground ,lnum :background ,bg2))))

;;;;; linum-relative
     `(linum-relative-current-face ((,class (:foreground ,comp))))

;;;;; magit
     `(magit-blame-culprit ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-date    ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-blame-hash    ((,class :background ,yellow-bg :foreground ,func)))
     `(magit-blame-header  ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-blame-heading ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-blame-name    ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-sha1    ((,class :background ,yellow-bg :foreground ,func)))
     `(magit-blame-subject ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-summary ((,class :background ,yellow-bg :foreground ,yellow)))
     `(magit-blame-time    ((,class :background ,yellow-bg :foreground ,green)))
     `(magit-branch ((,class (:foreground ,const :inherit bold))))
     `(magit-branch-current ((,class (:background ,blue-bg :foreground ,blue :inherit bold :box t))))
     `(magit-branch-local ((,class (:background ,blue-bg :foreground ,blue :inherit bold))))
     `(magit-branch-remote ((,class (:background ,aqua-bg :foreground ,aqua :inherit bold))))
     `(magit-diff-context-highlight ((,class (:background ,bg2 :foreground ,base))))
     `(magit-diff-file-header ((,class (:background ,comment-bg :foreground ,comment))))
     `(magit-diff-file-heading ((,class (:background ,comment-bg :foreground ,comment))))
     `(magit-diff-file-heading-highlight ((,class (:background ,comment-bg :foreground ,comment))))
     `(magit-diff-hunk-header ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(magit-diff-hunk-heading ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(magit-diff-hunk-heading-highlight ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(magit-hash ((,class (:foreground ,var))))
     `(magit-hunk-heading           ((,class (:background ,bg3))))
     `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
     `(magit-item-highlight ((,class :background ,bg2)))
     `(magit-log-author ((,class (:foreground ,func))))
     `(magit-log-head-label-head ((,class (:background ,yellow :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-local ((,class (:background ,keyword :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-remote ((,class (:background ,suc :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-tags ((,class (:background ,magenta :foreground ,bg1 :inherit bold))))
     `(magit-log-head-label-wip ((,class (:background ,cyan :foreground ,bg1 :inherit bold))))
     `(magit-log-sha1 ((,class (:foreground ,str))))
     `(magit-process-ng ((,class (:foreground ,war :inherit bold))))
     `(magit-process-ok ((,class (:foreground ,func :inherit bold))))
     `(magit-reflog-amend ((,class (:foreground ,magenta))))
     `(magit-reflog-checkout ((,class (:foreground ,blue))))
     `(magit-reflog-cherry-pick ((,class (:foreground ,green))))
     `(magit-reflog-commit ((,class (:foreground ,green))))
     `(magit-reflog-merge ((,class (:foreground ,green))))
     `(magit-reflog-other ((,class (:foreground ,cyan))))
     `(magit-reflog-rebase ((,class (:foreground ,magenta))))
     `(magit-reflog-remote ((,class (:foreground ,cyan))))
     `(magit-reflog-reset ((,class (:foreground ,red))))
     `(magit-section-heading        ((,class (:foreground ,keyword :inherit bold))))
     `(magit-section-highlight      ((,class (:background ,bg2))))
     `(magit-section-title ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))

;;;;; man
     `(Man-overstrike ((,class (:foreground ,head1 :inherit bold))))
     `(Man-reverse ((,class (:foreground ,highlight))))
     `(Man-underline ((,class (:foreground ,comp :underline t))))

;;;;; markdown
     `(markdown-header-face-1 ((,class (:inherit bold :foreground ,head1 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head1-bg)))))
     `(markdown-header-face-2 ((,class (:inherit bold :foreground ,head2 :height ,(if spacemacs-theme-org-height 1.2 1.0) :background ,(when spacemacs-theme-org-highlight head2-bg)))))
     `(markdown-header-face-3 ((,class (:bold nil :foreground ,head3 :height ,(if spacemacs-theme-org-height 1.1 1.0) :background ,(when spacemacs-theme-org-highlight head3-bg)))))
     `(markdown-header-face-4 ((,class (:bold nil :foreground ,head4 :background ,(when spacemacs-theme-org-highlight head4-bg)))))
     `(markdown-header-face-5 ((,class (:bold nil :foreground ,head1))))
     `(markdown-header-face-6 ((,class (:bold nil :foreground ,head2))))

;;;;; mode-line
     `(mode-line           ((,class (:foreground ,base :background ,act1 :box (:color ,border :line-width 1)))))
     `(mode-line-buffer-id ((,class (:inherit bold :foreground ,func))))
     `(mode-line-inactive  ((,class (:foreground ,base :background ,bg1  :box (:color ,border :line-width 1)))))

;;;;; mu4e
     `(mu4e-attach-number-face ((,class (:foreground ,var))))
     `(mu4e-cited-1-face ((,class (:foreground ,head1))))
     `(mu4e-cited-2-face ((,class (:foreground ,head2))))
     `(mu4e-cited-3-face ((,class (:foreground ,head3))))
     `(mu4e-cited-4-face ((,class (:foreground ,head4))))
     `(mu4e-cited-5-face ((,class (:foreground ,head1))))
     `(mu4e-cited-6-face ((,class (:foreground ,head2))))
     `(mu4e-cited-7-face ((,class (:foreground ,head3))))
     `(mu4e-draft-face ((,class (:foreground ,var))))
     `(mu4e-flagged-face ((,class (:foreground ,yellow :inherit bold))))
     `(mu4e-header-key-face ((,class (:foreground ,meta :inherit bold))))
     `(mu4e-header-marks-face ((,class (:foreground ,comp))))
     `(mu4e-header-highlight-face ((,class (:background ,highlight))))
     `(mu4e-highlight-face ((,class (:foreground ,comp))))
     `(mu4e-title-face ((,class (:foreground ,head1 :inherit bold))))
     `(mu4e-replied-face ((,class (:foreground ,green))))
     `(mu4e-modeline-face ((,class (:foreground ,func))))
     `(mu4e-special-header-value-face ((,class (:foreground ,green))))
     `(mu4e-unread-face ((,class (:foreground ,type :inherit bold))))
     `(mu4e-view-url-number-face ((,class (:foreground ,comp))))

;;;;; mu4e-maildirs
     `(mu4e-maildirs-extension-maildir-hl-face ((,class (:foreground ,head2 :inherit bold))))

;;;;; notmuch
     `(notmuch-search-date ((,class (:foreground ,func))))
     `(notmuch-search-flagged-face ((,class (:weight extra-bold))))
     `(notmuch-search-non-matching-authors ((,class (:foreground ,base-dim))))
     `(notmuch-search-unread-face ((,class (:background ,highlight-dim :box ,border))))
     `(notmuch-tag-face ((,class (:foreground ,keyword))))
     `(notmuch-tag-flagged ((,class (:foreground ,war))))

;;;;; neotree
     `(neo-dir-link-face ((,class (:foreground ,keyword :inherit bold))))
     `(neo-expand-btn-face ((,class (:foreground ,base))))
     `(neo-file-link-face ((,class (:foreground ,base))))
     `(neo-root-dir-face ((,class (:foreground ,func :inherit bold))))

;;;;; org
     `(org-agenda-clocking ((,class (:background ,highlight :foreground ,comp))))
     `(org-agenda-date ((,class (:foreground ,var :height ,(if spacemacs-theme-org-agenda-height 1.1 1.0)))))
     `(org-agenda-date-today ((,class (:foreground ,keyword :inherit bold :height ,(if spacemacs-theme-org-agenda-height 1.3 1.0)))))
     `(org-agenda-date-weekend ((,class (:inherit bold :foreground ,var))))
     `(org-agenda-done ((,class (:foreground ,suc :height ,(if spacemacs-theme-org-agenda-height 1.2 1.0)))))
     `(org-agenda-structure ((,class (:inherit bold :foreground ,comp))))
     `(org-block ((,class (:background ,cblk-bg :foreground ,cblk))))
     `(org-block-begin-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln))))
     `(org-block-end-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln))))
     `(org-clock-overlay ((,class (:foreground ,comp))))
     `(org-code ((,class (:foreground ,cyan))))
     `(org-column ((,class (:background ,highlight))))
     `(org-column-title ((,class (:background ,highlight))))
     `(org-date ((,class (:underline t :foreground ,var))))
     `(org-date-selected ((,class (:background ,func :foreground ,bg1))))
     `(org-document-info-keyword ((,class (:foreground ,meta))))
     `(org-document-title ((,class (:foreground ,func :inherit bold :height ,(if spacemacs-theme-org-height 1.4 1.0) :underline t))))
     `(org-done ((,class (:foreground ,suc :inherit bold :background ,green-bg))))
     `(org-ellipsis ((,class (:foreground ,keyword))))
     `(org-footnote  ((,class (:underline t :foreground ,base))))
     `(org-hide ((,class (:foreground ,base))))
     `(org-kbd ((,class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
     `(org-level-1 ((,class (:inherit bold :foreground ,head1 :height ,(if spacemacs-theme-org-height 1.3 1.0) :background ,(when spacemacs-theme-org-highlight head1-bg)))))
     `(org-level-2 ((,class (:inherit bold :foreground ,head2 :height ,(if spacemacs-theme-org-height 1.2 1.0) :background ,(when spacemacs-theme-org-highlight head2-bg)))))
     `(org-level-3 ((,class (:bold nil :foreground ,head3 :height ,(if spacemacs-theme-org-height 1.1 1.0) :background ,(when spacemacs-theme-org-highlight head3-bg)))))
     `(org-level-4 ((,class (:bold nil :foreground ,head4 :background ,(when spacemacs-theme-org-highlight head4-bg)))))
     `(org-level-5 ((,class (:bold nil :foreground ,head1))))
     `(org-level-6 ((,class (:bold nil :foreground ,head2))))
     `(org-level-7 ((,class (:bold nil :foreground ,head3))))
     `(org-level-8 ((,class (:bold nil :foreground ,head4))))
     `(org-link ((,class (:underline t :foreground ,comment))))
     `(org-meta-line ((,class (:foreground ,meta))))
     `(org-mode-line-clock-overrun ((,class (:foreground ,err))))
     `(org-priority ((,class (:foreground ,war :inherit bold))))
     `(org-quote ((,class (:inherit org-block :slant italic))))
     `(org-scheduled ((,class (:foreground ,comp))))
     `(org-scheduled-today ((,class (:foreground ,func :height ,(if spacemacs-theme-org-agenda-height 1.2 1.0)))))
     `(org-sexp-date ((,class (:foreground ,base))))
     `(org-special-keyword ((,class (:foreground ,func))))
     `(org-table ((,class (:foreground ,base :background ,head1-bg))))
     `(org-time-grid ((,class (:foreground ,str))))
     `(org-todo ((,class (:foreground ,war :inherit bold :background ,yellow-bg))))
     `(org-verbatim ((,class (:foreground ,keyword))))
     `(org-verse ((,class (:inherit org-block :slant italic))))
     `(org-warning ((,class (:foreground ,err))))

;;;;; perspective
     `(persp-selected-face ((,class (:inherit bold :foreground ,func))))

;;;;; popup
     `(popup-enu-selection-face ((,class (:background ,ttip-sl :foreground ,base))))
     `(popup-face ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(popup-isearch-match ((,class (:inherit match))))
     `(popup-menu-face ((,class (:background ,ttip-bg :foreground ,base))))
     `(popup-menu-mouse-face ((,class (:inherit highlight))))
     `(popup-scroll-bar-background-face ((,class (:background ,bg2))))
     `(popup-scroll-bar-foreground-face ((,class (:background ,act2))))
     `(popup-tip-face ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))

;;;;; powerline
     `(powerline-active1 ((,class (:background ,act2 :foreground ,base))))
     `(powerline-active2 ((,class (:background ,act2 :foreground ,base))))
     `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,base))))
     `(powerline-inactive2 ((,class (:background ,bg2 :foreground ,base))))

;;;;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,class :foreground ,keyword)))
     `(rainbow-delimiters-depth-2-face ((,class :foreground ,func)))
     `(rainbow-delimiters-depth-3-face ((,class :foreground ,str)))
     `(rainbow-delimiters-depth-4-face ((,class :foreground ,green)))
     `(rainbow-delimiters-depth-5-face ((,class :foreground ,yellow)))
     `(rainbow-delimiters-depth-6-face ((,class :foreground ,keyword)))
     `(rainbow-delimiters-depth-7-face ((,class :foreground ,func)))
     `(rainbow-delimiters-depth-8-face ((,class :foreground ,str)))
     `(rainbow-delimiters-mismatched-face ((,class :foreground ,err :overline t)))
     `(rainbow-delimiters-unmatched-face ((,class :foreground ,err :overline t)))

;;;;; rcirc
     `(rcirc-bright-nick ((,class (:background ,aqua-bg :foreground ,cyan))))
     `(rcirc-dim-nick ((,class (:foreground ,base-dim))))
     `(rcirc-keyword ((,class (:background ,green-bg-s :foreground ,green))))
     `(rcirc-timestamp ((,class (:foreground ,keyword))))
     `(rcirc-track-keyword ((,class (:background ,green :foreground ,bg1))))
     `(rcirc-url ((,class (:inherit link))))

;;;;; shm
     `(shm-current-face ((,class (:background ,green-bg-s))))
     `(shm-quarantine-face ((,class (:background ,red-bg-s))))

;;;;; show-paren
     `(show-paren-match ((,class (:background ,green-bg-s))))
     `(show-paren-mismatch ((,class (:background ,red-bg-s))))

;;;;; smartparens
     `(sp-pair-overlay-face ((,class (:background ,highlight :foreground nil))))
     `(sp-show-pair-match-face ((,class (:foreground ,mat :inherit bold :underline t))))

;;;;; smerge
     `(smerge-base ((,class (:background ,yellow-bg))))
     `(smerge-markers ((,class (:background ,ttip-bg :foreground ,ttip))))
     `(smerge-mine ((,class (:background ,red-bg))))
     `(smerge-other ((,class (:background ,green-bg))))
     `(smerge-refined-added ((,class (:background ,green-bg-s :foreground ,green))))
     `(smerge-refined-changed ((,class (:background ,blue-bg :foreground ,blue))))
     `(smerge-refined-removed ((,class (:background ,red-bg-s :foreground ,red))))

;;;;; spaceline
     `(spaceline-flycheck-error  ((,class (:foreground ,err))))
     `(spaceline-flycheck-info   ((,class (:foreground ,keyword))))
     `(spaceline-flycheck-warning((,class (:foreground ,war))))
     `(spaceline-python-venv ((,class (:foreground ,comp))))

;;;;; spacemacs-specific
     `(spacemacs-transient-state-title-face ((,class (:background nil :foreground ,comp :box nil :inherit bold))))

;;;;; swiper
     `(swiper-line-face ((,class (:background ,highlight :inherit bold))))
     `(swiper-match-face-1 ((,class (:inherit bold))))
     `(swiper-match-face-2 ((,class (:foreground ,head1 :underline t))))
     `(swiper-match-face-3 ((,class (:foreground ,head4 :underline t))))
     `(swiper-match-face-4 ((,class (:foreground ,head3 :underline t))))

;;;;; tabbar
     `(tabbar-button ((,class (:inherit tabbar-default ))))
     `(tabbar-button-highlight ((,class (:inherit tabbar-default))))
     `(tabbar-default ((,class (:background ,bg1 :foreground ,head1 :height 0.9))))
     `(tabbar-highlight ((,class (:underline t))))
     `(tabbar-selected ((,class (:inherit tabbar-default :foreground ,func :weight bold))))
     `(tabbar-separator ((,class (:inherit tabbar-default))))
     `(tabbar-unselected ((,class (:inherit tabbar-default :background ,bg1 :slant italic :weight light))))

;;;;; term
     `(term ((,class (:foreground ,base :background ,bg1))))
     `(term-color-black ((,class (:foreground ,bg4))))
     `(term-color-blue ((,class (:foreground ,keyword))))
     `(term-color-cyan ((,class (:foreground ,cyan))))
     `(term-color-green ((,class (:foreground ,green))))
     `(term-color-magenta ((,class (:foreground ,magenta))))
     `(term-color-red ((,class (:foreground ,red))))
     `(term-color-white ((,class (:foreground ,base))))
     `(term-color-yellow ((,class (:foreground ,yellow))))

;;;;; web-mode
     `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
     `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
     `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
     `(web-mode-html-tag-face ((,class (:foreground ,keyword))))
     `(web-mode-keyword-face ((,class (:foreground ,keyword))))
     `(web-mode-string-face ((,class (:foreground ,str))))
     `(web-mode-symbol-face ((,class (:foreground ,type))))
     `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
     `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))

;;;;; which-key
     `(which-key-command-description-face ((,class (:foreground ,base))))
     `(which-key-group-description-face ((,class (:foreground ,keyword))))
     `(which-key-key-face ((,class (:foreground ,func :inherit bold))))
     `(which-key-separator-face ((,class (:background nil :foreground ,str))))
     `(which-key-special-key-face ((,class (:background ,func :foreground ,bg1))))

;;;;; which-function-mode
     `(which-func ((,class (:foreground ,func))))

;;;;; whitespace-mode
     `(whitespace-empty ((,class (:background nil :foreground ,yellow))))
     `(whitespace-indentation ((,class (:background nil :foreground ,war))))
     `(whitespace-line ((,class (:background nil :foreground ,comp))))
     `(whitespace-newline ((,class (:background nil :foreground ,comp))))
     `(whitespace-space ((,class (:background nil :foreground ,act2))))
     `(whitespace-space-after-tab ((,class (:background nil :foreground ,yellow))))
     `(whitespace-space-before-tab ((,class (:background nil :foreground ,yellow))))
     `(whitespace-tab ((,class (:background nil))))
     `(whitespace-trailing ((,class (:background ,err :foreground ,war))))

;;;;; other, need more work
     `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
     `(ffap ((,class (:foreground ,base))))
     `(flx-highlight-face ((,class (:foreground ,comp :underline nil))))
     `(icompletep-determined ((,class :foreground ,keyword)))
     `(js2-external-variable ((,class (:foreground ,comp))))
     `(js2-function-param ((,class (:foreground ,const))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,keyword))))
     `(js2-jsdoc-value ((,class (:foreground ,str))))
     `(js2-private-function-call ((,class (:foreground ,const))))
     `(js2-private-member ((,class (:foreground ,base))))
     `(js3-error-face ((,class (:underline ,war))))
     `(js3-external-variable-face ((,class (:foreground ,var))))
     `(js3-function-param-face ((,class (:foreground ,keyword))))
     `(js3-instance-member-face ((,class (:foreground ,const))))
     `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
     `(js3-warning-face ((,class (:underline ,keyword))))
     `(slime-repl-inputed-output-face ((,class (:foreground ,comp))))
     `(trailing-whitespace ((,class :foreground nil :background ,err)))
     `(undo-tree-visualizer-current-face ((,class :foreground ,keyword)))
     `(undo-tree-visualizer-default-face ((,class :foreground ,base)))
     `(undo-tree-visualizer-register-face ((,class :foreground ,comp)))
     `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var))))

    (custom-theme-set-variables
     theme-name
     `(ansi-color-names-vector [,bg4 ,red ,green ,yellow ,blue ,magenta ,cyan ,base]))

    ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'spacemacs-common)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; spacemacs-common.el ends here
