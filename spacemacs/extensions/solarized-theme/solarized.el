;;; solarized.el --- Solarized for Emacs.

;; Copyright (C) 2011-2015 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: http://github.com/bbatsov/solarized-emacs
;; Version: 1.2.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of Solarized to Emacs.
;;
;;; Installation:
;;
;;   Drop the `solarized-theme.el` somewhere in your `load-path` and
;; the two themes in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Credits
;;
;; Ethan Schoonover created the original theme for vim on such this port
;; is based.
;;
;;; Code:

(require 'dash)

;;; Options

(defgroup solarized nil
  "Solarized theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom solarized-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects `linum-mode' background."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-variable-pitch t
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-less-bold nil
  "Use bold weight less often."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-more-italic nil
  "Use italic slant more often."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-emphasize-indicators t
  "Use more colors for indicators such as git:gutter, flycheck and similar."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-high-contrast-mode-line nil
  "Make the active/inactive mode line stand out more."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'solarized)

(defcustom solarized-scale-org-headlines t
  "Whether scaling of outline-headlines should apply to org-mode headlines."
  :type 'boolean
  :group 'solarized)

;;; Utilities

(defun solarized-color-name-to-rgb (color &optional frame)
  "Convert COLOR string to a list of normalized RGB components.
COLOR should be a color name (e.g. \"white\") or an RGB triplet
string (e.g. \"#ff12ec\").

Normally the return value is a list of three floating-point
numbers, (RED GREEN BLUE), each between 0.0 and 1.0 inclusive.

Optional argument FRAME specifies the frame where the color is to be
displayed.  If FRAME is omitted or nil, use the selected frame.
If FRAME cannot display COLOR, return nil."
  ;; `colors-values' maximum value is either 65535 or 65280 depending on the
  ;; display system.  So we use a white conversion to get the max value.
  (let ((valmax (float (car (color-values "#ffffff")))))
    (mapcar (lambda (x) (/ x valmax)) (color-values color frame))))

(defun solarized-color-rgb-to-hex  (red green blue)
  "Return hexadecimal notation for the color RED GREEN BLUE.
RED, GREEN, and BLUE should be numbers between 0.0 and 1.0, inclusive."
  (format "#%02x%02x%02x"
          (* red 255) (* green 255) (* blue 255)))

(defun solarized-color-blend (color1 color2 alpha)
  "Blends COLOR1 onto COLOR2 with ALPHA.

COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
triplet strings (e.g. \"#ff12ec\").

Alpha should be a float between 0 and 1."
  (apply 'solarized-color-rgb-to-hex
         (-zip-with '(lambda (it other)
                       (+ (* alpha it) (* other (- 1 alpha))))
                    (solarized-color-name-to-rgb color1)
                    (solarized-color-name-to-rgb color2))))

;;; Setup Start

(defun create-solarized-theme (variant theme-name &optional childtheme)
  "Create a VARIANT of the theme named THEME-NAME.

When optional argument CHILDTHEME function is supplied it's invoked to further
customize the resulting theme."
;;; Color palette
  (let* ((class '((class color) (min-colors 89)))
         (s-base03    "#002b36")
         (s-base02    "#073642")
         ;; emphasized content
         (s-base01    "#586e75")
         ;; primary content
         (s-base00    "#657b83")
         (s-base0     "#839496")
         ;; comments
         (s-base1     "#93a1a1")
         ;; background highlight light
         (s-base2     "#eee8d5")
         ;; background light
         (s-base3     "#fdf6e3")

         ;; Solarized accented colors
         (yellow    "#b58900")
         (orange    "#cb4b16")
         (red       "#dc322f")
         (magenta   "#d33682")
         (violet    "#6c71c4")
         (blue      "#268bd2")
         (cyan      "#2aa198")
         (green     "#859900")

         ;; Darker and lighter accented colors
         ;; Only use these in exceptional circumstances!
         (yellow-d  "#7B6000")
         (yellow-l  "#DEB542")
         (orange-d  "#8B2C02")
         (orange-l  "#F2804F")
         (red-d     "#990A1B")
         (red-l     "#FF6E64")
         (magenta-d "#93115C")
         (magenta-l "#F771AC")
         (violet-d  "#3F4D91")
         (violet-l  "#9EA0E5")
         (blue-d    "#00629D")
         (blue-l    "#69B7F0")
         (cyan-d    "#00736F")
         (cyan-l    "#69CABF")
         (green-d   "#546E00")
         (green-l   "#B4C342")

         ;; Solarized palette names, use these instead of -fg -bg...
         (base0 (if (eq variant 'light) s-base00 s-base0))
         (base00 (if (eq variant 'light) s-base0 s-base00))
         (base1 (if (eq variant 'light) s-base01 s-base1))
         (base01 (if (eq variant 'light) s-base1 s-base01))
         (base2 (if (eq variant 'light) s-base02 s-base2))
         (base02 (if (eq variant 'light) s-base2 s-base02))
         (base3 (if (eq variant 'light) s-base03 s-base3))
         (base03 (if (eq variant 'light) s-base3 s-base03))

         ;; Line drawing color
         ;;
         ;; NOTE only use this for very thin lines that are hard to see using base02, in low
         ;; color displayes base02 might be used instead
         (s-line (if (eq variant 'light) "#cccec4" "#284b54"))

         ;; Light/Dark adaptive higher/lower contrast accented colors
         ;;
         ;; NOTE Only use these in exceptional cirmumstances!
         (yellow-hc (if (eq variant 'light) yellow-d yellow-l))
         (yellow-lc (if (eq variant 'light) yellow-l yellow-d))
         (orange-hc (if (eq variant 'light) orange-d orange-l))
         (orange-lc (if (eq variant 'light) orange-l orange-d))
         (red-hc (if (eq variant 'light) red-d red-l))
         (red-lc (if (eq variant 'light) red-l red-d))
         (magenta-hc (if (eq variant 'light) magenta-d magenta-l))
         (magenta-lc (if (eq variant 'light) magenta-l magenta-d))
         (violet-hc (if (eq variant 'light) violet-d violet-l))
         (violet-lc (if (eq variant 'light) violet-l violet-d))
         (blue-hc (if (eq variant 'light) blue-d blue-l))
         (blue-lc (if (eq variant 'light) blue-l blue-d))
         (cyan-hc (if (eq variant 'light) cyan-d cyan-l))
         (cyan-lc (if (eq variant 'light) cyan-l cyan-d))
         (green-hc (if (eq variant 'light) green-d green-l))
         (green-lc (if (eq variant 'light) green-l green-d))

         ;; customize based face properties
         (s-maybe-bold (if solarized-use-less-bold
                           'unspecified 'bold))
         (s-maybe-italic (if solarized-use-more-italic
                             'italic 'normal))
         (s-variable-pitch (if solarized-use-variable-pitch
                               'variable-pitch 'default))
         (s-fringe-bg (if solarized-distinct-fringe-background
                          base02 base03))
         (s-fringe-fg base01)


         (s-header-line-fg (if solarized-high-contrast-mode-line
                                    base1 base0))
         (s-header-line-bg (if solarized-high-contrast-mode-line
                                    base02 base03))
         (s-header-line-underline (if solarized-high-contrast-mode-line
                                      nil base02))

         (s-mode-line-fg (if solarized-high-contrast-mode-line
                             base03 base0))
         (s-mode-line-bg (if solarized-high-contrast-mode-line
                             base0 base02))
         (s-mode-line-underline (if solarized-high-contrast-mode-line
                                    nil s-line))

         (s-mode-line-buffer-id-fg (if solarized-high-contrast-mode-line
                                       'unspecified base1))
         (s-mode-line-inactive-fg (if solarized-high-contrast-mode-line
                                      base0 base01))
         (s-mode-line-inactive-bg (if solarized-high-contrast-mode-line
                                      base02 base03))
         (s-mode-line-inactive-bc (if solarized-high-contrast-mode-line
                                               base02 base02)))
;;; Theme Faces
    (custom-theme-set-faces
     theme-name
;;;; Built-in
;;;;; basic coloring
     `(default ((,class (:foreground ,base0 :background ,base03))))
     `(shadow ((,class (:foreground ,base01))))
     `(match ((,class (:background ,base02 :foreground ,base1 :weight bold))))
     `(cursor ((,class (:foreground ,base03 :background ,base0
                                    :inverse-video t))))
     `(escape-glyph ((,class (:foreground ,violet))))
     `(fringe ((,class (:foreground ,s-fringe-fg :background ,s-fringe-bg))))
     `(highlight ((,class (:background ,base02))))
     `(link ((,class (:foreground ,yellow :underline t :weight bold))))
     `(link-visited ((,class (:foreground ,yellow :underline t :weight normal))))
     `(success ((,class (:foreground ,green ))))
     `(warning ((,class (:foreground ,yellow ))))
     `(error ((,class (:foreground ,orange))))
     `(lazy-highlight ((,class (:foreground ,base03 :background ,yellow
                                            :weight normal))))
     `(widget-field ((,class (:background ,base02))))
     '(button ((t (:underline t))))
;;;;; compilation
     `(compilation-column-face ((,class (:foreground ,cyan :underline nil))))
     `(compilation-column-number ((,class (:inherit font-lock-doc-face :foreground ,cyan
                                                    :underline nil))))
     `(compilation-enter-directory-face ((,class (:foreground ,green :underline nil))))
     `(compilation-error ((,class (:inherit error :underline nil))))
     `(compilation-error-face ((,class (:foreground ,red : :underline nil))))
     `(compilation-face ((,class (:foreground ,base0 :underline nil))))
     `(compilation-info ((,class (:foreground ,base01 :underline nil :bold nil))))
     `(compilation-info-face ((,class (:foreground ,blue :underline nil))))
     `(compilation-leave-directory-face ((,class (:foreground ,green :underline nil))))
     `(compilation-line-face ((,class (:foreground ,green :underline nil))))
     `(compilation-line-number ((,class (:foreground ,green :underline nil))))
     `(compilation-warning ((,class (:inherit warning :underline nil))))
     `(compilation-warning-face ((,class (:foreground ,yellow :weight normal :underline nil))))

     `(compilation-mode-line-exit
       ((,class (:foreground unspecified :weight bold))))
     `(compilation-mode-line-fail
       ((,class (:inherit compilation-error :foreground ,red :weight bold))))
     `(compilation-mode-line-run ((,class (:foreground ,orange :weight bold))))
;;;;; cua
     `(cua-global-mark ((,class (:background ,yellow :foreground ,base03))))
     `(cua-rectangle ((,class (:inherit region
                                        :background ,magenta :foreground ,base03))))
     `(cua-rectangle-noselect ((,class (:inherit region :background ,base02
                                                 :foreground ,base01))))
;;;;; diary
     `(diary ((,class (:foreground ,yellow))))
;;;;; dired
     `(dired-directory ((,class (:foreground ,blue :weight normal))))
     `(dired-flagged ((,class (:foreground ,red))))
     `(dired-header ((,class (:foreground ,base03 :background ,blue))))
     `(dired-ignored ((,class (:inherit shadow))))
     `(dired-mark ((,class (:foreground ,yellow :weight bold))))
     `(dired-marked ((,class (:foreground ,magenta :weight bold))))
     `(dired-perm-write ((,class (:foreground ,base0 :underline t))))
     `(dired-symlink ((,class (:foreground ,cyan :weight normal :slant italic))))
     `(dired-warning ((,class (:foreground ,orange :underline t))))
;;;;; dired-async
     `(dired-async-message ((,class (:background ,(if (eq variant 'light) yellow-l yellow) ))))
     `(dired-async-mode-message
       ((,class (:background ,(if (eq variant 'light) red-l red) ))))
;;;;; dired-efap
     `(dired-efap-face ((,class (:box nil
                                      :background ,base02
                                      :foreground ,base1
                                      :underline ,s-line
                                      :weight bold))))
;;;;; dropdown
     `(dropdown-list-face ((,class (:background ,base02 :foreground ,cyan))))
     `(dropdown-list-selection-face ((,class (:background ,cyan-lc :foreground ,cyan-hc))))
;;;;; ecb
     `(ecb-default-highlight-face ((,class (:background ,blue :foreground ,base03))))
     `(ecb-history-bucket-node-dir-soure-path-face
       ((,class (:inherit ecb-history-bucket-node-face :foreground ,yellow))))
     `(ecb-source-in-directories-buffer-face ((,class (:inherit ecb-directories-general-face
                                                                :foreground ,base0))))
     `(ecb-history-dead-buffer-face ((,class (:inherit ecb-history-general-face
                                                       :foreground ,base01))))
     `(ecb-directory-not-accessible-face ((,class (:inherit ecb-directories-general-face
                                                            :foreground ,base01))))
     `(ecb-bucket-node-face ((,class (:inherit ecb-default-general-face :weight normal
                                               :foreground ,blue))))
     `(ecb-tag-header-face ((,class (:background ,base02))))
     `(ecb-analyse-bucket-element-face ((,class (:inherit ecb-analyse-general-face
                                                          :foreground ,green))))
     `(ecb-directories-general-face ((,class (:inherit ecb-default-general-face :height 1.0))))
     `(ecb-method-non-semantic-face ((,class (:inherit ecb-methods-general-face
                                                       :foreground ,cyan))))
     `(ecb-mode-line-prefix-face ((,class (:foreground ,green))))
     `(ecb-tree-guide-line-face ((,class (:inherit ecb-default-general-face
                                                   :foreground ,base02 :height 1.0))))
;;;;; ee
     `(ee-bookmarked ((,class (:foreground ,base1))))
     `(ee-category ((,class (:foreground ,blue))))
     `(ee-link ((,class (:inherit link))))
     `(ee-link-visited ((,class (:inherit link-visited))))
     `(ee-marked ((,class (:foreground ,magenta :weight bold))))
     `(ee-omitted ((,class (:foreground ,base01))))
     `(ee-shadow ((,class (:inherit shadow))))
;;;;; enh-ruby-mode
     `(enh-ruby-string-delimiter-face ((,class (:foreground ,yellow))))
     `(enh-ruby-heredoc-delimiter-face ((,class (:inherit enh-ruby-string-delimiter-face))))
     `(enh-ruby-regexp-delimiter-face ((,class (:inherit enh-ruby-string-delimiter-face))))
     `(enh-ruby-op-face ((,class (:foreground ,base0))))
     `(erm-syn-errline ((,class (:inherit flymake-errline))))
     `(erm-syn-warnline ((,class (:inherit flymake-warnline))))
;;;;; completions
     `(completions-annotations ((t (:foreground ,base01))))
;;;;; grep
     `(grep-context-face ((,class (:foreground ,base0))))
     `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     `(grep-hit-face ((,class (:foreground ,blue))))
     `(grep-match-face ((,class (:foreground ,orange :weight bold))))
;;;;; isearch
     `(isearch ((,class (:foreground ,base03 :background ,magenta :weight normal))))
     `(isearch-fail ((,class (:foreground ,red :background ,base03 :bold t))))
;;;;; man
     `(Man-overstrike ((,class (:foreground ,blue :weight bold))))
     `(Man-reverse ((,class (:foreground ,orange))))
     `(Man-underline ((,class (:foreground ,green :underline t))))
;;;;; misc faces
     `(menu ((,class (:foreground ,base0 :background ,base03))))
     `(minibuffer-prompt ((,class (:foreground ,base0))))
     `(mode-line
       ((,class (:foreground ,s-mode-line-fg
                 :background ,s-mode-line-bg
                 :box (:color ,s-mode-line-fg :line-width 1)
                 ))))
     ;; `(mode-line-buffer-id ((,class (:foreground ,s-mode-line-buffer-id-fg :weight bold))))
     `(mode-line-inactive
       ((,class (:foreground ,s-mode-line-inactive-fg
                 :background ,s-mode-line-inactive-bg
                 :box (:color ,s-mode-line-inactive-fg :line-width 1)
                 ))))
     ;; `(header-line
     ;;   ((,class (:inverse-video unspecified
     ;;                            :overline nil
     ;;                            :underline ,s-header-line-underline
     ;;                            :foreground ,s-header-line-fg
     ;;                            :background ,s-header-line-bg
     ;;                            :box (:line-width 2 :color ,s-header-line-bg
     ;;                                              :style unspecified)
     ;;                            ))))
     `(region ((,class (:foreground ,base03 :background ,base1))))
     `(secondary-selection ((,class (:background ,base02))))

     `(trailing-whitespace ((,class (:background ,red))))
     `(vertical-border ((,class (:foreground ,s-line))))
;;;;; font lock
     `(font-lock-builtin-face ((,class (:foreground ,base0 :weight ,s-maybe-bold
                                                    :slant ,s-maybe-italic))))
     `(font-lock-comment-delimiter-face
       ((,class (:foreground ,base01 :slant ,s-maybe-italic))))
     `(font-lock-comment-face ((,class (:foreground ,base01))))
     `(font-lock-constant-face ((,class (:foreground ,blue :weight bold))))
     `(font-lock-doc-face ((,class (:foreground ,cyan :slant ,s-maybe-italic))))
     `(font-lock-function-name-face ((,class (:foreground ,blue))))
     `(font-lock-keyword-face ((,class (:foreground ,green :weight ,s-maybe-bold))))
     `(font-lock-negation-char-face ((,class (:foreground ,yellow :weight bold))))
     `(font-lock-preprocessor-face ((,class (:foreground ,blue))))
     `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow :weight bold))))
     `(font-lock-regexp-grouping-backslash ((,class (:foreground ,green :weight bold))))
     `(font-lock-string-face ((,class (:foreground ,cyan))))
     `(font-lock-type-face ((,class (:foreground ,yellow))))
     `(font-lock-variable-name-face ((,class (:foreground ,blue))))
     `(font-lock-warning-face ((,class (:inherit error :weight bold))))
     `(c-annotation-face ((,class (:inherit font-lock-constant-face))))
;;;; Third-party
;;;;; ace-jump-mode
     `(ace-jump-face-background
       ((,class (:foreground ,base01 :background ,base03
                             :inverse-video nil))))
     `(ace-jump-face-foreground
       ((,class (:foreground ,red :background ,base03 :inverse-video nil :weight bold))))
;;;;; auctex
     `(font-latex-bold-face ((,class (:inherit bold :foreground ,base1))))
     `(font-latex-doctex-documentation-face ((,class (:background unspecified))))
     `(font-latex-doctex-preprocessor-face ((,class
                                             (:inherit (font-latex-doctex-documentation-face
                                                        font-lock-builtin-face
                                                        font-lock-preprocessor-face)))))
     `(font-latex-italic-face ((,class (:inherit italic :foreground ,base1))))
     `(font-latex-math-face ((,class (:foreground ,violet))))
     `(font-latex-sectioning-0-face ((,class (:inherit font-latex-sectioning-1-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-1-face ((,class (:inherit font-latex-sectioning-2-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-2-face ((,class (:inherit font-latex-sectioning-3-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-3-face ((,class (:inherit font-latex-sectioning-4-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-4-face ((,class (:inherit font-latex-sectioning-5-face
                                                       :height ,solarized-height-plus-1))))
     `(font-latex-sectioning-5-face ((,class (:inherit ,s-variable-pitch :foreground ,yellow
                                                       :weight bold))))
     `(font-latex-sedate-face ((,class (:foreground ,base1))))
     `(font-latex-slide-title-face ((,class (:inherit (,s-variable-pitch font-lock-type-face)
                                                      :weight bold :height ,solarized-height-plus-3))))
     `(font-latex-string-face ((,class (:foreground ,cyan))))
     `(font-latex-subscript-face ((,class (:height ,solarized-height-minus-1))))
     `(font-latex-superscript-face ((,class (:height ,solarized-height-minus-1))))
     `(font-latex-verbatim-face ((,class (:inherit fixed-pitch :foreground ,base0
                                                   :slant italic))))
     `(font-latex-warning-face ((,class (:inherit bold :foreground ,orange))))
;;;;; auto-complete
     `(ac-candidate-face ((,class (:background ,base02 :foreground ,cyan))))
     `(ac-selection-face ((,class (:background ,cyan-lc :foreground ,cyan-hc))))
     `(ac-candidate-mouse-face ((,class (:background ,cyan-hc :foreground ,cyan-lc))))
     `(ac-completion-face ((,class (:foreground ,base1 :underline t))))
     `(ac-gtags-candidate-face ((,class (:background ,base02 :foreground ,blue))))
     `(ac-gtags-selection-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
     `(ac-yasnippet-candidate-face ((,class (:background ,base02 :foreground ,yellow))))
     `(ac-yasnippet-selection-face ((,class (:background ,yellow-lc :foreground ,yellow-hc))))
;;;;; auto highlight symbol
     `(ahs-definition-face ((,class (:foreground ,magenta :background unspecified :underline t
                                                 :slant normal :weight bold))))
     `(ahs-edit-mode-face ((,class (:foreground ,base03 :background ,magenta :weight bold))))
     `(ahs-face ((,class (:foreground ,magenta :background unspecified :weight bold))))
     `(ahs-plugin-bod-face ((,class (:foreground ,magenta :background unspecified :weight bold))))
     `(ahs-plugin-defalt-face ((,class (:foreground ,magenta :background unspecified :weight bold))))
     `(ahs-plugin-whole-buffer-face ((,class (:foreground ,magenta  :background unspecified :weight bold))))
     `(ahs-warning-face ((,class (:foreground ,red :weight bold))))
;;;;; android mode
     `(android-mode-debug-face ((,class (:foreground ,green))))
     `(android-mode-error-face ((,class (:foreground ,orange :weight bold))))
     `(android-mode-info-face ((,class (:foreground ,base0))))
     `(android-mode-verbose-face ((,class (:foreground ,base01))))
     `(android-mode-warning-face ((,class (:foreground ,yellow))))
;;;;; anzu-mode
     `(anzu-mode-line ((,class (:foreground ,yellow :weight bold))))
;;;;; bm
     `(bm-face ((,class (:overline ,base0))))
     `(bm-fringe-face ((,class (:overline ,base0))))
     `(bm-fringe-persistent-face ((,class (:overline ,base0))))
     `(bm-persistent-face ((,class (:overline ,base0))))
;;;;; calfw
     `(cfw:face-day-title ((,class (:background ,base02))))
     `(cfw:face-annotation ((,class (:inherit cfw:face-day-title :foreground ,yellow))))
     `(cfw:face-default-content ((,class (:foreground ,green))))
     `(cfw:face-default-day ((,class (:inherit cfw:face-day-title :weight bold))))
     `(cfw:face-disable ((,class (:inherit cfw:face-day-title
                                           :foreground ,base01))))
     `(cfw:face-grid ((,class (:foreground ,base01))))
     `(cfw:face-header ((,class (:foreground ,blue-hc :background ,blue-lc :weight bold))))
     `(cfw:face-holiday ((,class (:background nil :foreground ,red :weight bold))))
     `(cfw:face-periods ((,class (:foreground ,magenta))))
     `(cfw:face-select ((,class (:background ,magenta-lc :foreground ,magenta-hc))))
     `(cfw:face-saturday ((,class (:foreground ,cyan-hc :background ,cyan-lc))))
     `(cfw:face-sunday ((,class (:foreground ,red-hc :background ,red-lc :weight bold))))
     `(cfw:face-title ((,class (:inherit ,s-variable-pitch :foreground ,yellow
                                         :weight bold :height ,solarized-height-plus-4))))
     `(cfw:face-today ((,class (:weight bold :background ,base02 :foreground nil))))
     `(cfw:face-today-title ((,class (:background ,yellow-lc
                                                  :foreground ,yellow-hc :weight bold))))
     `(cfw:face-toolbar ((,class (:background ,base02 :foreground ,base0))))
     `(cfw:face-toolbar-button-off ((,class (:background ,yellow-lc :foreground ,yellow-hc
                                                         :weight bold))))
     `(cfw:face-toolbar-button-on ((,class (:background ,yellow-hc :foreground ,yellow-lc
                                                        :weight bold))))
;;;;; clojure-test-mode
     `(clojure-test-failure-face ((t (:foreground ,orange :weight bold :underline t))))
     `(clojure-test-error-face ((t (:foreground ,red :weight bold :underline t))))
     `(clojure-test-success-face ((t (:foreground ,green :weight bold :underline t))))
;;;;; cider-repl-mode
     `(cider-repl-err-output-face ((t (:inherit ,font-lock-warning-face :underline nil))))
;;;;; cider-test-mode
     `(cider-test-failure-face ((t (:foreground ,orange :weight bold :underline t))))
     `(cider-test-error-face ((t (:foreground ,red :weight bold :underline t))))
     `(cider-test-success-face ((t (:foreground ,green :weight bold :underline t))))
;;;;; company-mode
     `(company-template-field ((,class (:background ,yellow :foreground ,base02))))
     `(company-tooltip ((,class (:background ,base02 :foreground ,cyan))))
     `(company-tooltip-selection ((,class (:background ,cyan-lc :foreground ,cyan-hc))))
     `(company-tooltip-mouse ((,class (:background ,cyan-hc :foreground ,cyan-lc))))
     `(company-tooltip-common ((,class (:foreground ,base1 :underline t))))
     `(company-tooltip-common-selection ((,class (:foreground ,base1 :underline t))))
     `(company-tooltip-annotation ((,class (:foreground ,base1 :background ,base02))))
     `(company-scrollbar-fg ((,class (:foreground ,base03 :background ,base0))))
     `(company-scrollbar-bg ((,class (:background ,base02 :foreground ,cyan))))
     `(company-preview ((,class (:background ,base02 :foreground ,cyan))))
     `(company-preview-common ((,class (:foreground ,base1 :underline t))))
;;;;; column-enforce-mode
     `(column-enforce-face ((,class (:background unspecified :foreground ,magenta
                                                 :inverse-video unspecified))))
;;;;; cscope
     `(cscope-file-face ((,class (:foreground ,green :weight bold))))
     `(cscope-function-face ((,class (:foreground ,blue))))
     `(cscope-line-number-face ((,class (:foreground ,yellow))))
     `(cscope-line-face ((,class (:foreground ,base0))))
     `(cscope-mouse-face ((,class (:background ,blue :foreground ,base0))))
;;;;; ctable
     `(ctbl:face-cell-select ((,class (:background ,base02 :foreground ,base1
                                                   :underline ,base1 :weight bold))))
     `(ctbl:face-continue-bar ((,class (:background ,base02 :foreground ,yellow))))
     `(ctbl:face-row-select ((,class (:background ,base02 :foreground ,base0
                                                  :underline t))))
;;;;; coffee
     `(coffee-mode-class-name ((,class (:foreground ,yellow :weight bold))))
     `(coffee-mode-function-param ((,class (:foreground ,violet :slant italic))))

;;;;; cperl-mode
     `(cperl-array-face ((,class (:background unspecified :foreground ,blue))))
     `(cperl-hash-face ((,class (:background unspecified :foreground ,blue))))
     `(cperl-nonoverridable-face ((,class (:foreground ,base0 :weight bold))))

;;;;; custom
     `(custom-face-tag ((,class (:inherit ,s-variable-pitch :height ,solarized-height-plus-3
                                          :foreground ,violet :weight normal))))
     `(custom-variable-tag ((,class (:inherit ,s-variable-pitch
                                              :foreground ,cyan :height ,solarized-height-plus-3))))
     `(custom-comment-tag ((,class (:foreground ,base01))))
     `(custom-group-tag ((,class (:inherit ,s-variable-pitch :foreground ,blue :height ,solarized-height-plus-3))))
     `(custom-group-tag-1 ((,class (:inherit ,s-variable-pitch :foreground ,red :height ,solarized-height-plus-3))))
     `(custom-state ((,class (:foreground ,green))))
     `(custom-button ((,class (:background ,base02 :foreground ,base1
                                           :box (:line-width 2 :style released-button)))))
     `(custom-button-mouse ((,class (:background ,base01 :foreground ,base02
                                                 :box (:line-width 2 :style released-button)))))
     `(custom-button-pressed ((,class (:background ,base01 :foreground ,base1
                                                   :box (:line-width 2 :style pressed-button)))))
     `(custom-button-unraised ((,class (:inherit underline))))
     `(custom-button-pressed-unraised ((,class (:inherit custom-button-unraised :foreground ,magenta))))
;;;;; diff
     `(diff-added ((,class (:foreground ,green :background nil))))
     `(diff-changed ((,class (:foreground ,blue :background nil))))
     `(diff-removed ((,class (:foreground ,red :background nil))))
     `(diff-header ((,class (:background ,base03))))
     `(diff-file-header
       ((,class (:background ,base03 :foreground ,base0 :weight bold))))
     `(diff-refine-added ((,class (:foreground ,green :background ,base03
                                               :inverse-video t))))
     `(diff-refine-change ((,class (:foreground ,blue :background ,base03
                                                :inverse-video t))))
     `(diff-refine-removed ((,class (:foreground ,red :background ,base03
                                                 :inverse-video t))))
;;;;; ediff
     `(ediff-fine-diff-A ((,class (:background ,orange-lc))))
     `(ediff-fine-diff-B ((,class (:background ,green-lc))))
     `(ediff-fine-diff-C ((,class (:background ,yellow-lc))))

     `(ediff-current-diff-C ((,class (:background ,blue-lc))))

     `(ediff-even-diff-A ((,class (:background ,base01
                                               :foreground ,base3 ))))
     `(ediff-odd-diff-A ((,class (:background ,base01
                                              :foreground ,base03 ))))
     `(ediff-even-diff-B ((,class (:background ,base01
                                               :foreground ,base03 ))))
     `(ediff-odd-diff-B ((,class (:background ,base01
                                              :foreground ,base03 ))))
     `(ediff-even-diff-C ((,class (:background ,base01
                                               :foreground ,base0 ))))
     `(ediff-odd-diff-C ((,class (:background ,base01
                                              :foreground ,base03 ))))

;;;;;; alternative ediff (not finished)
     ;; `(ediff-fine-diff-A ((,class (
     ;;                               :background ,(solarized-color-blend blue base03 0.25))
     ;;                              )))
     ;; `(ediff-fine-diff-B ((,class (
     ;;                               :background ,(solarized-color-blend violet base03 0.25))
     ;;                              )))
     ;; `(ediff-fine-diff-C ((,class (
     ;;                               :background ,(solarized-color-blend yellow base03 0.25))
     ;;                              )))
     ;; `(ediff-current-diff-A ((,class (
     ;;                                  :background ,(solarized-color-blend blue base03 0.15)
     ;;                                              ))))
     ;; `(ediff-current-diff-B ((,class (
     ;;                                   :background ,(solarized-color-blend violet base03 0.15)
     ;;                                              ))))
     ;; `(ediff-current-diff-C ((,class (
     ;;                                  :background ,(solarized-color-blend yellow base03 0.15)
     ;;                                              ))))
     ;; `(ediff-even-diff-A ((,class (
     ;;                                ;; :background ,(solarized-color-blend base0 base03 0.15)
     ;;                               :background ,base02
     ;;                               ;; :foreground ,base2
     ;;                                ;; :background ,(solarized-color-blend green base02 0.15)
     ;;                                           ))))
     ;; `(ediff-even-diff-B ((,class (
     ;;                               ;; :background ,base01
     ;;                               :background ,base02
     ;;                               ;; :foreground ,base2
     ;;                                           ))))
     ;; `(ediff-even-diff-C ((,class (
     ;;                               ;; :background ,base01
     ;;                               :background ,base02
     ;;                                           ;; :foreground ,base2
     ;;                                           ))))
     ;; `(ediff-odd-diff-A ((,class (
     ;;                              ;; :background ,base01
     ;;                                          :background ,base02
     ;;                                          ))))
     ;; `(ediff-odd-diff-B ((,class (
     ;;                              ;; :background ,base01
     ;;                                          :background ,base02
     ;;                                          ))))
     ;; `(ediff-odd-diff-C ((,class (
     ;;                              ;; :background ,base01
     ;;                                          :background ,base03
     ;;                                          ))))
     ;; `(ediff-current-diff-Ancestor ((,class (:background "VioletRed" :foreground "Black"))))
     ;; `(ediff-even-diff-Ancestor ((,class (:background "Grey" :foreground "White"))))
     ;; `(ediff-fine-diff-Ancestor ((,class (:background "Green" :foreground "Black"))))
     ;; `(ediff-odd-diff-Ancestor ((,class (:background "gray40" :foreground "cyan3"))))
     ;; `(ediff-even-diff-A ((,class (:underline ,base01))))
     ;; `(ediff-odd-diff-A ((,class (:underline ,base01
     ;;                                          ))))
     ;; `(ediff-even-diff-B ((,class (:background ,base01
     ;;                                           :foreground ,base03
     ;;                                           ))))
     ;; `(ediff-odd-diff-B ((,class (:background ,base01
     ;;                                          :foreground ,base03
     ;;                                          ))))
     ;; `(ediff-even-diff-C ((,class (:background ,base01
     ;;                                           :foreground ,base0
     ;;                                           ))))
     ;; `(ediff-odd-diff-C ((,class (:background ,base01
     ;;                                          :foreground ,base03
     ;;                                          ))))
;;;;; diff-hl
     `(diff-hl-change ((,class (:background ,blue-lc  :foreground ,blue-hc))))
     `(diff-hl-delete ((,class (:background ,red-lc  :foreground ,red-hc))))
     `(diff-hl-insert ((,class (:background ,green-lc  :foreground ,green-hc))))
     `(diff-hl-unknown ((,class (:background ,cyan-lc   :foreground ,cyan-hc))))
;;;;; edts
     `(edts-face-error-line
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,(if (eq variant 'light) red-l red)) :inherit unspecified))
        (,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     `(edts-face-warning-line
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,(if (eq variant 'light) yellow-l yellow)) :inherit unspecified))
        (,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
     `(edts-face-error-fringe-bitmap
       ((,class (:foreground ,(if (eq variant 'light) red-l red) :background unspecified :weight bold))))
     `(edts-face-warning-fringe-bitmap
       ((,class (:foreground ,(if (eq variant 'light) yellow-l yellow) :background unspecified :weight bold))))
     `(edts-face-error-mode-line
       ((,class (:background ,(if (eq variant 'light) red-l red) :foreground unspecified))))
     `(edts-face-warning-mode-line
       ((,class (:background ,(if (eq variant 'light) yellow-l yellow) :foreground unspecified))))
;;;;; elfeed
     `(elfeed-search-date-face ((,class (:foreground ,base01))))
     `(elfeed-search-feed-face ((,class (:foreground ,base01))))
     `(elfeed-search-tag-face ((,class (:foreground ,base0))))
     `(elfeed-search-title-face ((,class (:foreground ,base0))))

;;;;; elscreen
     `(elscreen-tab-background-face ((,class (:background ,base03))))
     `(elscreen-tab-current-screen-face ((,class (:background ,base1 :foreground ,base03)) (t (:underline t))))
     `(elscreen-tab-other-screen-face ((,class (:background ,base02 :foreground ,base01))))
     `(elscreen-tab-control-face ((,class (:background ,base03 :foreground ,base0))))
;;;;; epa
     `(epa-mark ((,class (:foreground ,magenta :weight bold))))
     `(epa-string ((,class (:foreground ,violet))))
     `(epa-validity-disabled ((,class (:inverse-video t :slant italic))))
     `(epa-validity-high ((,class (:weight bold))))
     `(epa-validity-low ((,class (:slant italic))))
     `(epa-validity-medium ((,class (:slant italic))))
;;;;; epc
     `(epc:face-title ((,class (:foreground ,blue :background ,base03
                                            :weight normal :underline nil))))
;;;;; eshell
     `(eshell-prompt ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-archive ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-directory ((,class (:foreground ,blue :weight bold))))
     `(eshell-ls-executable ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-unreadable ((,class (:foreground ,base0))))
     `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
     `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
     `(eshell-ls-special ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-symlink ((,class (:foreground ,cyan :weight bold))))
;;;;; evil-search-highlight-persist
     `(evil-search-highlight-persist-highlight-face ((,class (:background ,(if (eq variant 'light) green-lc violet-lc)))))
;;;;; fic
     `(fic-author-face ((,class (:background ,base03 :foreground ,orange
                                             :underline t :slant italic))))
     `(fic-face ((,class (:background ,base03 :foreground ,orange
                                      :weight normal :slant italic))))
     `(font-lock-fic-face ((,class (:background ,base03 :foreground ,orange
                                      :weight normal :slant italic))))
;;;;; fixmee
     `(fixmee-notice-face ((,class (:background nil :foreground ,base1
						:underline nil :slant italic :weight bold))))

;;;;; flx
     `(flx-highlight-face ((,class (:foreground ,blue
                                                :weight normal :underline nil))))
;;;;; flymake
     `(flymake-errline
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,red) :inherit unspecified
                     :foreground unspecified :background unspecified))
        (,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     `(flymake-infoline
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,green) :inherit unspecified
                     :foreground unspecified :background unspecified))
        (,class (:foreground ,green-hc :background ,green-lc))))
     `(flymake-warnline
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,yellow) :inherit unspecified
                     :foreground unspecified :background unspecified))
        (,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
;;;;; flycheck
     `(flycheck-error
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,(if (eq variant 'light) red-l red)) :inherit unspecified))
        (,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     `(flycheck-warning
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,(if (eq variant 'light) yellow-l yellow)) :inherit unspecified))
        (,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
     `(flycheck-info
       ((,(append '((supports :underline (:style line))) class)
         (:underline (:style line :color ,(if (eq variant 'light) blue-l blue)) :inherit unspecified))
        (,class (:foreground ,blue-hc :background ,blue-lc :weight bold :underline t))))
     `(flycheck-fringe-error
       ((,class (:foreground ,(if (eq variant 'light) red-l red) :background unspecified :weight bold))))
     `(flycheck-fringe-warning
       ((,class (:foreground ,(if (eq variant 'light) yellow-l yellow) :background unspecified :weight bold))))
     `(flycheck-fringe-info
       ((,class (:foreground ,(if solarized-emphasize-indicators
                                  blue-hc base01)
                             :background ,(if solarized-emphasize-indicators
                                              blue-lc base03) :weight bold))))
;;;;; flyspell
     `(flyspell-duplicate
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,yellow) :inherit unspecified))
        (,class (:foreground ,yellow :weight bold :underline t))))
     `(flyspell-incorrect
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,red) :inherit unspecified))
        (,class (:foreground ,red :weight bold :underline t))))
;;;;; erc
     `(erc-action-face ((,class (:inherit erc-default-face))))
     `(erc-bold-face ((,class (:weight bold))))
     `(erc-current-nick-face ((,class (:foreground ,blue :weight bold))))
     `(erc-dangerous-host-face ((,class (:inherit font-lock-warning-face))))
     `(erc-default-face ((,class (:foreground ,base0))))
     `(erc-highlight-face ((,class (:inherit erc-default-face
                                             :background ,base02))))
     `(erc-direct-msg-face ((,class (:inherit erc-default-face))))
     `(erc-error-face ((,class (:inherit font-lock-warning-face))))
     `(erc-fool-face ((,class (:inherit erc-default-face))))
     `(erc-input-face ((,class (:foreground ,yellow))))
     `(erc-keyword-face ((,class (:foreground ,blue :weight bold))))
     `(erc-nick-default-face ((,class (:foreground ,yellow :weight bold))))
     `(erc-my-nick-face ((,class (:foreground ,red :weight bold))))
     `(erc-nick-msg-face ((,class (:inherit erc-default-face))))
     `(erc-notice-face ((,class (:foreground ,green))))
     `(erc-pal-face ((,class (:foreground ,orange :weight bold))))
     `(erc-prompt-face ((,class (:foreground ,orange :background ,base03 :weight bold))))
     `(erc-timestamp-face ((,class (:foreground ,green))))
     `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
     `(git-gutter:added
         ((,class (:weight normal
                           :foreground ,(if solarized-emphasize-indicators
                                            green s-fringe-fg)
                         :background ,s-fringe-bg
                         ))))
     `(git-gutter:deleted
         ((,class (:weight normal
                           :foreground ,(if solarized-emphasize-indicators
                                            red s-fringe-fg)
                         :background ,s-fringe-bg
                         ))))
     `(git-gutter:modified
       ((,class (:weight normal
                         :foreground ,(if solarized-emphasize-indicators
                                          blue s-fringe-fg)
                         :background ,s-fringe-bg
                         ))))
     `(git-gutter:unchanged
       ((,class (:weight normal
                         :foreground ,(if solarized-emphasize-indicators
                                          base01 s-fringe-fg)
                         :background ,s-fringe-bg
                         ))))
;;;;; git-gutter-fr
     `(git-gutter-fr:added ((,class (:foreground ,green  :weight bold))))
     `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))
     `(git-gutter-fr:modified ((,class (:foreground ,blue :weight bold))))
;;;;; git-gutter+ and git-gutter+-fr
     `(git-gutter+-added ((,class (:background ,green :foreground ,base03
                                               :weight bold))))
     `(git-gutter+-deleted ((,class (:background ,red :foreground ,base03
                                                 :weight bold))))
     `(git-gutter+-modified ((,class (:background ,blue :foreground ,base03
                                                  :weight bold))))
     `(git-gutter+-unchanged ((,class (:background ,base02
                                                   :foreground ,base03
                                                   :weight bold))))
     `(git-gutter-fr+-added ((,class (:foreground ,green :weight bold))))
     `(git-gutter-fr+-deleted ((,class (:foreground ,red :weight bold))))
     `(git-gutter-fr+-modified ((,class (:foreground ,blue :weight bold))))
;;;;; go-direx
     `(go-direx-header ((,class (:foreground ,blue))))
     `(go-direx-label ((,class (:foreground ,green))))
     `(go-direx-package ((,class (:foreground ,base1 :weight bold))))
;;;;; guide-key
     `(guide-key/highlight-command-face ((,class (:foreground ,blue))))
     `(guide-key/key-face ((,class (:foreground ,base01))))
     `(guide-key/prefix-command-face ((,class (:foreground ,green))))
;;;;; gnus
     `(gnus-group-mail-1 ((,class (:weight bold :inherit gnus-group-mail-1-empty))))
     `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
     `(gnus-group-mail-2 ((,class (:weight bold :inherit gnus-group-mail-2-empty))))
     `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
     `(gnus-group-mail-3 ((,class (:weight bold :inherit gnus-group-mail-3-empty))))
     `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
     `(gnus-group-mail-low ((,class (:weight bold :inherit gnus-group-mail-low-empty))))
     `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
     `(gnus-group-news-1 ((,class (:weight bold :inherit gnus-group-news-1-empty))))
     `(gnus-group-news-2 ((,class (:weight bold :inherit gnus-group-news-2-empty))))
     `(gnus-group-news-3 ((,class (:weight bold :inherit gnus-group-news-3-empty))))
     `(gnus-group-news-4 ((,class (:weight bold :inherit gnus-group-news-4-empty))))
     `(gnus-group-news-5 ((,class (:weight bold :inherit gnus-group-news-5-empty))))
     `(gnus-group-news-6 ((,class (:weight bold :inherit gnus-group-news-6-empty))))
     `(gnus-group-news-low ((,class (:weight bold :inherit gnus-group-news-low-empty))))
     `(gnus-header-content ((,class (:inherit message-header-other))))
     `(gnus-header-from ((,class (:inherit message-header-other))))
     `(gnus-header-name ((,class (:inherit message-header-name))))
     `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
     `(gnus-header-subject ((,class (:inherit message-header-subject))))
     `(gnus-summary-cancelled ((,class (:foreground ,orange))))
     `(gnus-summary-high-ancient ((,class (:foreground ,blue :weight bold))))
     `(gnus-summary-high-read ((,class (:foreground ,green :weight bold))))
     `(gnus-summary-high-ticked ((,class (:foreground ,orange :weight bold))))
     `(gnus-summary-high-unread ((,class (:foreground ,base0 :weight bold))))
     `(gnus-summary-low-ancient ((,class (:foreground ,blue))))
     `(gnus-summary-low-read ((t (:foreground ,green))))
     `(gnus-summary-low-ticked ((,class (:foreground ,orange))))
     `(gnus-summary-low-unread ((,class (:foreground ,base0))))
     `(gnus-summary-normal-ancient ((,class (:foreground ,blue))))
     `(gnus-summary-normal-read ((,class (:foreground ,green))))
     `(gnus-summary-normal-ticked ((,class (:foreground ,orange))))
     `(gnus-summary-normal-unread ((,class (:foreground ,base0))))
     `(gnus-summary-selected ((,class (:foreground ,yellow :weight bold))))
     `(gnus-cite-1 ((,class (:foreground ,blue))))
     `(gnus-cite-2 ((,class (:foreground ,blue))))
     `(gnus-cite-3 ((,class (:foreground ,blue))))
     `(gnus-cite-4 ((,class (:foreground ,green))))
     `(gnus-cite-5 ((,class (:foreground ,green))))
     `(gnus-cite-6 ((,class (:foreground ,green))))
     `(gnus-cite-7 ((,class (:foreground ,red))))
     `(gnus-cite-8 ((,class (:foreground ,red))))
     `(gnus-cite-9 ((,class (:foreground ,red))))
     `(gnus-cite-10 ((,class (:foreground ,yellow))))
     `(gnus-cite-11 ((,class (:foreground ,yellow))))
     `(gnus-group-news-1-empty ((,class (:foreground ,yellow))))
     `(gnus-group-news-2-empty ((,class (:foreground ,green))))
     `(gnus-group-news-3-empty ((,class (:foreground ,green))))
     `(gnus-group-news-4-empty ((,class (:foreground ,blue))))
     `(gnus-group-news-5-empty ((,class (:foreground ,blue))))
     `(gnus-group-news-6-empty ((,class (:foreground ,blue-lc))))
     `(gnus-group-news-low-empty ((,class (:foreground ,base01))))
     `(gnus-signature ((,class (:foreground ,yellow))))
     `(gnus-x-face ((,class (:background ,base0 :foreground ,base03))))
;;;;; helm
     ;; These probably needs tweaking.
     `(helm-apt-deinstalled ((,class (:foreground ,base01))))
     `(helm-apt-installed ((,class (:foreground ,green))))
     `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((,class (:foreground ,base0))))
     `(helm-bookmark-gnus ((,class (:foreground ,cyan))))
     `(helm-bookmark-info ((,class (:foreground ,green))))
     `(helm-bookmark-man ((,class (:foreground ,violet))))
     `(helm-bookmark-w3m ((,class (:foreground ,yellow))))
     `(helm-bookmarks-su ((,class (:foreground ,orange))))
     `(helm-buffer-not-saved ((,class (:foreground ,orange))))
     `(helm-buffer-saved-out ((,class (:foreground ,red :background ,base03
                                                   :inverse-video t))))
     `(helm-buffer-size ((,class (:foreground ,base01))))
     `(helm-candidate-number ((,class (:background ,base02 :foreground ,base1
                                                   :bold t))))
     `(helm-ff-directory ((,class (:background ,base03  :foreground ,blue))))
     `(helm-ff-executable ((,class (:foreground ,green))))
     `(helm-ff-file ((,class (:background ,base03 :foreground ,base0))))
     `(helm-ff-invalid-symlink ((,class (:background ,base03 :foreground ,orange
                                                     :slant italic))))
     `(helm-ff-prefix ((,class (:background ,yellow :foreground ,base03))))
     `(helm-ff-symlink ((,class (:foreground ,cyan))))
     `(helm-grep-file ((,class (:foreground ,cyan :underline t))))
     `(helm-grep-finish ((,class (:foreground ,green))))
     `(helm-grep-lineno ((,class (:foreground ,orange))))
     `(helm-grep-match ((,class (:inherit match))))
     `(helm-grep-running ((,class (:foreground ,red))))
     `(helm-header ((,class (:inherit header-line))))
     `(helm-lisp-completion-info ((,class (:foreground ,base0))))
     `(helm-lisp-show-completion ((,class (:foreground ,yellow  :background ,base02
                                                       :bold t))))
     `(helm-M-x-key ((,class (:foreground ,orange :underline t))))
     `(helm-moccur-buffer ((,class (:foreground ,cyan :underline t))))
     `(helm-match ((,class (:inherit match))))
     `(helm-selection ((,class (:background ,base02 :underline t))))
     `(helm-selection-line ((,class (:background ,base02 :foreground ,base1
                                                 :underline nil))))
     `(helm-separator ((,class (:foreground ,red))))
     `(helm-source-header ((,class (:background ,blue-lc :foreground ,base03
                                                :underline nil))))
     `(helm-time-zone-current ((,class (:foreground ,green))))
     `(helm-time-zone-home ((,class (:foreground ,red))))
     `(helm-visible-mark ((,class (:background ,base03 :foreground ,magenta :bold t))))
;;;;; helm-css-scss
     `(helm-css-scss-selector-depth-face-1 ((,class (:foreground ,base0))))
     `(helm-css-scss-selector-depth-face-2 ((,class (:foreground ,blue))))
     `(helm-css-scss-selector-depth-face-3 ((,class (:foreground ,cyan))))
     `(helm-css-scss-selector-depth-face-4 ((,class (:foreground ,green))))
     `(helm-css-scss-selector-depth-face-5 ((,class (:foreground ,yellow))))
     `(helm-css-scss-selector-depth-face-6 ((,class (:foreground ,violet))))
     `(helm-css-scss-target-line-face ((,class (:background unspecified :foreground ,magenta))))
;;;;; helm-go-package
     `(helm-source-go-package-godoc-description ((,class (:foreground ,base01))))
;;;;; helm-swoop
     `(helm-swoop-target-line-face ((,class (:foreground unspecified :background ,base02))))
     `(helm-swoop-target-line-block-face ((,class (:foreground unspecified :background ,base02))))
     `(helm-swoop-target-word-face ((,class (:foreground ,magenta :background unspecified))))
;;;;; hi-lock-mode
     `(hi-yellow ((,class (:foreground ,(solarized-color-blend yellow base1 0.5)
                                       :background,(solarized-color-blend yellow base03 0.15)))))
     `(hi-pink ((,class (:foreground ,(solarized-color-blend magenta base1 0.5)
                                       :background,(solarized-color-blend magenta base03 0.15)))))
     `(hi-green ((,class (:foreground ,(solarized-color-blend green base1 0.5)
                                       :background,(solarized-color-blend green base03 0.15)))))
     `(hi-blue ((,class (:foreground ,(solarized-color-blend blue base1 0.5)
                                       :background,(solarized-color-blend blue base03 0.15)))))
     `(hi-black-b ((,class (:foreground ,base1
                                        :background ,base03
                                        :weight bold))))
     `(hi-blue-b ((,class (:weight bold
                                   :foreground ,(solarized-color-blend cyan base1 0.7)
                                   :background ,(solarized-color-blend cyan base03 0.2)))))
     `(hi-green-b ((,class (:weight bold
                           :foreground ,(solarized-color-blend green base1 0.7)
                           :background ,(solarized-color-blend green base03 0.2)))))
     `(hi-red-b ((,class (:weight bold
                                  :foreground ,(solarized-color-blend red base1 0.7)
                                  :background ,(solarized-color-blend red base03 0.2)))))
     `(hi-black-hb ((,class (:weight bold
                                     :foreground ,base1
                                     :background ,base02))))
;;;;; highlight-changes
     `(highlight-changes ((,class (:foreground ,orange))))
     `(highlight-changes-delete ((,class (:foreground ,red :underline t))))
;;;;; highlight-indentation
     `(highlight-indentation-face ((,class (:background ,base02))))
     `(highlight-indentation-current-column-face((,class (:background ,base02))))
;;;;; highlight-symbol
     `(highlight-symbol-face ((,class (:foreground ,magenta))))
;;;;; hl-line-mode
     `(hl-line ((,class (:background ,base02))))
     `(hl-line-face ((,class (:background ,base02))))
;;;;; ido-mode
     `(ido-first-match ((,class (:foreground ,yellow :weight normal))))
     `(ido-only-match ((,class (:foreground ,base03 :background ,yellow :weight normal))))
     `(ido-subdir ((,class (:foreground ,blue))))
     `(ido-incomplete-regexp ((,class (:foreground ,red :weight bold ))))
     `(ido-indicator ((,class (:background ,red :foreground ,base03 :width condensed))))
     `(ido-virtual ((,class (:foreground ,cyan))))
;;;;; info
     `(info-title-1 ((,class (:foreground ,base1 :weight bold))))
     `(info-title-2 ((,class (:foreground ,base1 :weight bold))))
     `(info-title-3 ((,class (:weight bold))))
     `(info-title-4 ((,class (:weight bold))))
     `(info-node ((,class (:foreground ,base1 :slant italic :weight bold))))
     `(info-header-node ((,class (:inherit info-node))))
     `(info-header-xref ((,class (:inherit info-xref))))
     `(info-index-match ((,class (:inherit match))))
     `(info-menu-header ((,class (:inherit variable-pitch :weight bold))))
     `(info-menu-star ((,class (:foreground ,orange))))
     `(info-xref ((,class (:inherit link))))
     `(info-xref-visited ((,class (:inherit (link-visited info-xref)))))
;;;;; info+
     `(info-file
       ((,class (:foreground ,yellow :background ,base02))))
     `(info-menu
       ((,class (:foreground ,violet :background ,base02))))
     `(info-single-quote
       ((,class (:foreground ,cyan :inherit font-lock-string-face))))
     `(info-quoted-name
       ((,class (:foreground ,orange :inherit font-lock-string-face))))
     `(info-string
       ((,class (:foreground ,blue :inherit font-lock-string-face))))
     `(info-command-ref-item
       ((,class (:foreground ,green :background ,base02))))
     `(info-constant-ref-item
       ((,class (:foreground ,red :background ,base02))))
     `(info-function-ref-item
       ((,class (:foreground ,cyan :background ,base02))))
     `(info-macro-ref-item
       ((,class (:foreground ,green :background ,base02))))
     `(info-reference-item
       ((,class (:background ,base02))))
     `(info-special-form-ref-item
       ((,class (:foreground ,magenta :background ,base02))))
     `(info-syntax-class-item
       ((,class (:foreground ,magenta :background ,base02))))
     `(info-user-option-ref-item
       ((,class (:foreground ,orange :background ,base02))))
;;;;; jabber
     `(jabber-activity-face ((,class (:weight bold :foreground ,red))))
     `(jabber-activity-personal-face ((,class (:weight bold :foreground ,blue))))
     `(jabber-chat-error ((,class (:weight bold :foreground ,red))))
     `(jabber-chat-prompt-foreign ((,class (:weight bold :foreground ,red))))
     `(jabber-chat-prompt-local ((,class (:weight bold :foreground ,blue))))
     `(jabber-chat-prompt-system ((,class (:weight bold :foreground ,green))))
     `(jabber-chat-text-foreign ((,class (:foreground ,base1))))
     `(jabber-chat-text-local ((,class (:foreground ,base0))))
     `(jabber-chat-rare-time-face ((,class (:underline t :foreground ,green))))
     `(jabber-roster-user-away ((,class (:slant italic :foreground ,green))))
     `(jabber-roster-user-chatty ((,class (:weight bold :foreground ,orange))))
     `(jabber-roster-user-dnd ((,class (:slant italic :foreground ,red))))
     `(jabber-roster-user-error ((,class (:weight light :slant italic :foreground ,red))))
     `(jabber-roster-user-offline ((,class (:foreground ,base01))))
     `(jabber-roster-user-online ((,class (:weight bold :foreground ,blue))))
     `(jabber-roster-user-xa ((,class (:slant italic :foreground ,magenta))))
;;;;; js2-mode colors
     `(js2-error ((,class (:foreground ,red))))
     `(js2-external-variable ((,class (:foreground ,orange))))
     `(js2-function-param ((,class (:foreground ,green))))
     `(js2-instance-member ((,class (:foreground ,magenta))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,cyan))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,orange))))
     `(js2-jsdoc-tag ((,class (:foreground ,cyan))))
     `(js2-jsdoc-type ((,class (:foreground ,blue))))
     `(js2-jsdoc-value ((,class (:foreground ,violet))))
     `(js2-magic-paren ((,class (:underline t))))
     `(js2-private-function-call ((,class (:foreground ,yellow))))
     `(js2-private-member ((,class (:foreground ,blue))))
     `(js2-warning ((,class (:underline ,orange))))
;;;;; jedi
     `(jedi:highlight-function-argument ((,class (:inherit bold))))
;;;;; kite
     ;; Sadly kite is not very stable for me so these faces might miss out things.
     `(bg:kite-dataReceived ((,class (:background ,magenta))))
     `(bg:kite-receiveHeadersEnd ((,class (:background ,green))))
     `(bg:kite-requestStart ((,class (:background ,red))))
     `(bg:kite-sendEnd ((,class (:background ,cyan))))
     `(bg:kite-table-head ((,class (:background ,base02))))
     `(bg:kite-tick ((,class (:background ,base02))))
     `(kite-css-computed-proprietary-unused-property ((,class (:inherit kite-css-proprietary-property :foreground ,blue))))
     `(kite-css-computed-unused-property ((,class (:inherit kite-css-property :foreground ,blue))))
     `(kite-css-value-widget-error ((,class (:background ,orange-lc :foreground ,orange-hc))))
     `(kite-css-value-widget-modified ((,class (:background ,base02 :foreground ,yellow))))
     `(kite-delimited-data-face ((,class (:foreground ,green))))
     `(kite-delimiter-face ((,class (:foreground ,base1))))
     `(kite-modified-attribute-local-name-face ((,class (:inherit kite-attribute-local-name-face :background ,base02))))
     `(kite-modified-attribute-value-face ((,class (:inherit kite-attribute-value-face :background ,base02))))
     `(kite-modified-element-local-name-face ((,class (:inherit kite-element-local-name-face :background ,base02))))
     `(kite-name-face ((,class (:foreground ,blue))))
     `(kite-proto-property-name ((,class (:inherit default :foreground ,base02))))
     `(kite-ref-face ((,class (:foreground ,cyan))))
     `(kite-session-closed ((,class (:inherit default :background ,red))))
     `(kite-text-face ((,class (:background nil :foreground ,base01))))
     `(kite-node-highlight-face ((,class (:background ,base02))))
     `(bg:kite-pageStart ((,class nil)))
     `(kite-attribute-colon-face ((,class (:inherit kite-name-face))))
     `(kite-attribute-local-name-face ((,class (:inherit kite-name-face))))
     `(kite-attribute-prefix-face ((,class (:inherit kite-name-face))))
     `(kite-attribute-value-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-attribute-value-face ((,class (:inherit kite-delimited-data-face))))
     `(kite-boolean ((,class (:inherit nxml-char-ref-number))))
     `(kite-cdata-section-CDATA-face ((,class (:inherit kite-name-face))))
     `(kite-cdata-section-content-face ((,class (:inherit kite-text-face))))
     `(kite-cdata-section-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-char-ref-delimiter-face ((,class (:inherit kite-ref-face))))
     `(kite-char-ref-number-face ((,class (:inherit kite-ref-face))))
     `(kite-comment-content-face ((,class (:slant italic))))
     `(kite-comment-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-console-prompt-face ((,class (:inherit default))))
     `(kite-css-property ((,class (:inherit css-property))))
     `(kite-css-proprietary-property ((,class (:inherit css-proprietary-property))))
     `(kite-css-selected-overlay ((,class (:inherit secondary-selection))))
     `(kite-css-selector ((,class (:inherit css-selector))))
     `(kite-element-colon-face ((,class (:inherit kite-name-face))))
     `(kite-element-local-name-face ((,class (:inherit kite-name-face))))
     `(kite-element-prefix-face ((,class (:inherit kite-name-face))))
     `(kite-entity-ref-delimiter-face ((,class (:inherit kite-ref-face))))
     `(kite-entity-ref-name-face ((,class (:inherit kite-ref-face))))
     `(kite-hash-face ((,class (:inherit kite-name-face))))
     `(kite-link-face ((,class (:inherit change-log-file))))
     `(kite-loading ((,class (:inherit font-lock-comment))))
     `(kite-log-debug ((,class (:inherit font-lock-comment))))
     `(kite-log-error ((,class (:inherit error))))
     `(kite-log-log ((,class (:inherit default))))
     `(kite-log-warning ((,class (:inherit warning))))
     `(kite-markup-declaration-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-namespace-attribute-colon-face ((,class (:inherit kite-name-face))))
     `(kite-namespace-attribute-prefix-face ((,class (:inherit kite-name-face))))
     `(kite-namespace-attribute-value-delimiter-face ((,class (:inherit kite-attribute-value-delimiter-face))))
     `(kite-namespace-attribute-value-face ((,class (:inherit kite-attribute-value-face))))
     `(kite-namespace-attribute-xmlns-face ((,class (:inherit kite-name-face))))
     `(kite-null ((,class (:inherit nxml-char-ref-number))))
     `(kite-number ((,class (:inherit nxml-char-ref-number))))
     `(kite-object ((,class (:inherit font-lock-variable-name))))
     `(kite-processing-instruction-content-face ((,class (:inherit kite-delimited-data-face))))
     `(kite-processing-instruction-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-processing-instruction-target-face ((,class (:inherit kite-name-face))))
     `(kite-prolog-keyword-face ((,class (:inherit kite-name-face))))
     `(kite-prolog-literal-content-face ((,class (:inherit kite-delimited-data-face))))
     `(kite-prolog-literal-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-property-name ((,class (:inherit default))))
     `(kite-quote ((,class (:inherit font-lock-keyword))))
     `(kite-stack-column-number ((,class (:inherit kite-number))))
     `(kite-stack-error-message ((,class (:inherit default))))
     `(kite-stack-error-type ((,class (:inherit error))))
     `(kite-stack-file-name ((,class (:inherit link))))
     `(kite-stack-function-name ((,class (:inherit font-lock-function-name-face))))
     `(kite-stack-line-number ((,class (:inherit kite-number))))
     `(kite-stack-pseudo-file-name ((,class (:inherit default))))
     `(kite-string ((,class (:inherit font-lock-string))))
     `(kite-table-head ((,class (:inherit highlight))))
     `(kite-tag-delimiter-face ((,class (:inherit kite-delimiter-face))))
     `(kite-tag-slash-face ((,class (:inherit kite-name-face))))
     `(kite-undefined ((,class (:inherit nxml-char-ref-number))))
;;;;; ledger-mode
     `(ledger-font-payee-uncleared-face ((t (:foreground ,red))))
     `(ledger-font-payee-cleared-face ((t (:foreground ,green :weight normal))))
     `(ledger-font-xact-highlight-face ((t (:background ,base02))))
     `(ledger-font-pending-face ((t (:foreground ,yellow weight: normal))))
     `(ledger-font-other-face ((t (:foreground ,base0))))
     `(ledger-font-posting-account-face ((t (:foreground ,cyan))))
     `(ledger-font-posting-account-cleared-face ((t (:foreground ,base0))))
     `(ledger-font-posting-account-pending-face ((t (:foreground ,yellow))))
     `(ledger-font-posting-amount-face ((t (:foreground ,yellow))))
     `(ledger-occur-narrowed-face ((t (:foreground ,base3 :invisible t))))
     `(ledger-occur-xact-face ((t (:background ,base02))))
     `(ledger-font-comment-face ((t (:foreground ,base01))))
     `(ledger-font-reconciler-uncleared-face ((t (:foreground ,red :weight bold))))
     `(ledger-font-reconciler-cleared-face ((t (:foreground ,base0 :weight normal))))
     `(ledger-font-reconciler-pending-face ((t (:foreground ,yellow :weight normal))))
     `(ledger-font-report-clickable-face ((t (:foreground ,yellow :weight normal))))
;;;;; linum-mode
     `(linum ((,class (:foreground ,s-fringe-fg :background ,s-fringe-bg))))
;;;;; lusty-explorer
     `(lusty-directory-face ((,class (:inherit dired-directory))))
     `(lusty-file-face ((,class nil)))
     `(lusty-match-face ((,class (:inherit ido-first-match))))
     `(lusty-slash-face ((,class (:foreground ,cyan :weight bold))))
;;;;; magit
     `(magit-section-title ((,class (:foreground ,yellow :weight bold))))
     `(magit-branch ((,class (:foreground ,orange :weight bold))))
     `(magit-item-highlight ((,class (:background ,base02 :weight unspecified))))
     `(magit-log-author ((,class (:foreground ,cyan))))
     `(magit-log-graph ((,class (:foreground ,base01))))
     `(magit-log-head-label-bisect-bad ((,class (:background ,red-hc :foreground ,red-lc
                                                             :box 1))))
     `(magit-log-head-label-bisect-good ((,class (:background ,green-hc :foreground ,green-lc
                                                              :box 1))))
     `(magit-log-head-label-default ((,class (:background ,base02 :box 1))))
     `(magit-log-head-label-local ((,class (:background ,blue-lc :foreground ,blue-hc
                                                        :box 1))))
     `(magit-log-head-label-patches ((,class (:background ,red-lc :foreground ,red-hc
                                                          :box 1))))
     `(magit-log-head-label-remote ((,class (:background ,green-lc :foreground ,green-hc
                                                         :box 1))))
     `(magit-log-head-label-tags ((,class (:background ,yellow-lc :foreground ,yellow-hc
                                                       :box 1))))
     ;; magit hotfix
     `(magit-header ((,class (:inherit default))))
     ;; TODO
     `(magit-log-sha1 ((,class (:foreground ,yellow))))
     `(magit-cherry-equivalent ((,class (:foreground ,magenta))))
     `(magit-cherry-unmatched ((,class (:foreground ,cyan))))
     ;; `(magit-log-head-label-bisect-skip ((,class (:background "light goldenrod" :foreground "dark goldenrod" :box 1))))
     ;; `(magit-log-head-label-head ((,class (:background "Grey70" :foreground "Black" :box 1))))
     ;; `(magit-log-head-label-wip ((,class (:background "Grey95" :foreground "LightSkyBlue3" :box 1))))
     ;; `(magit-log-reflog-label-checkout ((,class (:background "Grey85" :foreground "LightSkyBlue4" :box 1))))
     ;; `(magit-log-reflog-label-cherry-pick ((,class (:background "light green" :foreground "dark olive green" :box 1))))
     ;; `(magit-log-reflog-label-commit ((,class (:background "LemonChiffon1" :foreground "goldenrod4" :box 1))))
     ;; `(magit-log-reflog-label-other ((,class (:background "Grey50" :box 1))))
     ;; `(magit-log-reflog-label-rebase ((,class (:background "Grey85" :foreground "OliveDrab4" :box 1))))
     ;; `(magit-log-reflog-label-remote ((,class (:background "Grey50" :box 1))))
     ;; `(magit-log-reflog-label-reset ((,class (:background "IndianRed1" :foreground "IndianRed4" :box 1))))
     `(magit-process-ng ((,class (:inherit magit-header :foreground ,red))))
     `(magit-process-ok ((,class (:inherit magit-header :foreground ,green))))
     `(magit-signature-bad ((,class (:foreground ,red))))
     `(magit-signature-good ((,class (:foreground ,green))))
     `(magit-signature-none ((,class (:inherit magit-log-message))))
     `(magit-signature-untrusted ((,class (:foreground ,cyan))))
     `(magit-whitespace-warning-face ((,class (:inherit trailing-whitespace))))
;;;;; markdown-mode
     `(markdown-blockquote-face ((,class (:inherit font-lock-doc-face))))
     `(markdown-bold-face ((,class (:inherit bold))))
     `(markdown-comment-face ((,class (:foreground ,base01 :strike-through t))))
     `(markdown-footnote-face ((,class (:inherit default))))
     `(markdown-header-delimiter-face ((,class (:foreground ,base01))))
     `(markdown-header-face ((,class (:foreground ,blue))))
     `(markdown-header-face-1 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-2 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-3 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-4 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-5 ((,class (:inherit markdown-header-face))))
     `(markdown-header-face-6 ((,class (:inherit markdown-header-face))))
     `(markdown-header-rule-face ((,class (:foreground ,base01))))
     `(markdown-inline-code-face ((,class (:foreground ,base01))))
     `(markdown-italic-face ((,class (:inherit italic))))
     `(markdown-language-keyword-face ((,class (:inherit default))))
     `(markdown-line-break-face ((,class (:inherit default :underline t))))
     `(markdown-link-face ((,class (:inherit default :foreground ,yellow))))
     `(markdown-link-title-face ((,class (:inherit font-lock-comment-face))))
     `(markdown-list-face ((,class (:inherit font-lock-builtin-face))))
     `(markdown-math-face ((,class (:inherit font-lock-string-face))))
     `(markdown-metadata-key-face ((,class (:inherit font-lock-comment-face))))
     `(markdown-metadata-value-face ((,class (:inherit default))))
     `(markdown-missing-link-face ((,class (:inherit font-lock-warning-face))))
     `(markdown-pre-face ((,class (:foreground ,base01))))
     `(markdown-reference-face ((,class (:inherit default :foreground ,base01))))
     `(markdown-url-face ((,class (:foreground ,base01))))
;;;;; multiple-cursors
     `(mc/cursor-face ((,class (:inherit cursor :inverse-video nil))))
;;;;; message-mode
     `(message-cited-text ((,class (:foreground ,base01))))
     `(message-header-name ((,class (:foreground ,base01))))
     `(message-header-other ((,class (:foreground ,base0 :weight normal))))
     `(message-header-to ((,class (:foreground ,base0 :weight normal))))
     `(message-header-cc ((,class (:foreground ,base0 :weight normal))))
     `(message-header-newsgroups ((,class (:foreground ,yellow :weight bold))))
     `(message-header-subject ((,class (:foreground ,cyan :weight normal))))
     `(message-header-xheader ((,class (:foreground ,cyan))))
     `(message-mml ((,class (:foreground ,yellow :weight bold))))
     `(message-separator ((,class (:foreground ,base01 :slant italic))))
;;;;; mew
     `(mew-face-header-subject ((,class (:foreground ,orange))))
     `(mew-face-header-from ((,class (:foreground ,yellow))))
     `(mew-face-header-date ((,class (:foreground ,green))))
     `(mew-face-header-to ((,class (:foreground ,red))))
     `(mew-face-header-key ((,class (:foreground ,green))))
     `(mew-face-header-private ((,class (:foreground ,green))))
     `(mew-face-header-important ((,class (:foreground ,blue))))
     `(mew-face-header-marginal ((,class (:foreground ,base0 :weight bold))))
     `(mew-face-header-warning ((,class (:foreground ,red))))
     `(mew-face-header-xmew ((,class (:foreground ,green))))
     `(mew-face-header-xmew-bad ((,class (:foreground ,red))))
     `(mew-face-body-url ((,class (:foreground ,orange))))
     `(mew-face-body-comment ((,class (:foreground ,base0 :slant italic))))
     `(mew-face-body-cite1 ((,class (:foreground ,green))))
     `(mew-face-body-cite2 ((,class (:foreground ,blue))))
     `(mew-face-body-cite3 ((,class (:foreground ,orange))))
     `(mew-face-body-cite4 ((,class (:foreground ,yellow))))
     `(mew-face-body-cite5 ((,class (:foreground ,red))))
     `(mew-face-mark-review ((,class (:foreground ,blue))))
     `(mew-face-mark-escape ((,class (:foreground ,green))))
     `(mew-face-mark-delete ((,class (:foreground ,red))))
     `(mew-face-mark-unlink ((,class (:foreground ,yellow))))
     `(mew-face-mark-refile ((,class (:foreground ,green))))
     `(mew-face-mark-unread ((,class (:foreground ,red))))
     `(mew-face-eof-message ((,class (:foreground ,green))))
     `(mew-face-eof-part ((,class (:foreground ,yellow))))
;;;;; mic-paren
     `(paren-face-match
       ((,class (:foreground ,magenta :background unspecified
                             :weight ,s-maybe-bold))))
     `(paren-face-mismatch
       ((,class (:foreground ,base02 :background ,red
                             :weight ,s-maybe-bold))))
     `(paren-face-no-match
       ((,class (:foreground ,base02 :background ,red
                             :weight ,s-maybe-bold))))
;;;;; mingus
     `(mingus-directory-face ((,class (:foreground ,blue))))
     `(mingus-pausing-face ((,class (:foreground ,magenta))))
     `(mingus-playing-face ((,class (:foreground ,cyan))))
     `(mingus-playlist-face ((,class (:foreground ,cyan ))))
     `(mingus-song-file-face ((,class (:foreground ,yellow))))
     `(mingus-stopped-face ((,class (:foreground ,red))))
;;;;; moccur
     `(moccur-current-line-face ((,class (:underline t))))
     `(moccur-edit-done-face ((,class
                               (:foreground ,base01
                                            :background ,base03
                                            :slant italic))))
     `(moccur-edit-face
       ((,class (:background ,yellow :foreground ,base03))))
     `(moccur-edit-file-face ((,class (:background ,base02))))
     `(moccur-edit-reject-face ((,class (:foreground ,red))))
     `(moccur-face ((,class (:background ,base02 :foreground ,base1
                                         :weight bold))))
     `(search-buffers-face ((,class (:background ,base02 :foreground ,base1
                                                 :weight bold))))
     `(search-buffers-header-face ((,class (:background ,base02 :foreground ,yellow
                                                        :weight bold))))
;;;;; mu4e
     `(mu4e-cited-1-face ((,class (:foreground ,green :slant italic :weight normal))))
     `(mu4e-cited-2-face ((,class (:foreground ,blue :slant italic :weight normal))))
     `(mu4e-cited-3-face ((,class (:foreground ,orange :slant italic :weight normal))))
     `(mu4e-cited-4-face ((,class (:foreground ,yellow :slant italic :weight normal))))
     `(mu4e-cited-5-face ((,class (:foreground ,cyan :slant italic :weight normal))))
     `(mu4e-cited-6-face ((,class (:foreground ,green :slant italic :weight normal))))
     `(mu4e-cited-7-face ((,class (:foreground ,blue :slant italic :weight normal))))
     `(mu4e-flagged-face ((,class (:foreground ,blue :weight normal))))
     `(mu4e-unread-face ((,class (:foreground ,green :weight normal))))
     `(mu4e-view-url-number-face ((,class (:foreground ,yellow :weight normal))))
     `(mu4e-warning-face ((,class (:foreground ,red :slant normal :weight bold))))
     `(mu4e-header-highlight-face
       ((,class (:inherit unspecified :foreground unspecified :background ,base02
                          :underline unspecified  :weight unspecified))))
     `(mu4e-view-contact-face ((,class (:foreground ,base0  :weight normal))))
     `(mu4e-view-header-key-face ((,class (:inherit message-header-name :weight normal))))
     `(mu4e-view-header-value-face ((,class (:foreground ,cyan :weight normal :slant normal))))
     `(mu4e-view-link-face ((,class (:inherit link))))
     `(mu4e-view-special-header-value-face ((,class (:foreground ,blue :weight normal :underline nil))))
;;;;; mumamo
     `(mumamo-background-chunk-submode1 ((,class (:background ,base02))))
;;;;; nav
     `(nav-face-heading ((,class (:foreground ,yellow))))
     `(nav-face-button-num ((,class (:foreground ,cyan))))
     `(nav-face-dir ((,class (:foreground ,green))))
     `(nav-face-hdir ((,class (:foreground ,red))))
     `(nav-face-file ((,class (:foreground ,base0))))
     `(nav-face-hfile ((,class (:foreground ,red))))
;;;;; nav-flash
     ;; `(nav-flash-face ((,class (:background ,base02))))
     `(nav-flash-face ((,class (:foreground
                                ,(apply 'solarized-color-blend
                                        (if
                                            (eq variant 'light)
                                            (list yellow base1 0.2)
                                          (list cyan base1 0.1)))
                                :background
                                ,(apply 'solarized-color-blend
                                        (if
                                            (eq variant 'light)
                                            (list yellow base03 0.2)
                                          (list cyan base03 0.3)))))))
;;;;; navi2ch
     `(navi2ch-list-category-face ((,class (:foreground ,blue ))))
     `(navi2ch-list-add-board-name-face ((,class (:foreground ,yellow))))
     `(navi2ch-list-board-name-face ((,class (:foreground ,blue))))
     `(navi2ch-list-change-board-name-face ((,class (:foreground ,green :weight bold))))
     `(navi2ch-bm-unread-face ((,class (:foreground ,green))))
     `(navi2ch-bm-view-face ((,class (:foreground ,yellow))))
     `(navi2ch-bm-cache-face ((,class (:foreground ,blue))))
     `(navi2ch-bm-update-face ((,class (:foreground ,orange))))
     `(navi2ch-bm-down-face ((,class (:foreground ,base1))))
     `(navi2ch-bm-mark-face ((,class (:foreground ,red))))
     `(navi2ch-bm-new-unread-face ((,class (:foreground ,green))))
     `(navi2ch-bm-new-view-face ((,class (:foreground ,yellow))))
     `(navi2ch-bm-new-cache-face ((,class (:foreground ,blue))))
     `(navi2ch-bm-new-update-face ((,class (:foreground ,orange))))
     `(navi2ch-bm-new-mark-face ((,class (:foreground ,red))))
     `(navi2ch-bm-updated-unread-face ((,class (:foreground ,green))))
     `(navi2ch-bm-updated-view-face ((,class (:foreground ,yellow))))
     `(navi2ch-bm-updated-cache-face ((,class (:foreground ,blue))))
     `(navi2ch-bm-updated-update-face ((,class (:foreground ,orange))))
     `(navi2ch-bm-updated-navi2ch-bm-updated-mark-facemark-face ((,class (:foreground ,red))))
     `(navi2ch-bm-seen-unread-face ((,class (:foreground ,green))))
     `(navi2ch-bm-seen-view-face ((,class (:foreground ,yellow))))
     `(navi2ch-bm-seen-cache-face ((,class (:foreground ,blue))))
     `(navi2ch-bm-seen-update-face ((,class (:foreground ,orange))))
     `(navi2ch-bm-seen-mark-face ((,class (:foreground ,red))))
     `(navi2ch-article-header-face ((,class (:foreground ,base1))))
     `(navi2ch-article-header-contents-face ((,class (:foreground ,blue))))
     `(navi2ch-article-header-fusianasan-face ((,class (:foreground ,blue :underline t))))
     `(navi2ch-article-link-face ((,class (:weight bold))))
     `(navi2ch-article-url-face ((,class (:weight bold))))
     `(navi2ch-article-citation-face ((,class (:foreground ,yellow))))
     `(navi2ch-article-auto-decode-face ((,class (:foreground ,base03))))
     `(navi2ch-article-message-separator-face ((,class (:foreground ,green))))
     `(navi2ch-splash-screen-face ((,class (:foreground ,cyan))))
     `(navi2ch-message-link-face ((,class (:weight bold))))
     `(navi2ch-message-url-face ((,class (:weight bold))))
     `(navi2ch-message-citation-face ((,class (:foreground ,magenta))))
;;;;; neotree
     `(neo-banner-face ((,class (:foreground ,base01))))
     `(neo-header-face ((,class (:foreground ,blue))))
     `(neo-root-dir-face ((,class (:foreground ,base1 :weight bold))))
     `(neo-dir-link-face ((,class (:foreground ,blue))))
     `(neo-file-link-face ((,class (:foreground ,base0))))
     `(neo-expand-btn-face ((,class (:foreground ,base01))))
;;;;; org-mode
     `(org-agenda-structure
       ((,class (:foreground ,base1 :background ,base02
                             :weight bold :slant normal :inverse-video nil :height ,solarized-height-plus-1
                             :underline nil
                             :box (:line-width 2 :color ,base03)))))
     `(org-agenda-calendar-event ((,class (:foreground ,base1))))
     `(org-agenda-calendar-sexp ((,class (:foreground ,base0 :slant italic))))
     `(org-agenda-date
       ((,class (:foreground ,base01 :background ,base03 :weight normal
                             :box (:line-width 2 :color ,base03)
                             :inverse-video nil :overline nil :slant normal :height 1.0))))
     `(org-agenda-date-weekend
       ((,class (:inherit org-agenda-date :inverse-video nil :background unspecified
                          :foreground ,base01 :weight unspecified
                          :underline t :overline nil :box unspecified))))
     `(org-agenda-date-today
       ((,class (:inherit org-agenda-date :inverse-video t :weight bold
                          :underline unspecified :overline nil :box unspecified
                          :foreground ,blue :background ,base03))))
     `(org-agenda-done ((,class (:foreground ,base01 :slant italic))))
     `(org-archived ((,class (:foreground ,base01 :weight normal))))
     `(org-block ((,class (:foreground ,base01))))
     `(org-block-begin-line ((,class (:foreground ,base01 :slant italic))))
     `(org-checkbox ((,class (:background ,base03 :foreground ,base0
                                          :box (:line-width 1 :style released-button)))))
     `(org-code ((,class (:foreground ,base01))))
     `(org-date ((,class (:foreground ,blue :underline t))))
     `(org-done ((,class (:weight bold :foreground ,green))))
     `(org-ellipsis ((,class (:foreground ,base01))))
     `(org-formula ((,class (:foreground ,yellow))))
     `(org-headline-done ((,class (:foreground ,green))))
     `(org-hide ((,class (:foreground ,base03))))
     `(org-level-1 ((,class (:inherit ,s-variable-pitch :foreground ,orange
                             ,@(when solarized-scale-org-headlines
                                 (list :height solarized-height-plus-4))))))
     `(org-level-2 ((,class (:inherit ,s-variable-pitch :foreground ,green
                             ,@(when solarized-scale-org-headlines
                                 (list :height solarized-height-plus-3))))))
     `(org-level-3 ((,class (:inherit ,s-variable-pitch :foreground ,blue
                             ,@(when solarized-scale-org-headlines
                                 (list :height solarized-height-plus-2))))))
     `(org-level-4 ((,class (:inherit ,s-variable-pitch :foreground ,yellow
                             ,@(when solarized-scale-org-headlines
                                 (list :height solarized-height-plus-1))))))
     `(org-level-5 ((,class (:inherit ,s-variable-pitch
                                      :foreground ,cyan))))
     `(org-level-6 ((,class (:inherit ,s-variable-pitch
                                      :foreground ,green))))
     `(org-level-7 ((,class (:inherit ,s-variable-pitch
                                      :foreground ,red))))
     `(org-level-8 ((,class (:inherit ,s-variable-pitch
                                      :foreground ,blue))))
     `(org-link ((,class (:foreground ,yellow :underline t))))
     `(org-sexp-date ((,class (:foreground ,violet))))
     `(org-scheduled ((,class (:foreground ,green))))
     `(org-scheduled-previously ((,class (:foreground ,cyan))))
     `(org-scheduled-today ((,class (:foreground ,blue :weight normal))))
     `(org-special-keyword ((,class (:foreground ,base01 :weight bold))))
     `(org-table ((,class (:foreground ,green))))
     `(org-tag ((,class (:weight bold))))
     `(org-time-grid ((,class (:foreground ,base01))))
     `(org-todo ((,class (:foreground ,cyan :weight bold))))
     `(org-upcoming-deadline ((,class (:foreground ,yellow  :weight normal :underline nil))))
     `(org-warning ((,class (:foreground ,orange :weight normal :underline nil))))
     ;; org-habit
     ;; (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
     `(org-habit-clear-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
     `(org-habit-clear-future-face ((,class (:background ,blue-lc))))
     `(org-habit-ready-face ((,class (:background ,green-lc :foreground ,green))))
     `(org-habit-ready-future-face ((,class (:background ,green-lc))))
     `(org-habit-alert-face ((,class (:background ,yellow :foreground ,yellow-lc))))
     `(org-habit-alert-future-face ((,class (:background ,yellow-lc))))
     `(org-habit-overdue-face ((,class (:background ,red :foreground ,red-lc))))
     `(org-habit-overdue-future-face ((,class (:background ,red-lc))))
     ;; latest additions
     `(org-agenda-dimmed-todo-face ((,class (:foreground ,base01))))
     `(org-agenda-restriction-lock ((,class (:background ,yellow))))
     `(org-clock-overlay ((,class (:background ,yellow))))
     `(org-column ((,class (:background ,base02 :strike-through nil
                                        :underline nil :slant normal :weight normal :inherit default))))
     `(org-column-title ((,class (:background ,base02 :underline t :weight bold))))
     `(org-date-selected ((,class (:foreground ,red :inverse-video t))))
     `(org-document-info ((,class (:foreground ,base0))))
     `(org-document-title ((,class (:foreground ,base1  :weight bold :height ,solarized-height-plus-4))))
     `(org-drawer ((,class (:foreground ,cyan))))
     `(org-footnote ((,class (:foreground ,magenta :underline t))))
     `(org-latex-and-export-specials ((,class (:foreground ,orange))))
     `(org-mode-line-clock-overrun ((,class (:inherit mode-line :background ,red))))
;;;;; outline
     `(outline-1 ((,class (:inherit org-level-1 :height ,solarized-height-plus-4))))
     `(outline-2 ((,class (:inherit org-level-2 :height ,solarized-height-plus-3))))
     `(outline-3 ((,class (:inherit org-level-3 :height ,solarized-height-plus-2))))
     `(outline-4 ((,class (:inherit org-level-4 :height ,solarized-height-plus-1))))
     `(outline-5 ((,class (:inherit org-level-5))))
     `(outline-6 ((,class (:inherit org-level-6))))
     `(outline-7 ((,class (:inherit org-level-7))))
     `(outline-8 ((,class (:inherit org-level-8))))
;;;;; paren-face
     `(paren-face  ((,class (:foreground ,base01))))
;;;;; perspective
     `(persp-selected-face ((,class (:foreground ,yellow))))
;;;;; pretty-mode
     `(pretty-mode-symbol-face  ((,class (:foreground ,yellow :weight normal))))
;;;;; prodigy
     `(prodigy-green-face ((,class (:foreground ,green))))
     `(prodigy-red-face ((,class (:foreground ,orange))))
     `(prodigy-yellow-face ((,class (:foreground ,yellow))))
     `(prodigy-line-face ((,class (:background ,base02))))
;;;;; popup
     `(popup-face ((,class (:background ,base02 :foreground ,base0))))
     `(popup-isearch-match ((,class (:background ,yellow :foreground ,base03))))
     `(popup-menu-face ((,class (:background ,base02 :foreground ,base0))))
     `(popup-menu-mouse-face ((,class (:background ,blue :foreground ,base03))))
     `(popup-menu-selection-face ((,class (:background ,magenta :foreground ,base03))))
     `(popup-scroll-bar-background-face ((,class (:background ,base01))))
     `(popup-scroll-bar-foreground-face ((,class (:background ,base1))))
     `(popup-tip-face ((,class (:background ,base02 :foreground ,base0))))
;;;;; pophint
     `(pophint:tip-face ((,class (:background ,magenta :foreground ,base03))))
     `(pophint:match-face ((,class (:background ,blue :foreground ,base03))))
     `(pophint:pos-tip-face ((,class (:background ,base02 :foreground ,base0))))
;;;;; powerline
     `(powerline-active1 ((,class ,(if solarized-high-contrast-mode-line
                                       `(:background ,base00 :foreground ,base03)
                                       `(:background ,base03 :foreground ,base00)))))
     `(powerline-active2 ((,class ,(if solarized-high-contrast-mode-line
                                       `(:background ,base01 :foreground ,base03)
                                       `(:background ,base02 :foreground ,base00)))))
     `(powerline-inactive1 ((,class ,(if solarized-high-contrast-mode-line
                                         `(:background ,base03 :foreground ,base1)
                                         `(:background ,base02 :foreground ,base01)))))
     `(powerline-inactive2 ((,class ,(if solarized-high-contrast-mode-line
                                         `(:background ,base02 :foreground ,base1)
                                         `(:background ,base03 :foreground ,base01)))))
;;;;; rainbow-blocks
     `(rainbow-blocks-depth-1-face ((,class (:foreground ,cyan))))
     `(rainbow-blocks-depth-2-face ((,class (:foreground ,yellow))))
     `(rainbow-blocks-depth-3-face ((,class (:foreground ,blue))))
     `(rainbow-blocks-depth-4-face ((,class (:foreground ,violet))))
     `(rainbow-blocks-depth-5-face ((,class (:foreground ,green))))
     `(rainbow-blocks-depth-6-face ((,class (:foreground ,yellow))))
     `(rainbow-blocks-depth-7-face ((,class (:foreground ,blue))))
     `(rainbow-blocks-depth-8-face ((,class (:foreground ,violet))))
     `(rainbow-blocks-depth-9-face ((,class (:foreground ,green))))
     `(rainbow-blocks-unmatched-face ((,class (:foreground ,red))))
;;;;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyan))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-10-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-11-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-12-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-unmatched-face
       ((,class (:foreground ,base0 :background ,base03 :inverse-video t))))
;;;;; rst-mode
     `(rst-level-1-face ((,class (:background ,yellow   :foreground ,base03))))
     `(rst-level-2-face ((,class (:background ,cyan    :foreground ,base03))))
     `(rst-level-3-face ((,class (:background ,blue    :foreground ,base03))))
     `(rst-level-4-face ((,class (:background ,violet  :foreground ,base03))))
     `(rst-level-5-face ((,class (:background ,magenta :foreground ,base03))))
     `(rst-level-6-face ((,class (:background ,red     :foreground ,base03))))
;;;;; rpm-mode
     `(rpm-spec-dir-face ((,class (:foreground ,green))))
     `(rpm-spec-doc-face ((,class (:foreground ,green))))
     `(rpm-spec-ghost-face ((,class (:foreground ,red))))
     `(rpm-spec-macro-face ((,class (:foreground ,yellow))))
     `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red))))
     `(rpm-spec-package-face ((,class (:foreground ,red))))
     `(rpm-spec-section-face ((,class (:foreground ,yellow))))
     `(rpm-spec-tag-face ((,class (:foreground ,blue))))
     `(rpm-spec-var-face ((,class (:foreground ,red))))
;;;;; sh-mode
     `(sh-quoted-exec ((,class (:foreground ,violet :weight bold))))
     `(sh-escaped-newline ((,class (:foreground ,yellow :weight bold))))
     `(sh-heredoc ((,class (:foreground ,yellow :weight bold))))
;;;;; skewer-mode
     `(skewer-error-face ((,class (:foreground ,orange :underline nil))))
     `(skewer-repl-log-face ((,class (:foreground ,violet))))
;;;;; smart-mode-line
     ;; use (setq sml/theme nil) to enable Solarized for sml
     `(sml/filename ((,class (:foreground ,base1 :weight bold))))
     `(sml/prefix ((,class (:foreground unspecified))))
     `(sml/git ((,class (:foreground unspecified))))
     `(sml/process ((,class (:weight bold))))
     `(sml/sudo ((,class  (:foreground ,orange :weight bold))))
     `(sml/read-only ((,class (:foreground ,cyan))))
     `(sml/outside-modified ((,class (:foreground , cyan))))
     `(sml/modified ((,class (:foreground ,cyan))))
     `(sml/vc-edited ((,class (:foreground ,green))))
     `(sml/charging ((,class (:foreground ,base1))))
     `(sml/discharging ((,class (:foreground ,base1 :weight bold))))
;;;;; smartparens
     `(sp-pair-overlay-face ((,class (:background ,base02))))
     `(sp-wrap-overlay-face ((,class (:background ,base02))))
     `(sp-wrap-tag-overlay-face ((,class (:background ,base02))))
     `(sp-show-pair-enclosing ((,class (:inherit highlight))))
     `(sp-show-pair-match-face
       ((,class (:background unspecified :foreground ,magenta
                             :weight ,s-maybe-bold))))
     `(sp-show-pair-mismatch-face
       ((,class (:foreground ,base02 :background ,red
                             :weight ,s-maybe-bold))))
;;;;; show-paren
     `(show-paren-match
       ((,class (:foreground ,magenta :background unspecified
                             :weight ,s-maybe-bold))))
     `(show-paren-mismatch
       ((,class (:foreground ,base02 :background ,red
                             :weight ,s-maybe-bold))))
;;;;; syslog-mode
     `(syslog-ip ((,class (:background unspecified
                                       :foreground ,green
                                       :underline nil
                                       :weight normal
                                       :slant normal))))
     `(syslog-hour ((,class (:background unspecified
                                         :foreground ,yellow))))
     `(syslog-error ((,class (:background unspecified
                                          :foreground ,orange
                                          :weight bold))))
     `(syslog-warn ((,class (:background unspecified
                                         :foreground ,yellow
                                         :weight bold))))
     `(syslog-info ((,class (:background unspecified
                                         :foreground ,blue
                                         :weight bold))))
     `(syslog-debug ((,class (:background unspecified
                                          :foreground ,cyan
                                          :weight bold))))
     `(syslog-su ((,class (:background unspecified
                                       :foreground ,violet
                                       :weight normal))))
;;;;; slime
     `(slime-repl-inputed-output-face ((,class (:foreground ,red))))
;;;;; speedbar
     `(speedbar-button-face ((,class (:inherit ,s-variable-pitch
                                               :foreground ,base01))))
     `(speedbar-directory-face ((,class (:inherit ,s-variable-pitch :foreground ,blue))))
     `(speedbar-file-face ((,class (:inherit ,s-variable-pitch :foreground ,base0))))
     `(speedbar-highlight-face ((,class (:inherit ,s-variable-pitch :background ,base02))))
     `(speedbar-selected-face ((,class (:inherit ,s-variable-pitch
                                                 :foreground ,yellow :underline t))))
     `(speedbar-separator-face ((,class (:inherit ,s-variable-pitch
                                                  :background ,blue :foreground ,base03
                                                  :overline ,cyan-lc))))
     `(speedbar-tag-face ((,class (:inherit ,s-variable-pitch :foreground ,green))))
;;;;; stripe-buffer
     `(stripe-highlight ((,class (:background ,base02))))
;;;;; structured-haskell
     `(shm-current-face ((,class (:background ,base02))))
     `(shm-quarantine-face ((,class (:background ,base01))))
;;;;; sunrise commander
;;;;;; headings
     `(sr-active-path-face ((,class (:background ,blue :foreground ,base03
                                                 :height ,solarized-height-plus-1  :weight bold))))
     `(sr-editing-path-face ((,class (:background ,yellow :foreground ,base03
                                                  :weight bold :height ,solarized-height-plus-1))))
     `(sr-highlight-path-face ((,class (:background ,green :foreground ,base03
                                                    :weight bold :height ,solarized-height-plus-1))))
     `(sr-passive-path-face ((,class (:background ,base01 :foreground ,base03
                                                  :weight bold :height ,solarized-height-plus-1))))
;;;;;; marked
     `(sr-marked-dir-face ((,class (:inherit dired-marked))))
     `(sr-marked-file-face ((,class (:inherit dired-marked))))
     `(sr-alt-marked-dir-face ((,class (:background ,magenta :foreground ,base03
                                                    :weight bold))))
     `(sr-alt-marked-file-face ((,class (:background ,magenta :foreground ,base03
                                                     :weight bold))))
;;;;;; fstat
     `(sr-directory-face ((,class (:inherit dired-directory :weight normal))))
     `(sr-symlink-directory-face ((,class (:inherit dired-directory
                                                    :slant italic :weight normal))))
     `(sr-symlink-face ((,class (:inherit dired-symlink :slant italic :weight normal))))
     `(sr-broken-link-face ((,class (:inherit dired-warning :slant italic :weight normal))))
;;;;;; file types
     `(sr-compressed-face ((,class (:foreground ,base0))))
     `(sr-encrypted-face ((,class (:foreground ,base0))))
     `(sr-log-face ((,class (:foreground ,base0))))
     `(sr-packaged-face ((,class (:foreground ,base0))))
     `(sr-html-face ((,class (:foreground ,base0))))
     `(sr-xml-face ((,class (:foreground ,base0))))
;;;;;; misc
     `(sr-clex-hotchar-face ((,class (:background ,red  :foreground ,base03
                                                  :weight bold))))
;;;;; swoop
     `(swoop-face-header-format-line ((,class (:foreground ,yellow :weight bold
                                                           :height unspecified))))
     `(swoop-face-line-buffer-name ((,class (:background ,base02 :foreground ,base1
                                                         :weight bold :height unspecified))))
     `(swoop-face-line-number ((,class (:foreground ,base01))))
     `(swoop-face-target-line ((,class (:background ,base02 :foreground unspecified))))
     `(swoop-face-target-words ((,class (:background unspecified :foreground ,magenta))))
;;;;; table
     `(table-cell ((,class (:foreground ,base0 :background ,base02))))
;;;;; term
     `(term ((t ( :background ,base03 :foreground ,base0))))
     `(term-color-black ((t (:foreground ,base02 :background ,base02))))
     `(term-color-red ((t (:foreground ,red :background ,red))))
     `(term-color-green ((t (:foreground ,green :background ,green))))
     `(term-color-yellow ((t (:foreground ,yellow :background ,yellow))))
     `(term-color-blue ((t (:foreground ,blue :background ,blue))))
     `(term-color-magenta ((t (:foreground ,magenta :background ,magenta))))
     `(term-color-cyan ((t (:foreground ,cyan :background ,cyan))))
     `(term-color-white ((t (:foreground ,base2 :background ,base2))))
;;;;; todotxt
     `(todotxt-priority-a-face ((,class (:foreground ,orange))))
     `(todotxt-priority-b-face ((,class (:foreground ,yellow))))
     `(todotxt-priority-c-face ((,class (:foreground ,violet))))
;;;;; tooltip
     ;; NOTE: This setting has no effect on the os widgets for me
     ;; zencoding uses this.
     `(tooltip ((,class (:background ,yellow-lc :foreground ,yellow-hc
                                     :inherit ,s-variable-pitch))))
;;;;; tuareg
     `(tuareg-font-lock-governing-face ((,class (:foreground ,magenta :weight bold))))
     `(tuareg-font-lock-multistage-face ((,class (:foreground ,blue :background ,base02
                                                              :weight bold))))
     `(tuareg-font-lock-operator-face ((,class (:foreground ,base1))))
     `(tuareg-font-lock-error-face ((,class (:foreground ,yellow :background ,red
                                                         :weight bold))))
     `(tuareg-font-lock-interactive-output-face ((,class (:foreground ,cyan))))
     `(tuareg-font-lock-interactive-error-face ((,class (:foreground ,red))))
;;;;; undo-tree
     `(undo-tree-visualizer-default-face
       ((,class (:foreground ,base01 :background ,base03))))
     `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,green))))
     `(undo-tree-visualizer-current-face ((,class (:foreground ,blue :inverse-video t))))
     `(undo-tree-visualizer-active-branch-face
       ((,class (:foreground ,base1 :background ,base03 :weight bold))))
     `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))
;;;;; volatile highlights
     `(vhl/default-face ((,class (:background ,green-lc :foreground ,green-hc))))
;;;;; w3m
     `(w3m-anchor ((,class (:inherit link))))
     `(w3m-arrived-anchor ((,class (:inherit link-visited))))
     `(w3m-form ((,class (:background ,base03 :foreground ,base0))))
     `(w3m-header-line-location-title
       ((,class (:background ,base02 :foreground ,yellow))))
     `(w3m-header-line-location-content
       ((,class (:background ,base02 :foreground ,base0))))
     `(w3m-bold ((,class (:foreground ,base1 :weight bold))))
     `(w3m-image-anchor ((,class (:background ,base03 :foreground ,cyan :inherit link))))
     `(w3m-image ((,class (:background ,base03 :foreground ,cyan))))
     `(w3m-lnum-minibuffer-prompt ((,class (:foreground ,base1))))
     `(w3m-lnum-match ((,class (:background ,base02))))
     `(w3m-lnum ((,class (:underline nil :bold nil :foreground ,red))))
     `(w3m-session-select ((,class (:foreground ,base0))))
     `(w3m-session-selected ((,class (:foreground ,base1 :bold t :underline t))))
     `(w3m-tab-background ((,class (:background ,base03 :foreground ,base0))))
     `(w3m-tab-selected-background
       ((,class (:background ,base03 :foreground ,base0))))
     `(w3m-tab-mouse ((,class (:background ,base02 :foreground ,yellow))))
     `(w3m-tab-selected ((,class (:background ,base02 :foreground ,base1
                                              :bold t))))
     `(w3m-tab-unselected ((,class (:background ,base02 :foreground ,base0))))
     `(w3m-tab-selected-retrieving ((,class (:background ,base02 :foreground ,red))))
     `(w3m-tab-unselected-retrieving
       ((,class (:background ,base02 :foreground ,orange))))
     `(w3m-tab-unselected-unseen ((,class (:background ,base02 :foreground ,violet))))
;;;;; web-mode
     `(web-mode-builtin-face ((,class (:inherit font-lock-builtin-face))))
     `(web-mode-comment-face ((,class (:foreground ,base01))))
     `(web-mode-constant-face ((,class (:foreground ,blue :weight bold))))
     `(web-mode-current-element-highlight-face ((,class
                                                 (:underline unspecified :weight unspecified
                                                             :background ,base02))))
     `(web-mode-css-at-rule-face ((,class (:foreground ,violet :slant italic))))
     `(web-mode-css-pseudo-class-face ((,class (:foreground ,green :slant italic))))
     `(web-mode-doctype-face ((,class (:foreground ,base01
                                                   :slant italic :weight bold))))
     `(web-mode-folded-face ((,class (:underline t))))
     `(web-mode-function-name-face ((,class (:foreground ,blue))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,blue :slant normal))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,cyan :slant italic))))
     `(web-mode-html-tag-face ((,class (:foreground ,green))))
     `(web-mode-keyword-face ((,class (:foreground ,yellow :weight normal))))
     `(web-mode-preprocessor-face ((,class (:foreground ,yellow :slant normal :weight unspecified))))
     `(web-mode-string-face ((,class (:foreground ,cyan))))
     `(web-mode-type-face ((,class (:foreground ,yellow))))
     `(web-mode-variable-name-face ((,class (:foreground ,blue))))
     `(web-mode-warning-face ((,class (:inherit font-lock-warning-face))))
     `(web-mode-block-attr-name-face ((,class (:inherit web-mode-html-attr-name-face))))
     `(web-mode-block-attr-value-face ((,class (:inherit web-mode-html-attr-value-face))))
     `(web-mode-block-comment-face ((,class (:inherit web-mode-comment-face))))
     `(web-mode-block-control-face ((,class (:inherit font-lock-preprocessor-face))))
     `(web-mode-block-face ((,class (:background unspecified))))
     `(web-mode-block-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-comment-keyword-face ((,class (:box 1 :weight bold))))
     `(web-mode-css-color-face ((,class (:inherit font-lock-builtin-face))))
     `(web-mode-css-function-face ((,class (:inherit font-lock-builtin-face))))
     `(web-mode-css-priority-face ((,class (:inherit font-lock-builtin-face))))
     `(web-mode-css-property-name-face ((,class (:inherit font-lock-variable-name-face))))
     `(web-mode-css-selector-face ((,class (:inherit font-lock-keyword-face))))
     `(web-mode-css-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-javascript-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-json-context-face ((,class (:foreground ,violet))))
     `(web-mode-json-key-face ((,class (:foreground ,violet))))
     `(web-mode-json-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-param-name-face ((,class (:foreground ,base0))))
     `(web-mode-part-comment-face ((,class (:inherit web-mode-comment-face))))
     `(web-mode-part-face ((,class (:inherit web-mode-block-face))))
     `(web-mode-part-string-face ((,class (:inherit web-mode-string-face))))
     `(web-mode-symbol-face ((,class (:foreground ,yellow))))
     `(web-mode-whitespace-face ((,class (:background ,red))))
     `(web-mode-html-tag-bracket-face ((,class (:foreground ,base01))))
     `(web-mode-block-delimiter-face ((,class (:inherit font-lock-preprocessor-face))))
     `(web-mode-css-comment-face ((,class (:inherit web-mode-comment-face))))
     `(web-mode-css-variable-face ((,class (:inherit web-mode-variable-name-face :slant italic))))
     `(web-mode-error-face ((,class (:background ,red))))
     `(web-mode-function-call-face ((,class (:inherit font-lock-function-name-face))))
     `(web-mode-html-attr-custom-face ((,class (:inherit web-mode-html-attr-name-face))))
     `(web-mode-html-attr-engine-face ((,class (:inherit web-mode-html-attr-custom-face))))
     `(web-mode-html-attr-equal-face ((,class (:inherit web-mode-html-attr-name-face))))
     `(web-mode-html-tag-custom-face ((,class (:inherit web-mode-html-tag-face))))
     `(web-mode-javascript-comment-face ((,class (:inherit web-mode-comment-face))))
     `(web-mode-json-comment-face ((,class (:inherit web-mode-comment-face))))
;;;;; weather-metno
     `(weather-metno-date ((,class (:foreground ,yellow :height ,solarized-height-plus-3))))
     `(weather-metno-date-range ((,class (:foreground ,blue))))
     `(weather-metno-entry ((,class (:foreground ,cyan))))
     `(weather-metno-footer ((,class (:inherit font-lock-comment-face))))
     `(weather-metno-header ((,class (:inherit header-line))))
;;;;; wgrep
     `(wgrep-delete-face ((,class (:background unspecified :foreground ,blue))))
     `(wgrep-done-face ((,class (:foreground ,green))))
     `(wgrep-face ((,class (:background unspecified :foreground ,blue))))
     `(wgrep-file-face ((,class (:background unspecified :foreground ,magenta))))
     `(wgrep-reject-face ((,class (:foreground ,red :weight unspecified))))
;;;;; whitespace-mode
     `(whitespace-space ((,class (:background unspecified :foreground ,base01
                                              :inverse-video unspecified :slant italic))))
     `(whitespace-hspace ((,class (:background unspecified :foreground ,base1
                                               :inverse-video unspecified))))
     `(whitespace-tab ((,class (:background unspecified :foreground ,red
                                            :inverse-video unspecified :weight bold))))
     `(whitespace-newline ((,class(:background unspecified :foreground ,base01
                                               :inverse-video unspecified))))
     `(whitespace-trailing ((,class (:background unspecified :foreground ,orange-lc
                                                 :inverse-video t))))
     `(whitespace-line ((,class (:background unspecified :foreground ,magenta
                                             :inverse-video unspecified))))
     `(whitespace-space-before-tab ((,class (:background ,red-lc :foreground unspecified
                                                         :inverse-video unspecified))))
     `(whitespace-indentation ((,class (:background unspecified :foreground ,yellow
                                                    :inverse-video unspecified :weight bold))))
     `(whitespace-empty ((,class (:background unspecified :foreground ,red-lc
                                              :inverse-video t))))
     `(whitespace-space-after-tab ((,class (:background unspecified :foreground ,orange
                                                        :inverse-video t :weight bold))))
;;;;; wanderlust
     `(wl-highlight-folder-few-face ((,class (:foreground ,red))))
     `(wl-highlight-folder-many-face ((,class (:foreground ,red))))
     `(wl-highlight-folder-path-face ((,class (:foreground ,orange))))
     `(wl-highlight-folder-unread-face ((,class (:foreground ,blue))))
     `(wl-highlight-folder-zero-face ((,class (:foreground ,base0))))
     `(wl-highlight-folder-unknown-face ((,class (:foreground ,blue))))
     `(wl-highlight-message-citation-header ((,class (:foreground ,red))))
     `(wl-highlight-message-cited-text-1 ((,class (:foreground ,red))))
     `(wl-highlight-message-cited-text-2 ((,class (:foreground ,green))))
     `(wl-highlight-message-cited-text-3 ((,class (:foreground ,blue))))
     `(wl-highlight-message-cited-text-4 ((,class (:foreground ,blue))))
     `(wl-highlight-message-header-contents-face ((,class (:foreground ,green))))
     `(wl-highlight-message-headers-face ((,class (:foreground ,red))))
     `(wl-highlight-message-important-header-contents ((,class (:foreground ,green))))
     `(wl-highlight-message-header-contents ((,class (:foreground ,green))))
     `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,green))))
     `(wl-highlight-message-signature ((,class (:foreground ,green))))
     `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,base0))))
     `(wl-highlight-summary-answered-face ((,class (:foreground ,blue))))
     `(wl-highlight-summary-disposed-face ((,class (:foreground ,base0 :slant italic))))
     `(wl-highlight-summary-new-face ((,class (:foreground ,blue))))
     `(wl-highlight-summary-normal-face ((,class (:foreground ,base0))))
     `(wl-highlight-summary-thread-top-face ((,class (:foreground ,yellow))))
     `(wl-highlight-thread-indent-face ((,class (:foreground ,magenta))))
     `(wl-highlight-summary-refiled-face ((,class (:foreground ,base0))))
     `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))
;;;;; weechat
     `(weechat-error-face ((,class (:inherit error))))
     `(weechat-highlight-face ((,class (:foreground ,base0 :weight bold))))
     `(weechat-nick-self-face ((,class (:foreground ,base01 :weight unspecified))))
     `(weechat-prompt-face ((,class (:inherit minibuffer-prompt))))
     `(weechat-time-face ((,class (:foreground ,base01))))
;;;;; which-func-mode
     `(which-func ((,class (:foreground ,green))))
;;;;; window-number-mode
     `(window-number-face ((,class (:foreground ,green))))
;;;;; yascroll
     `(yascroll:thumb-text-area
       ((,class (:foreground ,base01 :background ,base01))))
     `(yascroll:thumb-fringe
       ((,class (:foreground ,base01 :background ,base01))))
;;;;; yasnippet
     `(yas-field-highlight-face ((,class (:inherit secondary-selection))))
;;;;; zencoding
     `(zencoding-preview-input ((,class (:background ,base02 :box ,base1))))
;;;;; ztree
     `(ztreep-arrow-face ((,class (:foreground ,base01))))
     `(ztreep-diff-header-face ((,class (:foreground ,base01 :weight bold :height 1.2))))
     `(ztreep-diff-header-small-face ((,class (:foreground ,base01 :weight bold))))
     `(ztreep-diff-model-add-face ((,class (:foreground ,blue))))
     `(ztreep-diff-model-diff-face ((,class (:foreground ,red))))
     `(ztreep-diff-model-normal-face ((,class (:foreground ,base0))))
     `(ztreep-expand-sign-face ((,class (:foreground ,base01))))
     `(ztreep-header-face ((,class (:foreground ,base01 :weight bold :height 1.2))))
     `(ztreep-leaf-face ((,class (:foreground  ,base0))))
     `(ztreep-node-face ((,class (:foreground ,blue))))
     ) ; END custom-theme-set-faces
;;; Theme Variables
    (custom-theme-set-variables
     theme-name
;;;;; ansi-colors
     `(ansi-color-names-vector
       [,base02 ,red ,green ,yellow ,blue ,magenta ,cyan ,base00])
;;;;; compilation
     `(compilation-message-face 'default)
;;;;; cua
     `(cua-normal-cursor-color ,base0)
     `(cua-read-only-cursor-color ,green)
     `(cua-global-mark-cursor-color ,cyan)
     `(cua-overwrite-cursor-color ,yellow)
;;;;; fill-column-indicator
     `(fci-rule-color ,base02)
;;;;; magit
     `(magit-diff-use-overlays nil)
;;;;; highlight-changes
     `(highlight-changes-colors '(,magenta ,violet))
;;;;; highlight-symbol
     `(highlight-symbol-foreground-color ,base1)
     `(highlight-symbol-colors
       (--map (solarized-color-blend it ,base03 0.25)
              '(,yellow ,cyan ,red ,violet ,green ,orange ,blue)))
;;;;; highlight-tail
     `(highlight-tail-colors
       '((,base02 . 0)(,green-lc . 20)(,cyan-lc . 30)(,blue-lc . 50)
         (,yellow-lc . 60)(,orange-lc . 70)(,magenta-lc . 85)(,base02 . 100)))
;;;;; hl-anything
     `(hl-fg-colors '(,base03 ,base03 ,base03 ,base03 ,base03 ,base03
                              ,base03 ,base03))
     `(hl-bg-colors '(,yellow-lc ,orange-lc ,red-lc ,magenta-lc
                                 ,violet-lc ,blue-lc ,cyan-lc ,green-lc))
;;;;; pos-tip
     `(pos-tip-foreground-color ,base1)
     `(pos-tip-background-color ,base02)
;;;;; smartrep
     `(smartrep-mode-line-active-bg (solarized-color-blend ,green ,s-mode-line-bg 0.2))
;;;;; term
     `(term-default-fg-color ,base0) ;; @deprecated24.3
     `(term-default-bg-color ,base03) ;; @deprecated24.3
;;;;; vc
     `(vc-annotate-color-map
       '((20 . ,red)
         (40 . ,(solarized-color-blend yellow red (/ 2.0 4)))
         (60 . ,(solarized-color-blend yellow red (/ 3.0 4)))
         (80 . ,yellow)
         (100 . ,(solarized-color-blend green yellow (/ 2.0 6)))
         (120 . ,(solarized-color-blend green yellow (/ 3.0 6)))
         (140 . ,(solarized-color-blend green yellow (/ 4.0 6)))
         (160 . ,(solarized-color-blend green yellow (/ 5.0 6)))
         (180 . ,green)
         (200 . ,(solarized-color-blend cyan green (/ 2.0 6)))
         (220 . ,(solarized-color-blend cyan green (/ 3.0 6)))
         (240 . ,(solarized-color-blend cyan green (/ 4.0 6)))
         (260 . ,(solarized-color-blend cyan green (/ 5.0 6)))
         (280 . ,cyan)
         (300 . ,(solarized-color-blend blue cyan (/ 2.0 5)))
         (320 . ,(solarized-color-blend blue cyan (/ 3.0 5)))
         (340 . ,(solarized-color-blend blue cyan (/ 4.0 5)))
         (360 . ,blue)))
     `(vc-annotate-very-old-color nil)
     `(vc-annotate-background nil)
;;;;; weechat
     `(weechat-color-list
       '(unspecified ,base03 ,base02
                     ,red-d ,red
                     ,green-d ,green
                     ,yellow-d ,yellow
                     ,blue-d ,blue
                     ,magenta-d ,magenta
                     ,cyan-d ,cyan
                     ,base0 ,base00)))
;;;;; xterm-color
     `(xterm-color-names [,base02 ,red ,green ,yellow
                                  ,blue ,magenta ,cyan ,base2])
     `(xterm-color-names-bright [,base03 ,orange ,base01 ,base00
                                         ,base0 ,violet ,base1 ,base3])
;;; Setup End
     (when childtheme
       (funcall childtheme))
     ) ; END custom-theme-set-variables
  )    ; END defun create-solarized-theme

;;; Footer

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'solarized)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; indent-tabs-mode: nil
;; fill-column: 95
;; End:
;;; solarized.el ends here
