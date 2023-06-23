;;; packages.el --- Asciidoc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Mark Safronov <hijarian@gmail.com>
;; Author: Torben Hoffmann <torben.lehoff@gmail.com>
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


(setq asciidoc-packages '(adoc-mode))

(defun asciidoc/init-adoc-mode ()
  (use-package adoc-mode
    ;; We will NOT default `.txt' files to AsciiDoc mode,
    ;; and `.asciidoc' extension is just plain stupid.
    :mode (("\\.adoc?\\'" . adoc-mode))
		:defer t
    :config
    ;; We have quite a lot of possible keybindings.
    ;; See `adoc-mode.el', its bottom part where the huge easy-menu
    ;; is defined and after that, where the various `tempo-template-*'
    ;; functions are defined.

    ;; See /doc/CONVENTIONS.md#plain-text-markup-languages
    (spacemacs/set-leader-keys-for-major-mode 'adoc-mode
      "h1" 'tempo-template-adoc-title-1
      ;; Alternative method of inserting top-level heading
      "hI" 'tempo-template-adoc-title-1
      "h2" 'tempo-template-adoc-title-2
      ;; Alternative method of inserting the most usual heading
      "hi" 'tempo-template-adoc-title-2
      "h3" 'tempo-template-adoc-title-3
      "h4" 'tempo-template-adoc-title-4
      "h5" 'tempo-template-adoc-title-5
      "xb" 'tempo-template-adoc-strong
      "xi" 'tempo-template-adoc-emphasis)
    ;; yes, exactly like that. To "promote" title is to INCREASE its size.
    ;; `adoc-demote' does the opposite: increases its LEVEL,
    ;; which DECREASES its size.
    (define-key adoc-mode-map (kbd "M-h") 'adoc-demote)
    ;; see the comment about  adoc-demote above
    (define-key adoc-mode-map (kbd "M-l") 'adoc-promote)))
