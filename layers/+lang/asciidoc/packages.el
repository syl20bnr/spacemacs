;;; packages.el --- Asciidoc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Mark Safronov <hijarian@gmail.com>
;; Author: Torben Hoffmann <torben.lehoff@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq asciidoc-packages '(adoc-mode))

(defun asciidoc/init-adoc-mode ()
  (use-package adoc-mode
    ;; We will NOT default `.txt' files to AsciiDoc mode,
    ;; and `.asciidoc' extension is just plain stupid.
    :mode (("\\.adoc?\\'" . adoc-mode))
		:defer t
    :config
    (progn
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
      (define-key adoc-mode-map (kbd "M-l") 'adoc-promote))))
