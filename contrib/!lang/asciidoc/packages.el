;;; packages.el --- Asciidoc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Mark Safronov, Torben Hoffmann & Contributors
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
    :mode (("\\.adoc?$" . adoc-mode))
		:defer t
    :config
    (progn
      ;; We have quite a lot of possible keybindings.
      ;; See `adoc-mode.el', its bottom part where the huge easy-menu
      ;; is defined and after that, where the various `tempo-template-*'
      ;; functions are defined.

      ;; See /doc/CONVENTIONS.md#plain-text-markup-languages
      (evil-leader/set-key-for-mode 'adoc-mode
        "mh1" 'tempo-template-adoc-title-1
        ;; Alternative method of inserting top-level heading
        "mhI" 'tempo-template-adoc-title-1
        "mh2" 'tempo-template-adoc-title-2
        ;; Alternative method of inserting the most usual heading
        "mhi" 'tempo-template-adoc-title-2
        "mh3" 'tempo-template-adoc-title-3
        "mh4" 'tempo-template-adoc-title-4
        "mh5" 'tempo-template-adoc-title-5
        "mxb" 'tempo-template-adoc-strong
        "mxi" 'tempo-template-adoc-emphasis)
      ;; yes, exactly like that. To "promote" title is to INCREASE its size.
      ;; `adoc-denote' does the opposite: increases its LEVEL,
      ;; which DECREASES its size.
      (define-key adoc-mode-map (kbd "M-h") 'adoc-denote)
      ;; see the comment about  adoc-denote above
      (define-key adoc-mode-map (kbd "M-l") 'adoc-promote))))
