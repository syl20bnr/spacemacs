;;; packages.el --- Perl5 Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troyhinckley@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq perl5-packages
      '(
        (company-plsense :requires company)
        (cperl-mode :location built-in)
        flycheck
        org
        realgud
        smartparens
        ))

(defun perl5/init-company-plsense ()
  (use-package company-plsense
    :defer t
    :init
    (spacemacs|add-company-backends
     :backends company-plsense
     :modes cperl-mode)))

(defun perl5/init-cperl-mode ()
  (use-package cperl-mode
    :defer t
    :mode "\\.\\(p[lm]x?\\|P[LM]X?\\)\\'"
    :interpreter "perl"
    :interpreter "perl5"

    :init
    (progn
      (setq
       ;; highlight all scalar variables not just the instantiation
       cperl-highlight-variables-indiscriminately t
       cperl-indent-level 4        ; 4 spaces is the standard indentation
       cperl-close-paren-offset -4 ; indent the closing paren back four spaces
       cperl-continued-statement-offset 4 ; if a statement continues indent it to four spaces
       cperl-indent-parens-as-block t)) ; parentheses are indented with the block and not with scope

    :config
    (progn
      ;; Don't highlight arrays and hashes in comments
      (font-lock-remove-keywords
       'cperl-mode
       '(("\\(\\([@%]\\|\\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
          (if (eq (char-after (match-beginning 2)) 37)
              'cperl-hash-face 'cperl-array-face) t)
         ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)" 1
          (if (= (- (match-end 2) (match-beginning 2)) 1)
              (if (eq (char-after (match-beginning 3)) 123)
                  'cperl-hash-face 'cperl-array-face)
            font-lock-variable-name-face) t)
         ("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
          (2 font-lock-string-face t)
          ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}" nil nil
           (1 font-lock-string-face t)))
         ("[[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1 font-lock-string-face t)))

      (font-lock-add-keywords
       'cperl-mode
       '(("\\(\\([@%]\\|\\$#\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)" 1
          (if (nth 4 (syntax-ppss))
              'font-lock-comment-face
            (if (eq (char-after (match-beginning 2)) ?%)
                'cperl-hash-face
              'cperl-array-face)) t)
         ("\\(\\([$@]+\\)[a-zA-Z_:][a-zA-Z0-9_:]*\\)[ \t]*\\([[{]\\)" 1
          (if (nth 4 (syntax-ppss))
              'font-lock-comment-face
            (if (= (- (match-end 2) (match-beginning 2)) 1)
                (if (eq (char-after (match-beginning 3)) ?{)
                    'cperl-hash-face
                  'cperl-array-face)
              font-lock-variable-name-face)) t)
         ("\\([]}\\\\%@>*&]\\|\\$[a-zA-Z0-9_:]*\\)[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}"
          (2 (if (nth 4 (syntax-ppss))
                 'font-lock-comment-face
               'font-lock-string-face) t)
          ("\\=[ \t]*{[ \t]*\\(-?[a-zA-Z0-9_:]+\\)[ \t]*}" nil nil
           (1 (if (nth 4 (syntax-ppss))
                  'font-lock-comment-face
                'font-lock-string-face) t)))
         ("[[ \t{,(]\\(-?[a-zA-Z0-9_:]+\\)[ \t]*=>" 1
          (if (nth 4 (syntax-ppss))
              'font-lock-comment-face
            'font-lock-string-face) t)))

      ;; Use less horrible colors for cperl arrays and hashes
      (set-face-attribute 'cperl-array-face nil
                          :foreground  "#DD7D0A"
                          :background  'unspecified
                          :weight 'unspecified)
      (set-face-attribute 'cperl-hash-face nil
                          :foreground "OrangeRed3"
                          :background 'unspecified
                          :weight 'unspecified)

      ;; tab key will ident all marked code when tab key is pressed
      (add-hook 'cperl-mode-hook
                (lambda () (local-set-key (kbd "<tab>") 'indent-for-tab-command)))

      (spacemacs/declare-prefix-for-mode 'cperl-mode "m=" "format")
      (spacemacs/declare-prefix-for-mode 'cperl-mode "mg" "find-symbol")
      (spacemacs/declare-prefix-for-mode 'cperl-mode "mh" "perldoc")
      (spacemacs/set-leader-keys-for-major-mode 'cperl-mode
        "==" 'spacemacs/perltidy-format
        "=b" 'spacemacs/perltidy-format-buffer
        "=f" 'spacemacs/perltidy-format-function
        "hh" 'cperl-perldoc-at-point
        "hd" 'cperl-perldoc
        "v" 'cperl-select-this-pod-or-here-doc)

      (font-lock-add-keywords 'cperl-mode
                      '(("\\_<say\\_>" . cperl-nonoverridable-face))))))

(defun perl5/post-init-flycheck ()
  (spacemacs/enable-flycheck 'cperl-mode))

(defun perl5/post-init-realgud()
  (spacemacs/add-realgud-debugger 'cperl-mode "trepan.pl"))

(defun perl5/post-init-smartparens ()
  ;; fixs a bug with electric mode and smartparens https://github.com/syl20bnr/spacemacs/issues/480
  (with-eval-after-load 'cperl-mode
    (add-hook 'smartparens-enabled-hook 'spacemacs//perl5-smartparens-enable)
    (add-hook 'smartparens-disabled-hook 'spacemacs//perl5-spartparens-disable)))

(defun perl5/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(perl . t))))
