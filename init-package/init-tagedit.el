(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-experimental-features)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

