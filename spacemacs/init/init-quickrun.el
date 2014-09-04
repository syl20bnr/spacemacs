(use-package quickrun
  :disabled t
  :init
  (evil-leader/set-key
    "qba" 'quickrun-arg
    "qbc" 'quickrun-compile-only
    "qbs" 'quickrun-shell
    "qbx" 'quickrun
    "qeb" 'eval-buffer
    "qex" 'eval-last-sexp
    "qh"  'helm-quickrun
    "qrr" 'quickrun-replace-region
    "qrx" 'quickrun-region))
