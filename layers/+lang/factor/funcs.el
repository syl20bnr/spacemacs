(defun factor//fuel-stack-effect ()
  "Small wrapper around factors stack effect help. If region is
  active, use that, otherwise use sexp under point."
  (interactive)
  (if (region-active-p)
      (call-interactively 'fuel-stack-effect-region)
    (call-interactively 'fuel-stack-effect-sexp)))
