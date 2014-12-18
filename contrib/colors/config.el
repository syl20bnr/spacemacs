;; ---------------------------------------------------------------------------
;; Prefixes 
;; ---------------------------------------------------------------------------

(setq spacemacs/key-binding-prefixes '(("C" .  "colors")
                                       ("Ci" . "colors-identifiers")
                                       ("tC" . "toggles-colors")))
(mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
      spacemacs/key-binding-prefixes)
