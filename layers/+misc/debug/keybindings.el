(spacemacs|define-transient-state debug
  :title "Debug Transient State"
  :doc "
 [_n_]^^^^      next            [_i_]^^    nexti
 [_b_]^^^^      break           [_x_]^^    remove
 [_c_]^^^^      continue        [_r_]^^    run
 [_w_]^^^^      watch"
  :bindings
  ("n" gud-next)
  ("i" gud-nexti)
  ("b" gud-break)
  ("x" gud-remove)
  ("c" gud-cont)
  ("r" gud-run)
  ("w" gud-watch)
  ("q" nil :exit t))
(spacemacs/declare-prefix "d" "debug")
(spacemacs/set-leader-keys "dd" 'spacemacs/debug-enable)
(spacemacs/set-leader-keys "dn" 'gud-next)
(spacemacs/set-leader-keys "di" 'gud-nexti)
(spacemacs/set-leader-keys "db" 'gud-break)
(spacemacs/set-leader-keys "dx" 'gud-remove)
(spacemacs/set-leader-keys "dc" 'gud-cont)
(spacemacs/set-leader-keys "dr" 'gud-run)
(spacemacs/set-leader-keys "dw" 'gud-watch)
(spacemacs/set-leader-keys "d." 'spacemacs/debug-transient-state/body)
