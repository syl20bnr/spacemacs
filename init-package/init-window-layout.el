(require 'window-layout)

;; five windows upper right main
(setq wm-recipe-1
      '(| (:left-size-ratio 0.33)
        (- (:upper-size-ratio 0.5) first second)
        (- (:upper-size-ratio 0.5) main
           (| (:left-size-ratio 0.5) third fourth))))
(setq wm-info-1
      '((:name main)
        (:name first)
        (:name second)
        (:name third)
        (:name fourth)))

;; five windows center main
(setq wm-recipe-2
      '(| (:left-size-ratio 0.32)
          (- (:upper-size-ratio 0.5) first second)
          (| (:left-size-ratio 0.51) main
             (- (:upper-size-ratio 0.5) third fourth))))
(setq wm-info-2
      '((:name main)
        (:name first)
        (:name second)
        (:name third)
        (:name fourth)))

;; (setq my-default-layout wm-five-upright)
;; (wlf:refresh my-default-layout)
;; (wlf:select my-default-layout 'main)
