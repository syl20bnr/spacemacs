(use-package centered-cursor-mode
  :commands global-centered-cursor-mode
  :config
  (custom-set-variables
   '(ccm-recenter-at-end-of-file t)
   '(ccm-ignored-commands (quote (mouse-drag-region
                                  mouse-set-point
                                  widget-button-click
                                  scroll-bar-toolkit-scroll
                                  evil-mouse-drag-region))))
)
