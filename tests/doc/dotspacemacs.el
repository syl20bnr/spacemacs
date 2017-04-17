(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layers '(
                                       (org :variables
                                            org-enable-github-support t
                                            org-enable-bootstrap-support t
                                            org-enable-reveal-js-support t
                                            )
                                       bibtex
                                       (latex :variables
                                              latex-enable-auto-fill t
                                              latex-enable-folding t
                                              )
                                       html
                                       )))
(defun dotspacemacs/init ())
(defun dotspacemacs/user-init ())
(defun dotspacemacs/config ())
(defun dotspacemacs/user-config ())
