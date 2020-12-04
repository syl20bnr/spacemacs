(defun dotspace-macs/layers ()
  (setq-default
   dotspace-macs-distribution 'space-macs
   dotspace-macs-configuration-layers '(
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
(defun dotspace-macs/init ())
(defun dotspace-macs/user-init ())
(defun dotspace-macs/config ())
(defun dotspace-macs/user-config ())


