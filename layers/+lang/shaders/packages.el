;;; packages.el --- shaders layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Alan Love <alan@cattes.us>
;; URL: https://github.com/ell
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GLPv3
(setq shaders-packages
      '(glsl-mode
<<<<<<< HEAD
        (company-glsl :location (recipe
                                 :fetcher github
                                 :repo "Kaali/company-glsl")
                      :toggle (and (configuration-layer/package-usedp 'company)
                                   (executable-find "glslangValidator")))))
=======
        company
        (company-glsl
         :location (recipe
                    :fetcher github
                    :repo "Kaali/company-glsl")
         :toggle (and (configuration-layer/package-usedp 'company)
                      (executable-find "glslangValidator")))))

(defun shaders/post-init-company ()
  (spacemacs|add-company-hook glsl-mode))
>>>>>>> bd7ef98e4c35fd87538dd2a81356cc83f5fd02f3

(defun shaders/init-company-glsl ()
  (use-package company-glsl
    :defer t
<<<<<<< HEAD
    :init (spacemacs|add-company-backends
            :backends company-glsl
            :modes glsl-mode)))
=======
    :init
    (push 'company-glsl
          company-backends-glsl-mode)))
>>>>>>> bd7ef98e4c35fd87538dd2a81356cc83f5fd02f3

(defun shaders/init-glsl-mode ()
  "Initialize GLSL mode"
  (use-package glsl-mode
    :mode (("\\.fsh\\'"  . glsl-mode)
           ("\\.vsh\\'"  . glsl-mode))))
