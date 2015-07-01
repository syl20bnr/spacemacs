;;; extensions.el --- Github Layer Extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq github-post-extensions '(magit-gh-pulls))

(defun github/init-magit-gh-pulls ()
  (use-package magit-gh-pulls
    :commands magit-gh-pulls-mode
    :init
    (progn
      (defun spacemacs/load-gh-pulls-mode ()
        "Start `magit-gh-pulls-mode' only after a manual request."
        (interactive)
        (magit-gh-pulls-mode)
        (magit-gh-pulls-reload))
      (defun spacemacs/fetch-gh-pulls-mode ()
        "Start `magit-gh-pulls-mode' only after a manual request."
        (interactive)
        (magit-gh-pulls-mode)
        (magit-gh-pulls-fetch-commits))
      (eval-after-load 'magit
        '(progn
           (define-key magit-mode-map "#gg" 'spacemacs/load-gh-pulls-mode)
           (define-key magit-mode-map "#gf" 'spacemacs/fetch-gh-pulls-mode))))
    :config
    (spacemacs|diminish magit-gh-pulls-mode "Github-PR")))
