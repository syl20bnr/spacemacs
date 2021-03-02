;;; packages.el --- fasd Layer packages File for Spacemacs
;;
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst fasd-packages
  '((helm-fasd :requires helm
               :location (recipe :repo "ajsalminen/helm-fasd"
                                 :fetcher github
                                 :files ("*.el")))
    (fasd :toggle (not (configuration-layer/layer-used-p 'helm)))))

(defun fasd/init-fasd ()
  "initializes fasd-emacs and adds a key binding to `SPC f z'"
  (use-package fasd
    :init
    (progn
      (defun fasd-find-file-only ()
        (interactive)
        (fasd-find-file -1))

      (defun fasd-find-directory-only ()
        (interactive)
        (fasd-find-file 2))

      (global-fasd-mode 1)
      (spacemacs/declare-prefix "fa" "fasd-find")
      (spacemacs/set-leader-keys "fad" 'fasd-find-directory-only)
      (spacemacs/set-leader-keys "faf" 'fasd-find-file-only)
      (spacemacs/set-leader-keys "fas" 'fasd-find-file)

      (when (configuration-layer/layer-used-p 'ivy)
        (ivy-set-actions
         'fasd-find-file
         '(("o" fasd-find-file-action "find-file")
           ("s" ivy-search-from-action "search-from"))))

      ;; we will fall back to using the default completing-read function, which is helm once helm is loaded.
      (setq fasd-completing-read-function 'nil))))

(defun fasd/init-helm-fasd ()
  "initializes fasd-emacs and adds a key binding to `SPC f z'"
  (use-package helm-fasd
    :init
    (progn
      (spacemacs/declare-prefix "fa" "fasd find")
      (spacemacs/set-leader-keys "fad" #'helm-fasd-directories)
      (spacemacs/set-leader-keys "faf" #'helm-fasd-files)
      (spacemacs/set-leader-keys "fas" #'helm-fasd))))
