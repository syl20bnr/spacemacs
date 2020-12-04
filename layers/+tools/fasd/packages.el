(setq fasd-packages
      '((helm-fasd :requires helm
                   :location (recipe :repo "ajsalminen/helm-fasd"
                                     :fetcher github
                                     :files ("*.el")))
        (fasd :toggle (not (configuration-layer/layer-used-p 'helm)))))



(defun fasd/init-fasd ()
  "initializes fasd-e-macs and adds a key binding to `SPC f z'"
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
      (space-macs/declare-prefix "fa" "fasd-find")
      (space-macs/set-leader-keys "fad" 'fasd-find-directory-only)
      (space-macs/set-leader-keys "faf" 'fasd-find-file-only)
      (space-macs/set-leader-keys "fas" 'fasd-find-file)

      ;; we will fall back to using the default completing-read function, which is helm once helm is loaded.
      (setq fasd-completing-read-function 'nil))))

(defun fasd/init-helm-fasd ()
  "initializes fasd-e-macs and adds a key binding to `SPC f z'"
  (use-package helm-fasd
    :init
    (progn
      (space-macs/declare-prefix "fa" "fasd find")
      (space-macs/set-leader-keys "fad" #'helm-fasd-directories)
      (space-macs/set-leader-keys "faf" #'helm-fasd-files)
      (space-macs/set-leader-keys "fas" #'helm-fasd))))


