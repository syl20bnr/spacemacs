(setq phpplus-packages
      '(
        company
        flycheck
        company-php
        php-mode
        ))
(defun phpplus/post-init-company ()
  (spacemacs|add-company-hook php-mode)
  )


(defun phpplus/post-init-company-php ()

  (push 'company-ac-php-backend  company-backends-php-mode)
  )



(defun phpplus/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'php-mode))



(defun phpplus/init-php-mode ()
  (use-package php-mode
    :defer t
    :mode ("\\.php\\'" . php-mode)))

(defun phpplus/init-company-php ()
  (use-package company-php
    :defer t
    )
  )
