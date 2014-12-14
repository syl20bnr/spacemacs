(defvar c-c++-packages
  '(
    cc-mode
    cmake-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    (progn
      (add-hook 'c-mode-hook '(lambda () (c-toggle-auto-state t)))
      (add-hook 'c++-mode-hook '(lambda () (c-toggle-auto-state t))))))

(defun c-c++/init-cmake-mode ()
(use-package cmake-mode
  :defer t
  :init
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist))))
