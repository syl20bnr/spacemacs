(setq protobuf-packages
      '(
        flycheck
        (flycheck-protobuf :toggle (configuration-layer/package-usedp 'flycheck))

        protobuf-mode
        ))


(defun protobuf/init-flycheck-protobuf ()
  (use-package flycheck-protobuf
    :defer t
    :init))

(defun protobuf/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'protobuf-mode))

(defun protobuf/init-protobuf-mode()
  (use-package protobuf-mode
    :defer t))

