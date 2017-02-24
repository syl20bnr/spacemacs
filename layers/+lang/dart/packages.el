;;; packages.el --- Dart Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq dart-packages
  '(
    flycheck
    (dart-mode :location (recipe
      :fetcher github
      :repo "averrin/dart-mode")
    )
    (company-dart
     :toggle (configuration-layer/package-usedp 'company)
     :location (recipe
      :fetcher github
      :repo "averrin/company-dart"
      )
    )
  )
)


(defun dart/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'dart-mode))

(defun dart/init-company-dart ()
  (use-package company-dart
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-dart
            :modes dart-mode)))

(defun dart/init-dart-mode ()
  (use-package dart-mode
    :defer t
    :init
    (progn
      (add-hook 'dart-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
      )
    :config
    (progn
        (spacemacs/set-leader-keys
        ;; (spacemacs/set-leader-keys-for-major-mode 'dart-mode
          "mj" 'dart-jump-to-defn
          "mq" 'dart-quick-fix
          "md" 'dart-jump-to-defn
          "mf" 'dartfmt
          "mi" 'dart-imports
          "ms" 'dart-sort-members
          )
    )
))
