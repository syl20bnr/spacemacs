;;; packages.el --- Salt Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Ben Hayden
;;
;; Author: Ben Hayden <hayden767@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; Salt mode URL: https://github.com/beardedprojamz/salt-mode
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq salt-packages '(salt-mode
                      smartparens))

(defun salt/init-salt-mode ()
  (use-package salt-mode
    :defer t
    :config (evil-leader/set-key-for-mode 'salt-mode "mpb" 'mmm-parse-buffer)))

(defun salt/pre-init-smartparens ()
  (add-hook 'salt-mode-hook 'smartparens-mode)
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (progn
      (sp-local-pair 'salt-mode "{{" " }}")
      (sp-local-pair 'salt-mode "{%" " %}")
      (sp-local-pair 'salt-mode "{#" " #}"))))
