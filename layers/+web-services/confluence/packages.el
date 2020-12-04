;;; packages.el --- Confluence Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq confluence-packages
      '(confluence
        (ox-confluence :location built-in)
        ))

(defun confluence/init-confluence ()
  (use-package confluence
    :defer t
    :config
    (progn
      ;; remove the hook on buffer save that automatically store the buffer
      ;; in confluence, it creates a lot of useless revision in a page history.
      (advice-add 'confluence-base-mode-init
                  :after 'space-macs//confluence-remove-save-hook)
      (dolist (mode '(confluence-mode
                      confluence-xml-mode
                      confluence-search-mode))
        (space-macs/set-leader-keys-for-major-mode mode
          "s" 'space-macs/confluence-save-to-confluence-minor-edit)
        (space-macs/set-leader-keys-for-major-mode mode
          "S" 'space-macs/confluence-save-to-confluence-major-edit)
        (space-macs/set-leader-keys-for-major-mode mode
          "TAB" 'confluence-toggle-page-content-type)))))

(defun confluence/pre-init-ox-confluence ()
  (space-macs|use-package-add-hook org
    :post-config
    (progn
      (require 'ox-confluence)
      (space-macs/set-leader-keys-for-major-mode 'org-mode
        "ec" 'org-confluence-export-as-confluence))))
(defun confluence/init-ox-confluence ())


