;;; packages.el --- graphviz layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: luxbock <opieppo@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst graphviz-packages
  '(
    graphviz-dot-mode
    org
    smartparens
    ))

(defun graphviz/init-graphviz-dot-mode ()
  (use-package graphviz-dot-mode
    :mode (("\\.diag\\'"      . graphviz-dot-mode)
           ("\\.blockdiag\\'" . graphviz-dot-mode)
           ("\\.nwdiag\\'"    . graphviz-dot-mode)
           ("\\.rackdiag\\'"  . graphviz-dot-mode)
           ("\\.dot\\'"       . graphviz-dot-mode)
           ("\\.gv\\'"        . graphviz-dot-mode))
    :init (setq graphviz-dot-indent-width tab-width)
    :config
    (progn
      (space-macs|add-toggle graphviz-live-reload
        :status graphviz-dot-auto-preview-on-save
        :on (graphviz-turn-on-live-preview)
        :off (graphviz-turn-off-live-preview)
        :documentation "Enable Graphviz live reload.")
      (space-macs/set-leader-keys-for-major-mode 'graphviz-dot-mode
        "=" 'graphviz-dot-indent-graph
        "c" 'compile
        "t" 'space-macs/toggle-graphviz-live-reload)
      (when dotspace-macs-major-mode-e-macs-leader-key
        (space-macs/set-leader-keys-for-major-mode 'graphviz-dot-mode
          dotspace-macs-major-mode-e-macs-leader-key 'graphviz-dot-preview))
      (when dotspace-macs-major-mode-leader-key
        (space-macs/set-leader-keys-for-major-mode 'graphviz-dot-mode
          dotspace-macs-major-mode-leader-key 'graphviz-dot-preview)))))

(defun graphviz/pre-init-smartparens ()
  (space-macs|use-package-add-hook graphviz-dot-mode
    :post-config
    (progn
      ;; allow smartparens to work properly
      (define-key graphviz-dot-mode-map "{" nil)
      (define-key graphviz-dot-mode-map "}" nil))))

(defun graphviz/pre-init-org ()
  (space-macs|use-package-add-hook org
    :post-config
    (progn
      (add-to-list 'org-babel-load-languages '(dot . t))
      ;; replace fundamental mode by graphiz one
      (setq org-src-lang-modes
            (append '(("dot" . graphviz-dot))
                    (delete '("dot" . fundamental) org-src-lang-modes))))))


