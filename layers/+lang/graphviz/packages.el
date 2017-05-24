;;; packages.el --- graphviz layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: luxbock <opieppo@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst graphviz-packages
  '((graphviz-dot-mode :location (recipe :fetcher github
                                         :repo "luxbock/graphviz-dot-mode"))
    org
    smartparens))

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
      (spacemacs|add-toggle graphviz-live-reload
        :status graphviz-dot-auto-preview-on-save
        :on (graphviz-turn-on-live-preview)
        :off (graphviz-turn-off-live-preview)
        :documentation "Enable Graphviz live reload.")
      (spacemacs/set-leader-keys-for-major-mode 'graphviz-dot-mode
        "=" 'graphviz-dot-indent-graph
        "c" 'compile
        "t" 'spacemacs/toggle-graphviz-live-reload)
      (when dotspacemacs-major-mode-emacs-leader-key
        (spacemacs/set-leader-keys-for-major-mode 'graphviz-dot-mode
          dotspacemacs-major-mode-emacs-leader-key 'graphviz-dot-preview))
      (when dotspacemacs-major-mode-leader-key
        (spacemacs/set-leader-keys-for-major-mode 'graphviz-dot-mode
          dotspacemacs-major-mode-leader-key 'graphviz-dot-preview)))))

(defun graphviz/post-init-smartparens ()
  (spacemacs|use-package-add-hook graphviz-dot-mode
    :post-config
    (progn
      ;; allow smartparens to work properly
      (define-key graphviz-dot-mode-map "{" nil)
      (define-key graphviz-dot-mode-map "}" nil))))

(defun graphviz/post-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (add-to-list 'org-babel-load-languages '(dot . t))
      ;; replace fundamental mode by graphiz one
      (setq org-src-lang-modes
            (append '(("dot" . graphviz-dot))
                    (delete '("dot" . fundamental) org-src-lang-modes))))))
