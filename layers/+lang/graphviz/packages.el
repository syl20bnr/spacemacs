;;; packages.el --- graphviz layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: luxbock <opieppo@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
        dotspacemacs-major-mode-leader-key 'graphviz-dot-preview))))

(defun graphviz/pre-init-smartparens ()
  (spacemacs|use-package-add-hook graphviz-dot-mode
    :post-config
    (progn
    ;; allow smartparens to work properly
      (define-key graphviz-dot-mode-map "{" nil)
      (define-key graphviz-dot-mode-map "}" nil))))

(defun graphviz/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      ;; replace fundamental mode by graphiz one
      (add-to-list 'org-babel-load-languages '(dot . t))
      (setq org-src-lang-modes
            (append '(("dot" . graphviz-dot))
                     (delete '("dot" . fundamental) org-src-lang-modes))))))
