;;; packages.el --- Auto-completion Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(setq auto-completion-packages
      '(
        auto-yasnippet
        auto-complete
        ac-ispell
        company
        (company-box :toggle auto-completion-use-company-box)
        (all-the-icons :toggle auto-completion-use-company-box)
        (company-quickhelp :toggle auto-completion-enable-help-tooltip)
        (company-statistics :toggle auto-completion-enable-sort-by-usage)
        counsel
        fuzzy
        (helm-company :requires helm)
        (helm-c-yasnippet :requires helm)
        hippie-exp
        (ivy-yasnippet :requires ivy)
        smartparens
        yasnippet
        yasnippet-snippets))


;; TODO replace by company-ispell which comes with company
;; to be moved to spell-checking layer as well
(defun auto-completion/init-ac-ispell ()
  (use-package ac-ispell
    :defer t
    :init
    (progn
      (setq ac-ispell-requires 4)
      (with-eval-after-load 'auto-complete
        (ac-ispell-setup)))))
;; (add-hook 'markdown-mode-hook 'ac-ispell-ac-setup)


(defun auto-completion/init-auto-complete ()
  (use-package auto-complete
    :defer t
    :init
    (setq ac-auto-start 0
          ac-delay auto-completion-idle-delay
          ac-quick-help-delay 1.
          ac-use-fuzzy t
          ac-fuzzy-enable t
          ac-comphist-file (concat spacemacs-cache-directory "ac-comphist.dat")
          ;; use 'complete when auto-complete is disabled
          tab-always-indent 'complete
          ac-dwim t)
    :config
    (progn
      (require 'auto-complete-config)
      (setq-default ac-sources '(ac-source-abbrev
                                 ac-source-dictionary
                                 ac-source-words-in-same-mode-buffers))
      (when (configuration-layer/package-used-p 'yasnippet)
        (add-to-list 'ac-sources 'ac-source-yasnippet))
      (add-to-list 'completion-styles 'initials t)
      (define-key ac-completing-map (kbd "C-j") 'ac-next)
      (define-key ac-completing-map (kbd "C-k") 'ac-previous)
      (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
      (spacemacs|diminish auto-complete-mode " ⓐ" " a"))))

(defun auto-completion/init-auto-yasnippet ()
  (use-package auto-yasnippet
    :defer t
    :init
    (progn
      (setq aya-persist-snippets-dir
            (or auto-completion-private-snippets-directory
                (concat spacemacs-private-directory "snippets/")))
      (spacemacs/declare-prefix "iS" "auto-yasnippet")
      (spacemacs/set-leader-keys
        "iSc" 'aya-create
        "iSe" 'spacemacs/auto-yasnippet-expand
        "iSw" 'aya-persist-snippet))))

(defun auto-completion/init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay auto-completion-idle-delay
            company-minimum-prefix-length auto-completion-minimum-prefix-length
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil)

      (add-hook 'company-completion-started-hook 'company-turn-off-fci)
      (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
    :config
    (progn
      (spacemacs|diminish company-mode " ⓐ" " a")

      ;; key bindings
      (defun spacemacs//company-complete-common-or-cycle-backward ()
        "Complete common prefix or cycle backward."
        (interactive)
        (company-complete-common-or-cycle -1))
      (spacemacs//auto-completion-set-RET-key-behavior 'company)
      (spacemacs//auto-completion-set-TAB-key-behavior 'company)
      (spacemacs//auto-completion-setup-key-sequence 'company)

      (let ((map company-active-map))
        (define-key map (kbd "C-/")   'company-search-candidates)
        (define-key map (kbd "C-M-/") 'company-filter-candidates)
        (define-key map (kbd "C-d")   'company-show-doc-buffer))
      (add-hook 'spacemacs-editing-style-hook 'spacemacs//company-active-navigation)
      ;; ensure that the correct bindings are set at startup
      (spacemacs//company-active-navigation dotspacemacs-editing-style))))

(defun auto-completion/init-company-statistics ()
  (use-package company-statistics
    :defer t
    :init
    (progn
      (setq company-statistics-file (concat spacemacs-cache-directory
                                            "company-statistics-cache.el"))
      (add-hook 'company-mode-hook 'company-statistics-mode))))

(defun auto-completion/pre-init-counsel ()
  (spacemacs|use-package-add-hook company
    :post-config
    (define-key company-active-map (kbd "C-/") 'counsel-company)))

(defun auto-completion/init-fuzzy ()
  (use-package fuzzy :defer t))

(defun auto-completion/init-company-quickhelp ()
  (use-package company-quickhelp
    :commands company-quickhelp-manual-begin
    :init
    (spacemacs|do-after-display-system-init
     (with-eval-after-load 'company
       (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
       (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
       (unless (eq auto-completion-enable-help-tooltip 'manual)
         (company-quickhelp-mode))))))

(defun auto-completion/init-all-the-icons ()
  (use-package all-the-icons
    :after company
    :config
    (progn
      (eval-and-compile
        ;; Icons selected by liguangsheng
        ;; https://github.com/liguangsheng/emacsd/blob/master/lisp/init-completion.el
        (defun my-company-box-icon (family icon &rest args)
          "Defines icons using `all-the-icons' for `company-box'."
          (when icon
            (let ((icon (pcase family
                          ('octicon (all-the-icons-octicon icon :height 0.8 :v-adjust -0.05 args))
                          ('faicon (all-the-icons-faicon icon :height 0.8 :v-adjust -0.0575))
                          ('material (all-the-icons-material icon :height 0.8 :v-adjust -0.225 args))
                          ('alltheicon (all-the-icons-alltheicon icon :height 0.8 args)))))
              (unless (symbolp icon)
                (concat icon
                        (propertize " " 'face 'variable-pitch)))))))
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(my-company-box-icon 'octicon "file-text"))
              (Text . ,(my-company-box-icon 'faicon "file-text-o"))
              (Method . ,(my-company-box-icon 'faicon "cube"))
              (Function . ,(my-company-box-icon 'faicon "cube"))
              (Constructor . ,(my-company-box-icon 'faicon "cube"))
              (Field . ,(my-company-box-icon 'faicon "tag"))
              (Variable . ,(my-company-box-icon 'faicon "tag"))
              (Class . ,(my-company-box-icon 'faicon "cog"))
              (Interface . ,(my-company-box-icon 'faicon "cogs"))
              (Module . ,(my-company-box-icon 'alltheicon "less"))
              (Property . ,(my-company-box-icon 'faicon "wrench"))
              (Unit . ,(my-company-box-icon 'faicon "tag"))
              (Value . ,(my-company-box-icon 'faicon "tag"))
              (Enum . ,(my-company-box-icon 'faicon "file-text-o"))
              (Keyword . ,(my-company-box-icon 'material "format_align_center"))
              (Snippet . ,(my-company-box-icon 'material "content_paste"))
              (Color . ,(my-company-box-icon 'material "palette"))
              (File . ,(my-company-box-icon 'faicon "file"))
              (Reference . ,(my-company-box-icon 'faicon "tag"))
              (Folder . ,(my-company-box-icon 'faicon "folder"))
              (EnumMember . ,(my-company-box-icon 'faicon "tag"))
              (Constant . ,(my-company-box-icon 'faicon "tag"))
              (Struct . ,(my-company-box-icon 'faicon "cog"))
              (Event . ,(my-company-box-icon 'faicon "bolt"))
              (Operator . ,(my-company-box-icon 'faicon "tag"))
              (TypeParameter . ,(my-company-box-icon 'faicon "cog"))
              (Template . ,(my-company-box-icon 'octicon "file-code")))))))

(defun auto-completion/init-company-box ()
  (use-package company-box
    :hook '(company-mode . company-box-mode)
    :commands 'company-box-doc-manually
    :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (progn
      (spacemacs|hide-lighter company-box-mode)
      (setq company-box-backends-colors nil
            company-box-max-candidates 1000
            company-box-doc-enable nil)
      (add-hook 'company-box-selection-hook
                (lambda (selection frame) (company-box-doc--hide frame)))
      (cl-case auto-completion-enable-help-tooltip
        ('manual (define-key company-active-map
                   (kbd "M-h") #'company-box-doc-manually))
        ('t (setq company-box-doc-enable t))))))

(defun auto-completion/init-helm-c-yasnippet ()
  (use-package helm-c-yasnippet
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "is" 'spacemacs/helm-yas)
      (setq helm-yas-space-match-any-greedy t))))

(defun auto-completion/pre-init-helm-company ()
  (spacemacs|use-package-add-hook company
    :post-config
    (use-package helm-company
      :defer t
      :init
      (define-key company-active-map (kbd "C-/") 'helm-company))))
(defun auto-completion/init-helm-company ())

(defun auto-completion/init-hippie-exp ()
  ;; replace dabbrev-expand
  (global-set-key (kbd "M-/") 'hippie-expand)
  (define-key evil-insert-state-map [remap evil-complete-previous] 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol))
  (when (configuration-layer/package-used-p 'yasnippet)
    ;; Try to expand yasnippet snippets based on prefix
    (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)))

(defun auto-completion/init-ivy-yasnippet ()
  (use-package ivy-yasnippet
    :defer t
    :init
    (progn
      (setq ivy-yasnippet-expand-keys nil)
      (spacemacs/set-leader-keys "is" 'spacemacs/ivy-yas))))

(defun auto-completion/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (add-hook 'yas-before-expand-snippet-hook
              #'spacemacs//smartparens-disable-before-expand-snippet)
    (add-hook 'yas-after-exit-snippet-hook
              #'spacemacs//smartparens-restore-after-exit-snippet)))

(defun auto-completion/init-yasnippet ()
  (use-package yasnippet
    :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
    :init
    (progn
      ;; We don't want undefined variable errors
      (defvar yas-global-mode nil)
      (setq yas-triggers-in-field t
            yas-wrap-around-region t
            helm-yas-display-key-on-candidate t)
      ;; on multiple keys, fall back to completing read
      ;; typically this means helm
      (setq yas-prompt-functions '(yas-completing-prompt))
      ;; disable yas minor mode map
      ;; use hippie-expand instead
      (setq yas-minor-mode-map (make-sparse-keymap))
      ;; this makes it easy to get out of a nested expansion
      (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
      ;; configure snippet directories
      (let* ((spacemacs--auto-completion-dir
              (configuration-layer/get-layer-local-dir 'auto-completion))
             (emacs-directory-snippets-dir (concat
                                            spacemacs-private-directory
                                            "snippets/"))
             (spacemacs-layer-snippets-dir (expand-file-name
                                            "snippets"
                                            spacemacs--auto-completion-dir))
             (dotspacemacs-directory-snippets-dir
              (when dotspacemacs-directory
                (let ((snippet-dir (expand-file-name
                                    "snippets"
                                    dotspacemacs-directory)))
                  (when (file-accessible-directory-p snippet-dir)
                    snippet-dir)))))
        (setq yas-snippet-dirs nil)
        ;; ~/.emacs.d/layers/auto-completion/snippets
        (add-to-list 'yas-snippet-dirs spacemacs-layer-snippets-dir)
        ;; ~/.emacs.d/private/snippets
        (add-to-list 'yas-snippet-dirs emacs-directory-snippets-dir)
        ;; ~/.spacemacs.d/snippets
        (when dotspacemacs-directory-snippets-dir
          (add-to-list 'yas-snippet-dirs dotspacemacs-directory-snippets-dir))
        ;; arbitrary directories in `auto-completion-private-snippets-directory'
        (when auto-completion-private-snippets-directory
          (if (listp auto-completion-private-snippets-directory)
              (setq yas-snippet-dirs (append yas-snippet-dirs auto-completion-private-snippets-directory))
            (add-to-list 'yas-snippet-dirs auto-completion-private-snippets-directory))))
      (spacemacs|add-toggle yasnippet
        :mode yas-minor-mode
        :documentation "Enable snippets."
        :evil-leader "ty")
      (spacemacs/add-to-hooks
       'spacemacs/force-yasnippet-off '(term-mode-hook
                                        shell-mode-hook
                                        eshell-mode-hook))
      (spacemacs|require-when-dumping 'yasnippet)
      (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                          markdown-mode-hook
                                                          org-mode-hook)))

    :config (spacemacs|diminish yas-minor-mode " ⓨ" " y")))

(defun auto-completion/init-yasnippet-snippets ())
