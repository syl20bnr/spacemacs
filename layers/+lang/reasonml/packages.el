;;; packages.el --- reasonml layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Fredrik Dyrkell
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst reasonml-packages
  '(company
    evil-matchit
    flycheck
    flycheck-ocaml
    merlin
    popwin
    reason-mode
    utop))

(defun reasonml/post-init-company ()
  (when (configuration-layer/package-usedp 'merlin)
    (spacemacs|add-company-backends
      :backends merlin-company-backend
      :modes reason-mode)))

(defun reasonml/post-init-evil-matchit ()
  (evilmi-load-plugin-rules '(reason-mode) '(template simple html))
  (add-hook 'reason-mode-hook 'turn-on-evil-matchit-mode))

(defun flycheck-ocaml-reason-setup ()
  (with-eval-after-load 'merlin
    (setq merlin-error-after-save nil)

    (flycheck-define-generic-checker 'reason-merlin
      "A syntax checker for Reason using Merlin Mode.
    See URL `https://github.com/the-lambda-church/merlin'."
      :start #'flycheck-ocaml-merlin-start
      :verify #'flycheck-verify-ocaml-merlin
      :modes '(reason-mode)
      :predicate (and merlin-mode
                      ;; Don't check if Merlin's own checking is
                      ;; enabled, to avoid duplicate overlays
                      (not merlin-error-after-save)))

    (interactive)
    (add-to-list 'flycheck-checkers 'reason-merlin)))

(defun reasonml/post-init-flycheck ()
  (when (configuration-layer/layer-used-p 'syntax-checking)
    (spacemacs/enable-flycheck 'reason-mode)))

(defun reasonml/post-init-flycheck-ocaml ()
  (when (configuration-layer/layer-used-p 'syntax-checking)
    (flycheck-ocaml-reason-setup)))

(defun reasonml/post-init-merlin ()
  (use-package merlin
    :defer t
    :init
    (progn
      (setq merlin-completion-with-doc t)

      (spacemacs/set-leader-keys-for-major-mode 'reason-mode
        "cp" 'merlin-project-check
        "cv" 'merlin-goto-project-file
        "eC" 'merlin-error-check
        "en" 'merlin-error-next
        "eN" 'merlin-error-prev
        "gb" 'merlin-pop-stack
        "gg" 'merlin-locate
        "gG" 'spacemacs/merlin-locate-other-window
        "gl" 'merlin-locate-ident
        "gi" 'merlin-switch-to-ml
        "gI" 'merlin-switch-to-mli
        "go" 'merlin-occurrences
        "hh" 'merlin-document
        "ht" 'merlin-type-enclosing
        "hT" 'merlin-type-expr
        "rd" 'merlin-destruct))))

(defun reasonml/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*Refmt Errors*" :tail t :position bottom :noselect t)
          popwin:special-display-config)))

(defun reasonml/init-reason-mode ()
  (use-package reason-mode
    :defer t
    :mode ("\\.rei?\\'" . reason-mode)
    :init
    (progn
      (add-hook 'reason-mode-hook 'merlin-mode)
      (add-hook 'reason-mode-hook 'utop-minor-mode)
      (when (configuration-layer/layer-used-p 'syntax-checking)
        (add-hook 'reason-mode-hook 'flycheck-mode))

      (add-hook 'reason-mode-hook
                (lambda ()
                  (when reason-auto-refmt
                    (add-hook 'before-save-hook 'refmt nil t))))

      (spacemacs|add-toggle reason-auto-refmt
        :documentation "Toggle automatic refmt on save."
        :status reason-auto-refmt
        :on (progn
              (setq reason-auto-refmt t)
              (add-hook 'before-save-hook 'refmt nil t))
        :off (progn
               (setq reason-auto-refmt nil)
               (remove-hook 'before-save-hook 'refmt t))))

    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'reason-mode "mc" "compile")
      (spacemacs/declare-prefix-for-mode 'reason-mode "mt" "toggle")
      (spacemacs/declare-prefix-for-mode 'reason-mode "me" "errors/eval")
      (spacemacs/declare-prefix-for-mode 'reason-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'reason-mode "mh" "help/show")
      (spacemacs/declare-prefix-for-mode 'reason-mode "mr" "refactor")
      (spacemacs/declare-prefix-for-mode 'reason-mode "m=" "refmt")

      (spacemacs/set-leader-keys-for-major-mode 'reason-mode
        "cr" 'refmt
        "==" 'refmt
        "tr" 'spacemacs/toggle-reason-auto-refmt
        "=mr" 'reason/refmt-ml-to-re
        "=rm" 'reason/refmt-re-to-ml))))

(defun reasonml/pre-init-utop ()
  (spacemacs|use-package-add-hook utop
    :post-init
    (add-hook
     'reason-mode-hook
     (lambda ()
       (setq utop-command "rtop -emacs")
       (setq utop-edit-command nil)
       (setq utop-prompt 'reason/rtop-prompt)
       (setq utop-initial-command "let myVar = \"Hello Reason!\";")
       (setq utop-phrase-terminator ";")))
    :post-config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'reason-mode
        "er" 'utop-eval-region
        "eb" 'utop-eval-buffer
        "ee" 'utop-eval-phrase))))

;;; packages.el ends here
