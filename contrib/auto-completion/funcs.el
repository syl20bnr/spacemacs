;;; funcs.el --- Auto-completion functions File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|add-toggle auto-completion
                      :status
                      (if (eq 'company auto-completion-front-end)
                          company-mode
                        auto-complete-mode)
                      :on
                      (progn
                        (if (eq 'company auto-completion-front-end)
                            (company-mode)
                          (auto-complete-mode))
                        (message "Enabled %S." auto-completion-front-end))
                      :off
                      (progn
                        (if (eq 'company auto-completion-front-end)
                            (company-mode -1)
                          (auto-complete-mode -1))
                        (message "Disabled %S." auto-completion-front-end))
                      :documentation "Activate auto-completion."
                      :evil-leader "ta")

;; Company -------------------------------------------------------------------

(defmacro spacemacs|enable-company (mode &optional hook)
  "Enable company for the given MODE.
MODE must match the symbol passed in `spacemacs|init-company-backends'.
By default the initialization function is hooked to `MODE-hook', it is
possible to explicitly define a hook with HOOK."
  (when (configuration-layer/pacakge-usedp 'company)
    (let ((mode-hook (if hook hook
                       (intern (format "%S-hook" mode))))
          (func (intern (format "spacemacs//init-company-%S" mode))))
      `(progn
         (defun ,func ()
           ,(format "Initialize company for %S" mode)
           (set (make-variable-buffer-local 'auto-completion-front-end)
                'company)
           (set (make-variable-buffer-local 'company-backends)
                ,(intern (format "company-backends-%S" mode))))
         (add-hook ',mode-hook ',func)
         (add-hook ',mode-hook 'company-mode)))))

(defun spacemacs/company-backend-with-yas (backend)
  "Return BACKEND with support for yasnippet candidates."
  backend
  ;; ------------------
  ;; syl20bnr: For now adding company-snippet to some backend like anaconda
  ;; has weird side effects, I need to investigate a little more on this
  ;; ------------------
  ;; (if (and (configuration-layer/package-usedp 'yasnippet)
  ;;          auto-completion-enable-company-yasnippet
  ;;          (not (eq 'company-semantic backend)))
  ;;     (unless (and (listp backend) (member 'company-yasnippet backend))
  ;;       (append (if (listp backend) backend (list backend))
  ;;               (list :with 'company-yasnippet)))
  ;;       ;; (cons backend '(company-yasnippet)))
  ;;   backend)
  )

;; Auto-complete -------------------------------------------------------------

(defmacro spacemacs|enable-auto-complete (mode &optional hook)
  "Enable auto-complete for the given MODE.
By default the initialization function is hooked to `MODE-hook', it is
possible to explicitly define a hook with HOOK."
  (when (configuration-layer/layer-usedp 'auto-completion)
    (let ((mode-hook (if hook hook
                       (intern (format "%S-hook" mode))))
          (func (intern (format "spacemacs//init-auto-complete-%S" mode))))
      `(progn
         (defun ,func ()
           ,(format "Initialize auto-complete for %S" mode)
           (set (make-variable-buffer-local 'auto-completion-front-end)
                'auto-complete)
           (set (make-variable-buffer-local 'company-backends)
                ,(intern (format "company-backends-%S" mode))))
         (add-hook ',mode-hook ',func)
         (add-hook ',mode-hook 'auto-complete-mode)))))
