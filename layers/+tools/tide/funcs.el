;;; funcs.el --- Tide  Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//tide-setup-bindings ()
  "Define keys bindings for `tide-mode'"
  (space-macs/set-leader-keys-for-minor-mode 'tide-mode
    "E" "errors"
    "Ee" #'tide-fix
    "Ed" #'tide-add-tslint-disable-next-line
    "Ep" #'tide-project-errors
    "g" "goto"
    "ge" #'tide-project-errors
    "gb" #'tide-jump-back
    "gg" #'tide-jump-to-definition
    "gt" #'space-macs/typescript-jump-to-type-def
    "gr" #'tide-references
    "h" "help"
    "hh" #'tide-documentation-at-point
    "r" "refactor"
    "ri" #'tide-organize-imports
    "rr" #'tide-rename-symbol
    "rf" #'tide-rename-file
    "S" "server"
    "Sr" #'tide-restart-server
    "Sj" #'space-macs//tide-create-jsconfig-file))

(defun space-macs//tide-setup ()
  "Setup tide backend.
Must be called by a layer using tide."
  (evilified-state-evilify tide-references-mode tide-references-mode-map
    (kbd "C-k") 'tide-find-previous-reference
    (kbd "C-j") 'tide-find-next-reference
    (kbd "C-l") 'tide-goto-reference)
  (tide-hl-identifier-mode +1)
  (tide-setup))

(defun space-macs//tide--list-to-string (list)
  "Convert LIST to string."
  (cl-reduce (lambda (x y) (concat x " " (symbol-name y)))
             (cdr list)
             :initial-value (format "%s" (car list) )))

(defun space-macs//tide-setup-company (&rest modes)
  "Setup tide company for MODES.
Must be called by a layer using tide."
  (eval `(space-macs|add-company-backends
           :backends company-tide
           :modes ,@modes
           :append-hooks nil
           :call-hooks t))
  (company-mode))

(defun space-macs//tide-setup-eldoc ()
  "Setup eldoc for tide."
  (eldoc-mode))

(defun space-macs//tide-setup-jump-handle ()
  "Set jump handlers."
  (add-to-list 'space-macs-jump-handlers '(tide-jump-to-definition :async t)))

(defun space-macs//tide-create-jsconfig-file ()
  "Create a jsconfig file at project root."
  (interactive)
  (let ((jsconfig (cdr (project-current))))
    (if jsconfig
        (let ((jsconfig-file (concat jsconfig "jsconfig.json")))
          (if (file-exists-p jsconfig-file)
              (message "File exists")
            (with-temp-file jsconfig-file
              (insert tide-jsconfig-content))))
      (message "Project not found"))))


