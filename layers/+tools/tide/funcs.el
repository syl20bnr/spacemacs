;;; funcs.el --- Tide  Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//tide-setup-prefix ()
  "This one should run only once, otherwise `which-key' will become very slow #12455"
  (dolist (mode tide-managed-modes)
    (spacemacs/declare-prefix-for-mode mode "mE" "errors")
    (spacemacs/declare-prefix-for-mode mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode mode "mh" "help")
    (spacemacs/declare-prefix-for-mode mode "mn" "name")
    (spacemacs/declare-prefix-for-mode mode "mr" "refactor")
    (spacemacs/declare-prefix-for-mode mode "mS" "server")))

(defun spacemacs//tide-setup-bindings ()
  "Define keys bindings for `tide-mode'"
  (spacemacs/set-leader-keys-for-minor-mode 'tide-mode
    "Ee" #'tide-fix
    "Ed" #'tide-add-tslint-disable-next-line
    "gb" #'tide-jump-back
    "gg" #'tide-jump-to-definition
    "gt" #'spacemacs/typescript-jump-to-type-def
    "gr" #'tide-references
    "hh" #'tide-documentation-at-point
    "ri" #'tide-organize-imports
    "rr" #'tide-rename-symbol
    "rf" #'tide-rename-file
    "Sr" #'tide-restart-server
    "Sj" #'spacemacs//tide-create-jsconfig-file))

(defun spacemacs//tide-setup ()
  "Setup tide backend.
Must be called by a layer using tide."
  (evilified-state-evilify tide-references-mode tide-references-mode-map
    (kbd "C-k") 'tide-find-previous-reference
    (kbd "C-j") 'tide-find-next-reference
    (kbd "C-l") 'tide-goto-reference)
  (tide-hl-identifier-mode +1)
  (tide-setup))

(defun spacemacs//tide--list-to-string (list)
  "Convert LIST to string."
  (cl-reduce (lambda (x y) (concat x " " (symbol-name y)))
             (cdr list)
             :initial-value (format "%s" (car list) )))

(defun spacemacs//tide-setup-company (&rest modes)
  "Setup tide company for MODES.
Must be called by a layer using tide."
  (eval `(spacemacs|add-company-backends
           :backends company-tide
           :modes ,@modes
           :append-hooks nil
           :call-hooks t))
  (company-mode))

(defun spacemacs//tide-setup-eldoc ()
  "Setup eldoc for tide."
  (eldoc-mode))

(defun spacemacs//tide-setup-jump-handle ()
  "Loop through `tide-managed-modes' and set jump handlers for these modes."
  (dolist (mode tide-managed-modes)
    (add-to-list
     (intern (format "spacemacs-jump-handlers-%S" mode))
     '(tide-jump-to-definition :async t))))

(defun spacemacs//tide-create-jsconfig-file ()
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
