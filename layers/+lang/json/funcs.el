;;; funcs.el --- JSON Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/json-navigator-dwim (arg &optional start end)
  "Display the JSON hierarchy of the whole buffer or the active region.
If ARG is a universal prefix argument then display the hierarchy after point."
  (interactive "P\nr")
  (if arg
      (json-navigator-navigate-after-point)
    (if (equal start end)
        (save-excursion (json-navigator-navigate-region (point-min) (point-max)))
      (json-navigator-navigate-region start end))))


;; Formatters

(defun spacemacs/json-format ()
  "Call formatting tool specified in `json-fmt-tool'."
  (interactive)
  (cond
   ((eq json-fmt-tool 'jq)
    (call-interactively 'spacemacs/json-jq-format))
   ((eq json-fmt-tool 'web-beautify)
    (call-interactively 'spacemacs/json-web-beautify-format))
   ((eq json-fmt-tool 'prettier)
    (call-interactively 'spacemacs/json-prettier-format))
   ((eq json-fmt-tool 'fixjson)
    (call-interactively 'spacemacs/json-fixjson-format))

   (t (error (concat "%s isn't valid json-fmt-tool value."
                     " It should be 'jq, 'web-beutify, 'prettier, or 'fixjson.")
             (symbol-name json-fmt-tool)))))

(defun spacemacs/json-reformat-dwim (arg &optional start end)
  "Reformat the whole buffer of the active region.
If ARG is non-nil (universal prefix argument) then try to decode the strings.
If ARG is a numerical prefix argument then specify the indentation level."
  (interactive "P\nr")
  (let ((json-reformat:indent-width js-indent-level)
        (json-reformat:pretty-string? nil))
    (cond
     ((numberp arg) (setq json-reformat:indent-width arg))
     (arg (setq json-reformat:pretty-string? t)))
    (if (equal start end)
        (save-excursion (json-reformat-region (point-min) (point-max)))
      (json-reformat-region start end))))

(spacemacs|define-fmt-tool
 :id json-jq
 :name "jq"
 :program "jq"
 :args ("."))

(spacemacs|define-fmt-tool
 :id json-web-beautify
 :name "web-beautify"
 :program "js-beautify"
 :args ("-f" "-" "--type" "js"))

(spacemacs|define-fmt-tool
 :id json-prettier
 :name "prettier"
 :program "prettier"
 :args (lambda ()
         `("--parser" "json" "--stdin" "--stdin-filepath" ,buffer-file-name)))

(spacemacs|define-fmt-tool
 :id json-fixjson
 :name "fixjson"
 :program "fixjson"
 :args (lambda ()
         `(,@json-fixjson-args
           "--stdin-filename" ,(file-name-nondirectory buffer-file-name))))

(defun spacemacs/json-setup-fmt-on-save ()
  (add-to-list 'write-contents-functions #'spacemacs/json-format))
