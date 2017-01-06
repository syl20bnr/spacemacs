;;; funcs.el --- TypeScript  Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun typescript/tsfmt-format-buffer ()
  "Format buffer with tsfmt."
  (interactive)
  (if (executable-find "tsfmt")
      (let*  ((tmpfile (make-temp-file "~fmt-tmp" nil ".ts"))
              (coding-system-for-read 'utf-8)
              (coding-system-for-write 'utf-8)
              (outputbuf (get-buffer-create "*~fmt-tmp.ts*")))
        (unwind-protect
            (progn
              (with-current-buffer outputbuf (erase-buffer))
              (write-region nil nil tmpfile)
              (if (zerop (apply 'call-process "tsfmt" nil outputbuf nil
                                (list (format
                                       "--baseDir='%s' --"
                                       default-directory)
                                      tmpfile)))
                  (let ((p (point)))
                    (save-excursion
                      (with-current-buffer (current-buffer)
                        (erase-buffer)
                        (insert-buffer-substring outputbuf)))
                    (goto-char p)
                    (message "formatted.")
                    (kill-buffer outputbuf))
                (progn
                  (message "Formatting failed!")
                  (display-buffer outputbuf)))
              (progn
                (delete-file tmpfile)))))
    (error "tsfmt not found. Run \"npm install -g typescript-formatter\"")))

(defun typescript/format ()
  "Call formatting tool specified in `typescript-fmt-tool'."
  (interactive)
  (cond
   ((eq typescript-fmt-tool 'typescript-formatter)
    (call-interactively 'typescript/tsfmt-format-buffer))
   ((eq typescript-fmt-tool 'tide)
    (call-interactively 'tide-format))
   (t (error (concat "%s isn't valid typescript-fmt-tool value."
                     " It should be 'tide or 'typescript-formatter."
                     (symbol-name typescript-fmt-tool))))))

(defun typescript/fmt-before-save-hook ()
  (add-hook 'before-save-hook 'typescript/format t t))

(defun typescript/open-region-in-playground (start end)
  "Open selected region in http://www.typescriptlang.org/Playground
                 If nothing is selected - open the whole current buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (browse-url (concat "http://www.typescriptlang.org/Playground#src="
                      (url-hexify-string (buffer-substring-no-properties start end)))))
