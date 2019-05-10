;; -*- lexical-binding: t -*-

;;; npm-and-yarn.el

;; Author:   Seong Yong-ju <sei40kr@gmail.com>
;; Created:  May 10 2019
;; Version:  0.1.0
;; Keywords: node, npm, yarn

;;; Commentary:

;;; Code:

;;;###autoload
(defun npm-and-yarn/npm-install ()
  (interactive)
  (when-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
    (npm-and-yarn//npm-exec (concat "install --save " (shell-quote-argument dep)))))

;;;###autoload
(defun npm-and-yarn/npm-install-dev ()
  (interactive)
  (when-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
    (npm-and-yarn//npm-exec (concat "install --save-dev " (shell-quote-argument dep)))))

;;;###autoload
(defun npm-and-yarn/yarn-add ()
  (interactive)
  (when-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
    (npm-and-yarn//yarn-exec (concat "add " (shell-quote-argument dep)))))

;;;###autoload
(defun npm-and-yarn/yarn-add-dev ()
  (interactive)
  (when-let* ((dep (read-string "Enter package name: " (thing-at-point 'symbol))))
    (npm-and-yarn//yarn-exec (concat "add --dev " (shell-quote-argument dep)))))

(defun npm-and-yarn//npm-exec (npm-args)
  (compilation-start (concat "npm " npm-args) #'compilation-mode
                     #'npm-and-yarn//npm-exec-buffer-name))

(defun npm-and-yarn//npm-exec-buffer-name (_)
  "*npm*")

(defun npm-and-yarn//yarn-exec (yarn-args)
  (compilation-start (concat "yarn " yarn-args) #'compilation-mode
                     #'npm-and-yarn//yarn-exec-buffer-name))

(defun npm-and-yarn//yarn-exec-buffer-name (_)
  "*yarn*")

(provide 'npm-and-yarn)

;;; npm-and-yarn ends here
