;;; packages.el --- Markdown Layer packages File for Spacemacs
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

(setq markdown-packages
  '(
    markdown-mode
    markdown-toc
    mmm-mode
    ))

(defun markdown/init-markdown-mode ()
  (use-package markdown-mode
    :mode ("\\.m[k]d" . markdown-mode)
    :defer t
    :init (add-hook 'markdown-mode-hook 'smartparens-mode)
    :config
    (progn
      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
      (defun spacemacs/insert-keybinding-markdown (key)
        "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
        (interactive "kType key sequence: ")
        (let* ((tag "~%s~"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -6))))

      (spacemacs/set-markdown-keybindings 'markdown-mode markdown-mode-map)
      )))

(defun markdown/init-markdown-toc ()
  (use-package markdown-toc
    :defer t))

(defun markdown/init-mmm-mode ()
  (use-package mmm-mode
    :commands mmm-parse-buffer
    :init
    (evil-leader/set-key-for-mode 'markdown-mode
      ;; Highlight code blocks
      "mcs"   'mmm-parse-buffer)
    :config
    (progn
      (mmm-add-classes '((markdown-python
                          :submode python-mode
                          :face mmm-declaration-submode-face
                          :front "^```python[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-html
                          :submode web-mode
                          :face mmm-declaration-submode-face
                          :front "^```html[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-java
                          :submode java-mode
                          :face mmm-declaration-submode-face
                          :front "^```java[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-ruby
                          :submode ruby-mode
                          :face mmm-declaration-submode-face
                          :front "^```ruby[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-c
                          :submode c-mode
                          :face mmm-declaration-submode-face
                          :front "^```c[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-c++
                          :submode c++-mode
                          :face mmm-declaration-submode-face
                          :front "^```c\+\+[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-elisp
                          :submode emacs-lisp-mode
                          :face mmm-declaration-submode-face
                          :front "^```elisp[\n\r]+"
                          :back "^```$")))
      (setq mmm-global-mode t)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-java)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ruby)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c++)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-elisp)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-html))))
