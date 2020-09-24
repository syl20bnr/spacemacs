;;; packages.el --- evil-easymotion layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; Port from Doom project modules/editor/evil/config.el

;;; Code:

(defconst evil-easymotion-packages
  '(evil-easymotion))

(defun evil-easymotion/init-evil-easymotion ()
  (use-package evil-easymotion
    :config
    (evilem-default-keybindings "gs")
    (define-key evilem-map "a" (evilem-create #'evil-forward-arg))
    (define-key evilem-map "A" (evilem-create #'evil-backward-arg))
    (define-key evilem-map "o" (evilem-create #'evil-jump-out-args))
    (define-key evilem-map "s" #'evil-avy-goto-char-2)
    (define-key evilem-map "/" #'evil-avy-goto-char-timer)
    (define-key evilem-map (kbd "SPC") (lambda (&rest _)
                                   (interactive)
                                   (let ((current-prefix-arg t)) (evil-avy-goto-char-timer))))
    ;; Use evil-search backend, instead of isearch
    (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                        :bind ((evil-ex-search-highlight-all nil)))))

;;; packages.el ends here
