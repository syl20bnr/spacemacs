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
                        (message "Enabled auto-completion (using %S)."
                                 auto-completion-front-end))
                      :off
                      (progn
                        (if (eq 'company auto-completion-front-end)
                            (company-mode -1)
                          (auto-complete-mode -1))
                        (message "Disabled auto-completion."))
                      :documentation "Activate auto-completion."
                      :evil-leader "ta")


