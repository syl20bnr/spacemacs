;;; packages.el --- Vagrant Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconst vagrant-packages
  '(vagrant
    vagrant-tramp))

(defun vagrant/init-vagrant ()
  (use-package vagrant
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "atv" "vagrant")
      (spacemacs/set-leader-keys
        "atvD" 'vagrant-destroy
        "atve" 'vagrant-edit
        "atvH" 'vagrant-halt
        "atvp" 'vagrant-provision
        "atvr" 'vagrant-resume
        "atvR" 'vagrant-reload
        "atvs" 'vagrant-status
        "atvS" 'vagrant-suspend
        "atvV" 'vagrant-up))))

(defun spacemacs/vagrant-ssh ()
  (interactive)
  (call-interactively (if (eq shell-default-shell 'shell)
                          'vagrant-tramp-shell
                        'vagrant-tramp-term)))
(defun vagrant/init-vagrant-tramp ()
  (use-package vagrant-tramp
    :defer t
    :init
    (progn
      (defvar spacemacs--vagrant-tramp-loaded nil)
      (defadvice vagrant-tramp-term (before spacemacs//load-vagrant activate)
        "Lazy load vagrant-tramp."
        (unless spacemacs--vagrant-tramp-loaded
          (vagrant-tramp-add-method)
          (setq spacemacs--vagrant-tramp-loaded t)))
      (spacemacs/set-leader-keys "atvt" 'spacemacs/vagrant-ssh))))
