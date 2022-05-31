;;; packages.el --- emberjs layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Robert O'Connor <robby.oconnor@gmail.com>
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


;;; Commentary:

;;; Code:

(defconst emberjs-packages
  '(
    ember-mode
    ember-yasnippets
    ))

(defun emberjs/init-ember-mode ()
  (use-package ember-mode
    :defer t
    :config
    (spacemacs/declare-prefix-for-mode 'ember-mode "f"  "framework")
    (spacemacs/declare-prefix-for-mode 'ember-mode "fd" "destroy")
    (spacemacs/declare-prefix-for-mode 'ember-mode "ff" "find")
    (spacemacs/declare-prefix-for-mode 'ember-mode "fg" "generate")
    (spacemacs/declare-prefix-for-mode 'ember-mode "fr" "run")
    (spacemacs/set-leader-keys-for-minor-mode 'ember-mode
      ;; Destroy
      "fdc" 'ember-destroy-controller
      "fdm" 'ember-destroy-model
      "fdo" 'ember-destroy-router
      "fdp" 'ember-destroy-component
      "fdr" 'ember-destroy-route
      "fdj" 'ember-destroy-javascript
      "fdt" 'ember-destroy-template
      "fdx" 'ember-destroy-mixin
      "fdi" 'ember-destroy-initializer
      "fdu" 'ember-destroy-util
      "fds" 'ember-destroy-service
      "fdd" 'ember-destroy
      ;; Find
      "ffc" 'ember-open-controller
      "ffm" 'ember-open-model
      "ffo" 'ember-open-router
      "ffp" 'ember-open-component
      "ffr" 'ember-open-route
      "ffj" 'ember-open-javascript
      "fft" 'ember-open-template
      "ffx" 'ember-open-mixin
      "ffi" 'ember-open-initializer
      "ffu" 'ember-open-util
      "ffs" 'ember-open-service
      ;; Generate
      "fgc" 'ember-generate-controller
      "fgm" 'ember-generate-model
      "fgo" 'ember-generate-router
      "fgp" 'ember-generate-component
      "fgr" 'ember-generate-route
      "fgj" 'ember-generate-javascript
      "fgt" 'ember-generate-template
      "fgx" 'ember-generate-mixin
      "fgi" 'ember-generate-initializer
      "fgu" 'ember-generate-util
      "fgs" 'ember-generate-service
      "fgg" 'ember-generate
      ;; Run
      "frb" 'ember-build
      "frs" 'ember-serve-or-display
      "frt" 'ember-test)))

(defun emberjs/init-ember-yasnippets ()
  (use-package ember-yasnippets
    :defer t))

;;; packages.el ends here
