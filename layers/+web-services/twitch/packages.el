;;; packages.el --- twitch layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Benedikt Broich <b.broich@posteo.de>
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

;;; Code:

(defconst twitch-packages
  '(
    ;; (erc-twitch :toggle (configuration-layer/package-usedp 'erc))
    (twitch-api :location (recipe :fetcher github
                                  :repo "benediktbroich/twitch-api"))
    (helm-twitch :location (recipe :fetcher github
                                   :repo "benediktbroich/helm-twitch")
                 ;; :toggle (configuration-layer/package-usedp 'helm)
                 )))

(defun twitch/init-twitch-api ()
  (use-package twitch-api
    :init (progn
            (when (configuration-layer/package-usedp 'erc)
              (spacemacs/set-leader-keys
                "acit" 'twitch-api-erc-tls)))))

(defun twitch/init-helm-twitch ()
  (use-package helm-twitch
    :defer t
    :init (progn
            (spacemacs/declare-prefix "aws" "stream")
            (spacemacs/set-leader-keys
              "awst" 'helm-twitch)
            (when (configuration-layer/package-usedp 'streamlink)
              (setq helm-twitch-enable-livestreamer-actions t))
            (when (configuration-layer/package-usedp 'erc)
              (setq helm-twitch-enable-chat-actions t)))))
