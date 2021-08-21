;;; packages.el --- eaf configuration file
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Leslie Huang <lesliebinbin19900129@gmail.com>
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

(defcustom eaf-apps
  '(eaf-jupyter
    eaf-browser
    eaf-airshare
    eaf-file-browser
    eaf-file-manager
    eaf-file-sender
    eaf-music-player
    eaf-system-monitor
    eaf-mindmap
    eaf-org-previewer
    eaf-terminal
    eaf-netease-cloud-music
    eaf-video-player
    eaf-image-viewer
    eaf-demo
    eaf-vue-demo
    eaf-pdf-viewer
    eaf-markdown-previewer
    eaf-camera
    )
  "Eaf Applications"
  :group 'eaf
  :type '(symbol)
)
