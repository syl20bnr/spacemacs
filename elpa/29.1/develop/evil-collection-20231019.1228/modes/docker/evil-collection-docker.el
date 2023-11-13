;;; evil-collection-docker.el --- Evil bindings for docker.el -*- lexical-binding: t -*-

;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, docker, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for docker.el

;;; Code:
(require 'docker nil t)
(require 'evil-collection)

(defconst evil-collection-docker-maps '(docker-container-mode-map
					docker-context-mode-map
                                        docker-image-mode-map
                                        docker-machine-mode-map
                                        docker-network-mode-map
                                        docker-volume-mode-map))

(defun evil-collection-docker-setup ()
  "Set up `evil' bindings for `docker'."
  (evil-collection-define-key 'normal 'docker-container-mode-map
    ";"  'docker-container-ls
    "?"  'docker-container-help
    "C"  'docker-container-cp
    "D"  'docker-container-rm
    "I"  'docker-container-inspect
    "K"  'docker-container-kill
    "L"  'docker-container-logs
    "O"  'docker-container-stop
    "P"  'docker-container-pause
    "R"  'docker-container-restart
    "S"  'docker-container-start
    "a"  'docker-container-attach
    "b"  'docker-container-shells
    "d"  'docker-container-diff
    "f"  'docker-container-open
    "r"  'docker-container-rename-selection)

  (evil-collection-define-key 'normal 'docker-context-mode-map
    "?"  'docker-context-help
    "D"  'docker-context-rm
    "I"  'docker-context-inspect
    "X"  'docker-context-use)

  (evil-collection-define-key 'normal 'docker-image-mode-map
    ";"  'docker-image-ls
    "?"  'docker-image-help
    "D"  'docker-image-rm
    "F"  'docker-image-pull
    "I"  'docker-image-inspect
    "P"  'docker-image-push
    "R"  'docker-image-run
    "T"  'docker-image-tag-selection
    "d"  'docker-image-mark-dangling)

  (evil-collection-define-key 'normal 'docker-network-mode-map
    ";"  'docker-network-ls
    "?"  'docker-network-help
    "D"  'docker-network-rm
    "I"  'docker-network-inspect
    "d"  'docker-network-mark-dangling)

  (evil-collection-define-key 'normal 'docker-volume-mode-map
    ";"  'docker-volume-ls
    "?"  'docker-volume-help
    "D"  'docker-volume-rm
    "I"  'docker-volume-inspect
    "d"  'docker-volume-mark-dangling
    "f"  'docker-volume-dired-selection))

(provide 'evil-collection-docker)

;;; evil-collection-docker.el ends here
