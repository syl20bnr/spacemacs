;;; packages.el --- OpenAI Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Hendrik Rommeswinkel
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


(defconst openai-packages
  '((openai :location (recipe :fetcher github :repo "emacs-openai/openai"))
    (chatgpt :location (recipe :fetcher github :repo "emacs-openai/chatgpt"))
    (codegpt :location (recipe :fetcher github :repo "emacs-openai/codegpt"))
    (dall-e :location (recipe :fetcher github :repo "emacs-openai/dall-e"))))

(defun openai/init-openai ()
  (use-package openai
    :init
    (progn
      (spacemacs/declare-prefix "awo" "OpenAI")
      (spacemacs/declare-prefix "awoi" "OpenAI images")
      (spacemacs/set-leader-keys
        "awob" 'openai-completion-buffer-insert
        "awom" 'openai-list-models
        "awoM" 'openai-retrieve-model
        "awos" 'openai-completion-select-insert
        "awoC" 'openai-chat-say
        "awoe" 'openai-edit-prompt
        "awoE" 'openai-embedding-create
        "awoie" 'openai-image-edit-prompt
        "awoii" 'openai-image-prompt
        "awoiv" 'openai-image-variation-prompt))))

(defun openai/init-chatgpt ()
  (use-package chatgpt
    :init
    (progn
      (spacemacs/set-leader-keys
        ;; ChatGPT session
        "awog" 'chatgpt))
    :config
    (progn
      (evilified-state-evilify-map chatgpt-mode-map
        :mode chatgpt-mode
        :bindings
        (kbd "<return>") 'chatgpt-type-response))))


(defun openai/init-codegpt ()
  (use-package codegpt
    :init
    (progn
      (spacemacs/declare-prefix "awoc" "codegpt")
      (spacemacs/set-leader-keys
        ;; Coding tools
        "awocc" 'codegpt
        "awocC" 'codegpt-custom
        "awocd" 'codegpt-doc
        "awocf" 'codegpt-fix
        "awoce" 'codegpt-explain
        "awoci" 'codegpt-improve))))

(defun openai/init-dall-e ()
  (use-package dall-e
    :init
    (progn
      (spacemacs/set-leader-keys
        "awoid" 'dall-e))
    :config
    (progn
      (evilified-state-evilify-map dall-e-mode-map
        :mode dall-e-mode
        :bindings
        (kbd "<return>") 'dall-e-type-response))))
