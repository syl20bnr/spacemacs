;;; packages.el --- OpenAI Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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
    (spacemacs/declare-prefix "$" "AI")
    (spacemacs/declare-prefix "$o" "OpenAI")
    (spacemacs/declare-prefix "$oi" "OpenAI images")
    (spacemacs/set-leader-keys
      "$ob" 'openai-completion-buffer-insert
      "$om" 'openai-list-models
      "$oM" 'openai-retrieve-model
      "$os" 'openai-completion-select-insert
      "$oC" 'openai-chat-say
      "$oe" 'openai-edit-prompt
      "$oE" 'openai-embedding-create
      "$oie" 'openai-image-edit-prompt
      "$oii" 'openai-image-prompt
      "$oiv" 'openai-image-variation-prompt)))

(defun openai/init-chatgpt ()
  (use-package chatgpt
    :init
    (spacemacs/set-leader-keys
      ;; ChatGPT session
      "$og" 'chatgpt)
    :config
    (evilified-state-evilify-map chatgpt-mode-map
      :mode chatgpt-mode
      :bindings
      (kbd "<return>") 'chatgpt-type-response)))

(defun openai/init-codegpt ()
  (use-package codegpt
    :init
    (spacemacs/declare-prefix "$oc" "codegpt")
    (spacemacs/set-leader-keys
      ;; Coding tools
      "$occ" 'codegpt
      "$ocC" 'codegpt-custom
      "$ocd" 'codegpt-doc
      "$ocf" 'codegpt-fix
      "$oce" 'codegpt-explain
      "$oci" 'codegpt-improve)))

(defun openai/init-dall-e ()
  (use-package dall-e
    :init
    (spacemacs/set-leader-keys
      "$oid" 'dall-e)
    :config
    (evilified-state-evilify-map dall-e-mode-map
      :mode dall-e-mode
      :bindings
      (kbd "<return>") 'dall-e-type-response)))
