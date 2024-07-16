;;; packages.el --- Large Language Model Client for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <mail+spacemacs@codrut.pro>
;; Author: Alexander Matyasko <alexander.matyasko@gmail.com>
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


(defconst llm-client-packages
  '((ellama :toggle llm-client-enable-ellama)
    (gptel :toggle llm-client-enable-gptel)
    org
    window-purpose))

(defun llm-client/init-ellama ()
  "Initialize the `ellama` package and set up keybindings."
  (use-package ellama
    :defer t
    :ensure t
    :init
    (spacemacs/declare-prefix "$e" "Ellama")
    ;; Code
    (spacemacs/declare-prefix "$ec" "Code")
    (spacemacs/set-leader-keys
      "$ecc" 'ellama-code-complete    ; Complete code in current buffer
      "$eca" 'ellama-code-add         ; Add code according to description
      "$ece" 'ellama-code-edit        ; Edit code according to change
      "$eci" 'ellama-code-improve     ; Improve code
      "$ecr" 'ellama-code-review)     ; Review code
    ;; Summarize
    (spacemacs/declare-prefix "$es" "Summarize")
    (spacemacs/set-leader-keys
      "$ess" 'ellama-summarize           ; Summarize selected region or buffer
      "$esw" 'ellama-summarize-webpage   ; Summarize webpage
      "$esc" 'ellama-summarize-killring) ; Summarize text from kill ring
    ;; Edit
    (spacemacs/declare-prefix "$ee" "Edit")
    (spacemacs/set-leader-keys
      "$eec" 'ellama-complete            ; Complete text in current buffer
      "$eef" 'ellama-make-format         ; Render text as specified format
      "$eeg" 'ellama-improve-grammar     ; Improve grammar
      "$eel" 'ellama-make-list           ; Create markdown list
      "$ees" 'ellama-improve-conciseness ; Shorten text (make concise)
      "$eet" 'ellama-make-table          ; Create markdown table
      "$eew" 'ellama-improve-wording)    ; Improve wording
    ;; Ask
    (spacemacs/declare-prefix "$ea" "Ask")
    (spacemacs/set-leader-keys
      "$eaa" 'ellama-ask-about        ; Ask about selected region or buffer
      "$eac" 'ellama-chat             ; Chat with ellama
      "$eai" 'ellama-instant          ; Prompt ellama for instant reply
      "$eal" 'ellama-ask-line         ; Send current line to ellama
      "$eas" 'ellama-ask-selection)   ; Send selected region to ellama
    ;; Problem solving
    (spacemacs/declare-prefix "$ep" "Problem Solving")
    (spacemacs/set-leader-keys
      "$epr" 'ellama-solve-reasoning-problem        ; Solve reasoning PROBLEM with abstraction of thought
      "$epd" 'ellama-solve-domain-specific-problem) ; Solve domain-specific PROBLEM
    ;; Translate
    (spacemacs/declare-prefix "$et" "Translate")
    (spacemacs/set-leader-keys
      "$ett" 'ellama-translate                 ; Translate selected region or word at point
      "$etb" 'ellama-translate-buffer          ; Translate current buffer
      "$ete" 'ellama-chat-translation-enable   ; Enable chat translation
      "$etd" 'ellama-chat-translation-disable) ; Disable chat translation
    ;; Define
    (spacemacs/declare-prefix "$ed" "Define")
    (spacemacs/set-leader-keys
      "$edw" 'ellama-define-word)     ; Define current word
    ;; Context
    (spacemacs/declare-prefix "$ex" "Context")
    (spacemacs/set-leader-keys
      "$exb" 'ellama-context-add-buffer     ; Add buffer to context
      "$exf" 'ellama-context-add-file       ; Add file to context
      "$exs" 'ellama-context-add-selection  ; Add selection to context
      "$exi" 'ellama-context-add-info-node) ; Add info node to context
    ;; Session Management
    (spacemacs/declare-prefix "$eS" "Session")
    (spacemacs/set-leader-keys
      "$eSl" 'ellama-load-session     ; Load ellama session
      "$eSr" 'ellama-session-rename   ; Rename ellama session
      "$eSd" 'ellama-session-remove   ; Remove ellama session
      "$eSa" 'ellama-session-switch)  ; Switch ellama session
    ;; Provider
    (spacemacs/declare-prefix "$eP" "Provider")
    (spacemacs/set-leader-keys
      "$ePs" 'ellama-provider-select))) ; Select ellama provider

(defun llm-client/init-gptel ()
  "Initialize the `gptel` package and set up keybindings."
  (use-package gptel
    :defer t
    :ensure t
    :init
    (spacemacs/declare-prefix "$g" "Gptel")
    (spacemacs/set-leader-keys
      "$gg" 'gptel                          ; Start a new GPTel session
      "$gs" 'spacemacs//gptel-send-wrapper  ; Send a message to GPTel
      "$gq" 'spacemacs//gptel-abort-wrapper ; Abort any active GPTel process
      "$gm" 'gptel-menu                     ; Open the GPTel menu
      "$gc" 'gptel-add                      ; Add context
      "$gf" 'gptel-add-file                 ; Add a file
      "$go" 'gptel-org-set-topic            ; Set topic in Org-mode
      "$gp" 'gptel-org-set-properties)))    ; Set properties in Org-mode

(defun llm-client/post-init-org ()
  "Set up Org-mode keybindings for GPTel."
  (spacemacs/declare-prefix-for-mode 'org-mode "m$g" "Gptel")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "$go" 'gptel-org-set-topic
    "$gp" 'gptel-org-set-properties))

(defun llm-client/post-init-window-purpose ()
  ;; TODO: Temporary fix to avoid the error when using window-purpose
  ;; see https://github.com/karthink/gptel/issues/237 for details
  ;; (purpose-set-extension-configuration
  ;;  :llm-client-layer
  ;;  (purpose-conf :mode-purposes '((gptel-mode . chat))))
  (defun llm-client/disable-purpose-mode-around-for-gptel (orig-func &rest args)
    "Advice function to disable purpose-mode before calling ORIG-FUNC with ARGS."
    (let ((purpose-mode-was-enabled (bound-and-true-p purpose-mode)))
      (when purpose-mode-was-enabled
        (purpose-mode -1))
      (apply orig-func args)
      (when purpose-mode-was-enabled
        (purpose-mode 1))))
  (advice-add 'gptel :around #'llm-client/disable-purpose-mode-around-for-gptel))
