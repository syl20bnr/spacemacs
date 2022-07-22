;;; packages.el --- dirvish layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Thanh <thanhvg@gmail.com>
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

(defconst dirvish-packages
  '((dirvish :location (recipe
                        :fetcher github
                        :repo  "alexluigit/dirvish"
                        :files (:defaults "extensions/*.el")
                        ;; pin this because divish is still evolving fast
                        :commit "1b90bf473b840f9c85598934c7654d8c706ac51f"))))

(defun dirvish/init-dirvish ()
  (use-package dirvish
    :defer t
    :after (dired)
    :init
    ;; delay start up
    (run-with-idle-timer spacemacs-dirvish-delay
                         nil
                         #'dirvish-override-dired-mode)
    :config
    (spacemacs|add-toggle dirvish
      :mode dirvish-override-dired-mode
      :documentation "Enable dirvish."
      :evil-leader "t C-d")
    (setq dirvish-cache-dir (concat spacemacs-cache-directory "dirvish/")
          dirvish-hide-details t)
    (unless dirvish-attributes
      (setq dirvish-attributes '(all-the-icons file-size collapse subtree-state)))
    ;; (dirvish-peek-mode)
    ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
    (setq dired-dwim-target t)
    (setq delete-by-moving-to-trash t)
    ;; Enable mouse drag-and-drop files to other applications
    (setq dired-mouse-drag-files t)                   ; added in Emacs 29
    (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
    ;; Make sure to use the long name of flags when exists
    ;; eg. use "--almost-all" instead of "-A"
    ;; Otherwise some commands won't work properly
    (setq dired-listing-switches
          "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
    (spacemacs/set-leader-keys-for-major-mode 'dirvish-mode
      "SPC" #'dirvish-dispatch
      "." #'dirvish-dispatch
      "n" #'dirvish-narrow
      "^" #'dirvish-history-last
      "a" #'dirvish-quick-access
      "f" #'dirvish-file-info-menu
      "h" #'dirvish-history-jump
      "i" #'wdired-change-to-wdired-mode
      "s" #'dirvish-quicksort
      "t." #'dired-omit-mode
      "tf" #'dirvish-toggle-fullscreen
      "ts" #'dirvish-setup-menu
      "u" #'dired-up-directory
      "y" #'dirvish-yank-menu)
    (evil-define-key 'normal dirvish-mode-map
      "h" 'spacemacs/dirvish-collapse-or-up
      "l" 'spacemacs/dirvish-expand-or-open)
    :bind
    ;; Dirvish has all the keybindings (except `dired-summary') in `dired-mode-map' already
    (:map dirvish-mode-map
      ("TAB" . dirvish-subtree-toggle)
      ("M-n" . dirvish-history-go-forward)
      ("M-p" . dirvish-history-go-backward)
      ("M-l" . dirvish-ls-switches-menu)
      ("M-m" . dirvish-mark-menu)
      ("M-f" . dirvish-toggle-fullscreen)
      ("M-e" . dirvish-emerge-menu)
      ("M-j" . dirvish-fd-jump))))
