;;; packages.el --- eaf layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Daniel Nicolai <dalanicolai@gmail.com>
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

(defconst eaf-packages
  '((eaf :location (recipe
                    :fetcher github
                    :repo  "emacs-eaf/emacs-application-framework"
                    :files ("*" "core/*.el" "extension/*.el")))))

(defun eaf/init-eaf ()
  (use-package eaf
    :init
    (progn
      (spacemacs/declare-prefix "aa" "application-framework")
      (spacemacs/set-leader-keys "aac" 'eaf-open-camera)
      (spacemacs/set-leader-keys "aaf" 'eaf-open)
      (spacemacs/set-leader-keys "aaj" 'eaf-open-jupyter)
      (spacemacs/set-leader-keys "aao" 'eaf-open-office)
      (spacemacs/set-leader-keys "aat" 'eaf-open-terminal)
      (spacemacs/set-leader-keys "aas" 'eaf-open-system-monitor)
      (spacemacs/set-leader-keys "aaM" 'eaf-open-music-player)

      (spacemacs/declare-prefix "aab" "browser")
      (spacemacs/set-leader-keys "aabo" 'eaf-open-browser)
      (spacemacs/set-leader-keys "aabs" 'eaf-search-it)
      (spacemacs/set-leader-keys "aabb" 'eaf-open-bookmark)
      (spacemacs/set-leader-keys "aabh" 'eaf-open-browser-with-history)

      (spacemacs/declare-prefix "aabq" "quick-launch-website")
      (spacemacs/set-leader-keys "aabqd" 'duckduckgo)
      (spacemacs/set-leader-keys "aabqw" 'wikipedia)
      (spacemacs/set-leader-keys "aabqy" 'youtube)

      (spacemacs/declare-prefix "aam" "mindmap")
      (spacemacs/set-leader-keys "aamc" 'eaf-create-mindmap)
      (spacemacs/set-leader-keys "aamm" 'eaf-open-mindmap)

      (setq eaf-browser-keybinding
            '(("C--" . "zoom_out")
              ("C-=" . "zoom_in")
              ("C-0" . "zoom_reset")
              ("C-s" . "search_text_forward")
              ("C-r" . "search_text_backward")
              ("C-n" . "scroll_up")
              ("C-p" . "scroll_down")
              ("C-f" . "scroll_right")
              ("C-b" . "scroll_left")
              ("C-v" . "scroll_up_page")
              ("C-y" . "yank_text")
              ("C-w" . "kill_text")
              ("M-e" . "atomic_edit")
              ("M-c" . "caret_toggle_browsing")
              ("M-D" . "select_text")
              ("M-s" . "open_link")
              ("M-S" . "open_link_new_buffer")
              ("M-B" . "open_link_background_buffer")
              ("C-/" . "undo_action")
              ("M-_" . "redo_action")
              ("M-w" . "copy_text")
              ("M-f" . "history_forward")
              ("M-b" . "history_backward")
              ("M-q" . "clear_cookies")
              ("C-t" . "toggle_password_autofill")
              ("C-d" . "save_page_password")
              ("M-a" . "toggle_adblocker")
              ("C-M-q" . "clear_history")
              ("C-M-i" . "import_chrome_history")
              ("M-v" . "scroll_down_page")
              ("M-<" . "scroll_to_begin")
              ("M->" . "scroll_to_bottom")
              ("M-p" . "duplicate_page")
              ("M-t" . "new_blank_page")
              ("M-d" . "toggle_dark_mode")
              ("<" . "insert_or_select_left_tab")
              (">" . "insert_or_select_right_tab")
              ("j" . "insert_or_scroll_up")
              ("k" . "insert_or_scroll_down")
              ("h" . "insert_or_scroll_left")
              ("l" . "insert_or_scroll_right")
              ("f" . "insert_or_open_link")
              ("F" . "insert_or_open_link_new_buffer")
              ("B" . "insert_or_open_link_background_buffer")
              ("c" . "insert_or_caret_at_line")
              ("J" . "insert_or_scroll_up_page")
              ("K" . "insert_or_scroll_down_page")
              ("H" . "insert_or_history_backward")
              ("L" . "insert_or_history_forward")
              ("t" . "insert_or_new_blank_page")
              ("T" . "insert_or_recover_prev_close_page")
              ("i" . "insert_or_focus_input")
              ("I" . "insert_or_open_downloads_setting")
              ("r" . "insert_or_refresh_page")
              ("g" . "insert_or_scroll_to_begin")
              ("x" . "insert_or_close_buffer")
              ("G" . "insert_or_scroll_to_bottom")
              ("-" . "insert_or_zoom_out")
              ("=" . "insert_or_zoom_in")
              ("0" . "insert_or_zoom_reset")
              ;; ("d" . "insert_or_dark_mode")
              ("m" . "insert_or_save_as_bookmark")
              ("o" . "insert_or_open_browser")
              ;; ("y" . "insert_or_download_youtube_video")
              ("y" . "insert_or_copy_text")
              ("Y" . "insert_or_download_youtube_audio")
              ("p" . "insert_or_toggle_device")
              ("P" . "insert_or_duplicate_page")
              ("1" . "insert_or_save_as_pdf")
              ("2" . "insert_or_save_as_single_file")
              ("v" . "insert_or_view_source")
              ("e" . "insert_or_edit_url")
              ("C-M-c" . "copy_code")
              ("C-M-l" . "copy_link")
              ("C-a" . "select_all_or_input_text")
              ("M-u" . "clear_focus")
              ("C-j" . "open_downloads_setting")
              ("M-o" . "eval_js")
              ("M-O" . "eval_js_file")
              ("<escape>" . "eaf-browser-send-esc-or-exit-fullscreen")
              ("M-," . "eaf-send-down-key")
              ("M-." . "eaf-send-up-key")
              ("M-m" . "eaf-send-return-key")
              ("<f5>" . "refresh_page")
              ("<f12>" . "open_devtools")
              ("<C-return>" . "eaf-send-ctrl-return-sequence")))


      (setq eaf-pdf-viewer-keybinding
            '(("j" . "scroll_up")
              ("<down>" . "scroll_up")
              ("C-n" . "scroll_up")
              ("k" . "scroll_down")
              ("<up>" . "scroll_down")
              ("C-p" . "scroll_down")
              ("h" . "scroll_left")
              ("<left>" . "scroll_left")
              ("C-b" . "scroll_left")
              ("l" . "scroll_right")
              ("<right>" . "scroll_right")
              ("C-f" . "scroll_right")
              ("J" . "scroll_up_page")
              ("K" . "scroll_down_page")
              ("C-v" . "scroll_up_page")
              ("M-v" . "scroll_down_page")
              ("t" . "toggle_read_mode")
              ("0" . "zoom_reset")
              ("=" . "zoom_in")
              ("-" . "zoom_out")
              ("g" . "scroll_to_begin")
              ("G" . "scroll_to_end")
              ("p" . "jump_to_page")
              ("P" . "jump_to_percent")
              ("[" . "save_current_pos")
              ("]" . "jump_to_saved_pos")
              ("i" . "toggle_inverted_mode")
              ("m" . "toggle_mark_link")
              ("f" . "jump_to_link")
              ("d" . "toggle_inverted_mode")
              ("M-w" . "copy_select")
              ("C-s" . "search_text_forward")
              ("C-r" . "search_text_backward")
              ("x" . "close_buffer")
              ("C-<right>" . "rotate_clockwise")
              ("C-<left>" . "rotate_counterclockwise")
              ("M-h" . "add_annot_highlight")
              ("M-u" . "add_annot_underline")
              ("M-s" . "add_annot_squiggly")
              ("M-d" . "add_annot_strikeout_or_delete_annot")
              ("M-e" . "add_annot_text_or_edit_annot")
              ("M-p" . "toggle_presentation_mode")
              ("o" . "eaf-pdf-outline"))))
    ;; switch tab only works with awesome-tab package
    ;; ("<C-tab>" . "select_left_tab")
    ;; ("<C-iso-lefttab>" . "select_right_tab")
    :config
    (progn
      (dolist (app eaf-apps)
        (require app nil 'noerror))
      (setq browse-url-browser-function 'eaf-open-browser)
      (setq eaf-browser-enable-adblocker "true")

      (define-key eaf-mode-map* (kbd "C-SPC C-SPC") 'execute-extended-command)
      ;;;; TODO need to consider the current pdf view mode which does not need to be pdf view mode
      (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode "E" 'spacemacs/open-with-eaf)
      (add-to-list 'evil-evilified-state-modes 'eaf-pdf-outline-mode)))

  ;; remove compiled file necessary to suppress clear_focus variable non-existent
  ;; error (eaf is not yet meant to be installed with quelpa, see
  ;; `https://github.com/manateelazycat/emacs-application-framework#install')

  ;; Alternative way to delete compiled file
  ;; (when (locate-library "eaf-evil.elc")
  ;;   (delete-file))

  (use-package eaf-evil
    :after eaf
    :config
    (progn
      ;; the following line are taken from the evil-integration example:
      ;; https://github.com/manateelazycat/emacs-application-framework/wiki/Evil
      (setq eaf-evil-leader-keymap spacemacs-cmds)

      (define-key key-translation-map (kbd "SPC")
        (lambda (prompt)
          (if (derived-mode-p 'eaf-mode)
              (pcase eaf--buffer-app-name
                ((or
                  (and "browser"
                       (guard (not (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True"))))
                  "image-viewer"
                  "pdf-viewer")
                 (kbd eaf-evil-leader-key))
                (_  (kbd "SPC")))
            (kbd "SPC"))))

      ;; The following lines create the major-mode leader key emulation map
      ;; in a similar way as how it was done in the evil-integration example
      (setq eaf-evil-leader-for-major-keymap (make-sparse-keymap))
      (define-key eaf-evil-leader-for-major-keymap (kbd "h") 'eaf-open-browser-with-history)
      (define-key eaf-evil-leader-for-major-keymap (kbd "d") 'eaf-proxy-toggle_dark_mode)
      (define-key eaf-evil-leader-for-major-keymap (kbd "s") 'eaf-search-it)
      (add-hook 'evil-normal-state-entry-hook
                (lambda ()
                  (when (derived-mode-p 'eaf-mode)
                    (define-key eaf-mode-map (kbd "C-,") eaf-evil-leader-for-major-keymap))))

      (define-key key-translation-map (kbd ",")
        (lambda (prompt)
          (if (derived-mode-p 'eaf-mode)
              (if (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
                  (kbd ",")
                (kbd "C-,"))))))))
