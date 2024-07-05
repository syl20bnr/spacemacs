;;; packages.el --- whisper layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Alexander Matyasko <amatyasko@amatyasko-PC>
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

(defconst whisper-packages
  '((whisper :location (recipe
                        :fetcher github
                        :repo "natrys/whisper.el"))))

(defun whisper/init-whisper ()
  (use-package whisper
    :defer t
    :init
    (spacemacs/declare-prefix "$w" "Whisper")
    (spacemacs/set-leader-keys
      "$wr" 'whisper-run ; Start recording and transcribe/translate audio
      "$wf" 'whisper-file-run ; Transcribe/translate a local file
      "$wl" 'spacemacs/whisper-select-language ; Select transcription language
      "$wm" 'spacemacs/whisper-select-model))) ; Select base model for transcription
