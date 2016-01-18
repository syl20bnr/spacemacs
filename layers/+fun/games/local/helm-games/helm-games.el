;;; helm-games.el --- Games selection with `helm'.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: helm, spacemacs, games
;; Version: 0.1
;; Package-Requires: ((helm "1.5"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package adds a convenient way to discover Spacemacs configuration
;; layers thanks to helm.

;;; Code:

(require 'helm)

(defvar helm-games-list '()
  "List of game candidates to pass to helm.")

;;;###autoload
(defun helm-games ()
  "Games selection with helm interface."
  (interactive)
  (helm :buffer "*helm: games*"
        :sources `(,(helm-games//games-source))))

(defun helm-games//games-source ()
  "Construct the helm source for the games."
  `((name . "Games")
    (candidates . ,(helm-games//game-candidates))
    (candidate-number-limit)
    (action . (("Run" . helm-games//run-game)
               ("Quit" . helm-games//quit-game)
               ("Reset" . helm-games//reset-game)))))

(defun helm-games//game-candidates ()
  "Return the list of game candidates."
  (setq helm-games-list (sort helm-games-list
                              (lambda (x y)
                                (string< (car x) (car y)))))
  helm-games-list)

(defun helm-games//run-game (candidate)
  "Run the selected game."
  (message "%s" candidate)
  (let ((func (car candidate)))
    (when func
      (call-interactively func))))

(defun helm-games//quit-game (candidate)
  "Quit the selected game."
  (let ((func (plist-get (cdr candidate) :quit)))
    (when func
      (if (listp func)
          (eval func)
        (funcall func)))))

(defun helm-games//reset-game (candidate)
  "Reset the selected game."
  (let ((func (plist-get (cdr candidate) :reset)))
    (when func
      (if (listp func)
          (eval func)
        (funcall func)))))

(provide 'helm-games)

;;; helm-games.el ends here
