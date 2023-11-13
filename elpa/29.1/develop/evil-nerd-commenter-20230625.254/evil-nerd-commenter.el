;;; evil-nerd-commenter.el --- Comment/uncomment lines efficiently. Like Nerd Commenter in Vim  -*- lexical-binding: t -*-

;; Author: Chen Bin <chenbin.sh@gmail.com>

;; URL: http://github.com/redguardtoo/evil-nerd-commenter
;; Version: 3.6.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience evil
;;
;; This file is not part of GNU Emacs.

;;; Credits:

;; - Lally Oppenheimer added the support for text-object in Evil
;; - Tom Willemse provided the fix to make Emacs 24.4 work

;;; License:

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; This program emulates nerd-commenter.vim by Marty Grenfell.
;;
;; It helps you comment/uncomment multiple lines without selecting them.
;;
;; `M-x evilnc-default-hotkeys` assigns hotkey `M-;` to
;; `evilnc-comment-or-uncomment-lines'
;;
;; `M-x evilnc-comment-or-uncomment-lines` comment or uncomment lines.
;;
;; `M-x evilnc-quick-comment-or-uncomment-to-the-line` will comment/uncomment
;; from current line to specified line.
;; The last digit(s) of line number is parameter of the command.
;;
;; For example, `C-u 9 evilnc-quick-comment-or-uncomment-to-the-line` comments
;; code from current line to line 99 if you current line is 91.
;;
;; Though this program could be used *independently*, it's recommended to
;; us it with Evil.
;;
;; Evil makes you take advantage of power of Vi to comment lines.
;; For example, you can press key `99,ci` to comment out 99 lines.
;;
;; Setup:
;;
;; If comma is your leader key, as most Vim users do, setup is one liner,
;;   (evilnc-default-hotkeys)
;;
;; Or else you can set up key bindings of below commands by yourself.
;;   `evilnc-comment-or-uncomment-lines'
;;   `evilnc-quick-comment-or-uncomment-to-the-line'
;;   `evilnc-comment-and-kill-ring-save'
;;   `evilnc-copy-and-comment-lines'
;;   `evilnc-comment-or-uncomment-paragraphs'
;;   `evilnc-comment-box'
;;   `evilnc-toggle-invert-comment-line-by-line'
;;   `evilnc-copy-and-comment-operator'
;;   `evilnc-comment-operator'
;;   `evilnc-comment-or-uncomment-html-tag'
;;   `evilnc-comment-or-uncomment-html-paragraphs'
;;
;; Set up `evilnc-original-above-comment-when-copy-and-comment'
;; to decide which style to use in `evilnc-copy-and-comment-lines'
;; and `evilnc-copy-and-comment-operator',
;;   - Place the commented out text above original text
;;   - Or place the original text above commented out text
;;
;; `evilnc-yank-and-comment-operator' (un)comment&yank text in one shot.
;;
;; Comment text object "c" is defined.  It can have multi-lines.
;; Press "vac" to select outer object (comment with limiters).
;; Press "vic" to select inner object (comment without limiter).
;; You can assign other key instead of "c" to the text object by
;; customizing `evilnc-comment-text-object'.  Either,
;;   (setq evilnc-comment-text-object "c")
;;   (evilnc-default-hotkeys)
;;
;; Or,
;;   (setq evilnc-comment-text-object "a")
;;   (define-key evil-inner-text-objects-map evilnc-comment-text-object 'evilnc-inner-commenter)
;;   (define-key evil-outer-text-objects-map evilnc-comment-text-object 'evilnc-outer-commenter)
;;
;; You can list of comments in current buffer through using imenu.
;; by setup `imenu-create-index-function' to `evilnc-imenu-create-index-function',
;;
;;   (defun counsel-imenu-comments ()
;;     (interactive)
;;     (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
;;       (unless (featurep 'counsel) (require 'counsel))
;;       (counsel-imenu)))
;;
;; For certain major modes, you need manual setup to override its original
;; keybindings,
;;
;; (defun matlab-mode-hook-config ()
;;   (local-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines))
;; (add-hook 'matlab-mode-hook 'matlab-mode-hook-config)
;;
;; Most commands call `evilnc-comment-or-uncomment-region-function'.
;; You can modify this variable to customize the comment style,
;;
;;   (with-eval-after-load 'evil-nerd-commenter
;;     (defun my-comment-or-uncomment-region (start end)
;;       (let* ((comment-start "aaa")
;;              (comment-end "bbb"))
;;         (evilnc-comment-or-uncomment-region-internal start end)))
;;     (setq evilnc-comment-or-uncomment-region-function
;;           'my-comment-or-uncomment-region))
;;
;; See "Options Controlling Comments" in Emacs manual for comment options.
;;
;; See https://github.com/redguardtoo/evil-nerd-commenter for detail.
;;
;;; Code:

(require 'subr-x) ; required by `string-trim'
(require 'sgml-mode)
(require 'newcomment)
(require 'evil-nerd-commenter-sdk)

(autoload 'count-lines "simple")

(defvar evilnc-original-above-comment-when-copy-and-comment nil
  "Keep the original text above the commented copy, when using either:
`evilnc-copy-and-comment-lines' or `evilnc-copy-and-comment-operator'.")

(defvar evilnc-invert-comment-line-by-line nil
  "If t then invert region comment status line by line.
Please note it has NOT effect on evil text object!")

(defvar evilnc-comment-or-uncomment-region-function
  'evilnc-comment-or-uncomment-region-internal
  "Comment or uncomment region.")

(defvar evilnc-cpp-like-comment-syntax-modes
  '(java-mode
    javascript-mode
    js-mode
    js2-mode
    js3-mode
    rjsx-mode
    js2-jsx-mode
    rust-mode
    c++-mode
    c-mode
    objc-mode)
  "Major modes which has C++ like comment syntax.")

(defvar evilnc-comment-text-object "c"
  "The comment object.
`vic` to select inner object.
`vac` to select outer object.")

(defvar evilnc-use-comment-object-setup t
  "Use evil text object setup when calling `evilnc-default-hotkeys'.")

(defvar evilnc-min-comment-length-for-imenu 8
  "Minimum length of comment to display in imenu.")

(defvar evilnc-html-comment-delimiters
  '((evilnc-html-jsx-p "{/* " " */}")
    ("js-mode" "{/* " " */}")
    (("web-mode" "html-mode") "<!-- " " -->"))
  "List of html tag comment rules.
The 1st item of each rule is the major mode(s) to match current `major-mode'.
Current `major-mode' could equal or derive from the listed major mode(s).
The 2nd and 3rd item is the comment start and comment end.")

(defun evilnc-html-jsx-p ()
  "Test if current file is jsx."
  (and buffer-file-name
       (string-match-p "\.[tj]sx?$" buffer-file-name)))

(defun evilnc-html-match-comment-delimiters-p (target-mode)
  "Use `major-mode' to match TARGET-MODE which could be:
One major mode.
List of major modes.
A function to return t or nil."
  (let* (rlt)
    (cond
     ((functionp target-mode)
      (setq rlt (funcall target-mode)))
     ((stringp target-mode)
      ;; convert name of `major-mode' to symbol
      (when (or (eq major-mode (intern target-mode))
                (derived-mode-p (intern target-mode)))
        (setq rlt t)))
     ((listp target-mode)
      (while (and (not rlt) target-mode)
        (unless (setq rlt (evilnc-html-match-comment-delimiters-p (nth 0 target-mode)))
          ;; continue looping
          (setq target-mode (cdr target-mode))))))
    rlt))

(defun evilnc-html-find-comment-delimiter ()
  "Found comment delimiter from `evilnc-html-find-comment-delimiter'."
  (let* (found
         (delimiters evilnc-html-comment-delimiters)
         rule)
    (while (and (not found) delimiters)
      (setq rule (car delimiters))
      (cond
       ((evilnc-html-match-comment-delimiters-p (nth 0 rule))
        (setq found rule))
       (t
        ;; keep looping
        (setq delimiters (cdr delimiters)))))
    found))

(defun evilnc-html-comment-start ()
  "Return start of html comment."
  (let* ((s (evilnc-html-find-comment-delimiter)))
    (if s (nth 1 s) "<!-- ")))

(defun evilnc-html-comment-end ()
  "Return end of html comment."
  (let* ((s (evilnc-html-find-comment-delimiter)))
    (if s (nth 2 s) " -->")))

(defun evilnc--goto-line (line-num)
  "Shamelessly copied from `goto-line'.  Goto line with LINE-NUM."
  (save-restriction
    (widen)
    (goto-char (point-min))
    (if (eq selective-display t)
      (re-search-forward "[\n\C-m]" nil 'end (1- line-num))
      (forward-line (1- line-num)))))

(defun evilnc--fix-buggy-major-modes ()
  "Fix major modes whose comment regex is buggy.
See http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-03/msg00891.html."
  (cond
   ((eq major-mode 'autoconf-mode)
    ;; since comment-use-syntax is nil in autoconf.el, the comment-start-skip need
    ;; make sure its first parenthesized expression match the string exactly before
    ;; the "dnl", check the comment-start-skip in lisp-mode for sample.
    ;; See code in (defun comment-search-forward) from emacs 24.2:
    ;; (if (not comment-use-syntax)
    ;;     (if (re-search-forward comment-start-skip limit noerror)
    ;;     (or (match-end 1) (match-beginning 0)))
    ;;     (do-something))
    ;; My regex makes sure (match-end 1) return the position of comment starter
    (when (and (boundp 'comment-use-syntax) (not comment-use-syntax))
      ;; Maybe autoconf.el will (setq comment-use-syntax t) in the future?
      (setq comment-start-skip "^\\(\\s*\\)\\(dnl\\|#\\) +")))
   ((eq major-mode 'haml-mode)
    (setq comment-use-syntax nil)
    (setq comment-start "-# ")
    (setq comment-start-skip "-##*[ \t]*"))))

(defun evilnc--forward-line (num)
  "Move NUM source or screen lines forward, depending on visual-line settings."
  (cond
   ((or (and visual-line-mode
             (or (not (bound-and-true-p evil-mode))
                 (bound-and-true-p evil-respect-visual-line-mode)))
        (and (boundp 'display-line-numbers-type)
             (eq display-line-numbers-type 'visual)))
    (vertical-motion num))
   (t
    (forward-line num))))

(defun evilnc--operation-on-lines-or-region (fn &optional num)
  "Apply FN on NUM lines or selected region."
  (cond
   ;; NO region is selected
   ((not (region-active-p))
    (let* ((b (line-beginning-position)) e)
      (save-excursion
        (evilnc--forward-line (- num 1))
        (setq e (line-end-position)))
      (funcall fn b e)))

   ((evilnc-sdk-inside-one-line-p (region-beginning) (region-end))
    (cond
     ;; current comment syntax is NOT fit to comment out a region.
     ;; So we also need hack the `comment-start' and `comment-end'.
     ((and (string= "" comment-end)
           (member major-mode evilnc-cpp-like-comment-syntax-modes))
      (let* ((comment-start-old comment-start)
            (comment-end-old comment-end)
            (comment-start-skip-old comment-start-skip)
            (comment-end-skip-old comment-end-skip))

        ;; use C comment syntax temporarily
        (setq comment-start "/* ")
        (setq comment-end " */")
        (setq comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
        (setq comment-end-skip "[   ]*\\(\\s>\\|\\*+/\\)")

        (funcall fn (region-beginning) (region-end))

        ;; Restore the original comment syntax
        (setq comment-start comment-start-old)
        (setq comment-end comment-end-old)
        (setq comment-start-skip comment-start-skip-old)
        (setq comment-end-skip comment-end-skip-old)))
     ;; just comment out the region
     (t (funcall fn (region-beginning) (region-end)))))

   ;; Select more than one line
   (t
    ;; selected region spans MORE than one line
    (let* ((range (evilnc-sdk-expand-to-contain-whole-lines (region-beginning)
                                                            (region-end)))
           (b (car range))
           (e (cdr range)))
      (save-excursion
        (funcall fn b e))))))

(defun evilnc--get-one-paragraph-region ()
  "Select a paragraph which has NO empty line."
  (let* ((b (save-excursion
              (cond
               ((re-search-backward "^[ \t]*$" nil t)
                (forward-line)
                (line-beginning-position))
               (t
                1))))
         (e (save-excursion
              (cond
               ((re-search-forward "^[ \t]*$" nil t)
                (forward-line -1)
                (line-end-position))
               (t
                (point-max))))))
    (list b e)))

(defun evilnc--invert-comment (start end)
  "Scan the region from START to END line by line, invert its comment status."
  (let* (done b e)
    (save-excursion
      (goto-char end)
      (while (not done)
        (setq b (line-beginning-position))
        (setq e (line-end-position))
        (funcall (if (comment-only-p b e)
                     'uncomment-region 'comment-region)
                 b e)

        (forward-line -1)
        (if (or (= (line-beginning-position) b)
                (< (line-end-position) start))
            (setq done t))))))

(defvar org-src-lang-modes)
(declare-function org-show-subtree "org")
(declare-function outline-up-heading "outline")
(declare-function org-element-property "org-element")

(defun evilnc--org-src-block-info ()
  "Return src-block info in org.  It's like (start end language)."
  (cond
   ;; Emacs 24.4+
   ((fboundp 'org-edit-src-find-region-and-lang)
    (let* ((info (org-edit-src-find-region-and-lang)))
      (list (nth 0 info)
            (1+ (nth 1 info))
            (nth 2 info))))

   ;; Emacs 26.1
   ((fboundp 'org-element-at-point)
    (let* ((elem (org-element-at-point))
           (b (org-element-property :begin elem))
           (e (org-element-property :end elem))
           (lang (org-element-property :language elem))
           (str (buffer-substring-no-properties b e))
           (case-fold-search t))
      (when (string-match ".\+BEGIN_SRC .*$" str)
        (setq b (+ b (length (match-string 0 str)) 1)))
      (save-excursion
        (goto-char b)
        (search-forward "#+END_SRC" e t)
        ;; - 1 to remove line feed
        (setq e (- (point) (length "#+END_SRC"))))
      ;; Now Emacs 24.4 and Emacs 26.1 return same result
      (list b e lang)))))

(defun evilnc--org-lang-major-mode (info)
  "Get `major-mode' from INFO of org source block."
  (let* ((lang (nth 2 info)))
    (cond
     ((setq lang (or (cdr (assoc lang org-src-lang-modes))
                     lang))
      (intern (concat (if (symbolp lang) (symbol-name lang) lang)
                      "-mode")))
     (t
      nil))))

(defun evilnc--working-on-region (start end fn)
  "Region from START to END is applied with operation FN.
Code snippets embedded in Org-mode is identified and right `major-mode' is used."
  (let* ((old-pos (point))
         (info (if (eq major-mode 'org-mode) (evilnc--org-src-block-info)))
         (lang-f (and info (evilnc--org-lang-major-mode info))))

    ;; turn on 3rd party language's major-mode temporarily
    (cond
     (lang-f
      (let* ((src-beg (nth 0 info))
             (src-end (nth 1 info))
             (comment-beg-in-buf (1+ (- start src-beg)))
             (comment-end-in-buf (1+ (- end src-beg)))
             (new-pos (1+ (- old-pos src-beg)))
             (old-code (buffer-substring-no-properties src-beg src-end))
             new-code)
        (with-temp-buffer
          (goto-char (point-min))
          (insert old-code)
          (funcall lang-f)
          (goto-char new-pos)

          (cond
           (evilnc-invert-comment-line-by-line
            (evilnc--invert-comment comment-beg-in-buf comment-end-in-buf))
           (t
            (funcall fn comment-beg-in-buf comment-end-in-buf)))

          (setq new-code (buffer-substring-no-properties (point-min) (point-max))))

        (delete-region src-beg src-end)
        (insert new-code)
        ;; go back to original position when comment inside org src-block
        (goto-char old-pos)))
     (t
      (cond
       (evilnc-invert-comment-line-by-line
        (evilnc--invert-comment start end))
       (t
        (funcall fn start end)))))))

(declare-function web-mode-comment-or-uncomment "ext:web-mode")
(declare-function web-mode-uncomment-erb-block "ext:web-mode")
(declare-function web-mode-comment-erb-block "ext:web-mode")
(defvar web-mode-engine)

(defun evilnc--warn-on-web-mode (is-comment)
  "Check certain part of html code IS-COMMENT."
  (let* ((comment-operation (concat "web-mode-"
                                    (if is-comment "comment-" "uncomment-")
                                    web-mode-engine
                                    "-block")))
    (unless (intern-soft comment-operation)
      (message "defun %s NOT implemented in web-mode! Fix it or report to its maintainer."
               comment-operation))
    is-comment))

;;;###autoload
(defun evilnc-comment-or-uncomment-region-internal (start end)
  "Comment or uncomment region from START to END."
  (cond
   ((eq major-mode 'web-mode)
    ;; elixir is not supported in web-mode for now
    (unless (fboundp 'web-mode-comment-elixir-block)
      (defun web-mode-comment-elixir-block (pos)
        (funcall 'web-mode-comment-erb-block pos))
      (defun web-mode-uncomment-elixir-block (pos)
        (funcall 'web-mode-uncomment-erb-block pos)))
    (web-mode-comment-or-uncomment-region start end))
   (t
    (evilnc--working-on-region start end 'comment-or-uncomment-region))))

;;;###autoload
(defun evilnc-comment-or-uncomment-region (start end)
  "Comment or uncomment region from START to END."
  (funcall evilnc-comment-or-uncomment-region-function start end))

(defun evilnc--current-line-num ()
  "Get current line number."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun evilnc--get-line-num (position)
  "Get line number at POSITION."
  (save-excursion
    (goto-char position)
    (evilnc--current-line-num)))

(defun evilnc--find-destination-linenum (last-digits)
  "Find closest line whose line number ends with digit LAST-DIGITS.
If LAST-DIGITS is 16, line 16, line 116, and line 216 are candidates.
Then if current line is 17, 16 is the final result."
  (let* ((cur-line-num (evilnc--current-line-num))
         (r 1)
         (l (length (number-to-string last-digits))))
    (while (> l 0)
      (setq r (* r 10))
      (setq l (- l 1)))
    (if (>= (mod cur-line-num r) last-digits)
        (setq last-digits (+ last-digits r)))
    (+ cur-line-num (- last-digits (mod cur-line-num r)))))

(defun evilnc-do-paragraphs (action num)
  "Apply ACTION on NUM paragraphs."
  (let* ((i 0)
         rlt
         (b (point-max))
         (e 0)
         linenum)
    (catch 'break
      (while (< i num)
        (when (setq rlt (evilnc--get-one-paragraph-region))
          (setq b (nth 0 rlt))
          (setq e (nth 1 rlt))
          (setq linenum (evilnc--get-line-num e)))

        ;; Original algorithm select all paragraph and comment once
        ;; New algorithm comment paragraph by paragraph
        (when (<= b e)
          (save-excursion
            (funcall action b e)))

        ;; prepare for the next paragraph
        (cond
         ((and rlt (< i num))
          (evilnc--goto-line linenum)

          ;; move to an empty line
          (forward-line)

          ;; move to next non-empty line
          (re-search-forward "^[ \t]*[^ \t]" nil t)
          (if (<= (line-beginning-position) e)
              (throw 'break i)))

          (t
           (throw 'break i)))

        (setq i (1+ i))))))

;;;###autoload
(defun evilnc-comment-or-uncomment-paragraphs (&optional num)
  "Comment or uncomment NUM paragraph(s).
A paragraph is a continuation non-empty lines.
Paragraphs are separated by empty lines."
  (interactive "p")
  (evilnc-do-paragraphs
   (lambda (b e)
     (evilnc--fix-buggy-major-modes)
     (evilnc-comment-or-uncomment-region b e))
   num))

;;;###autoload
(defun evilnc-comment-or-uncomment-to-the-line (&optional line-num)
  "Comment or uncomment from current line to LINE-NUM line."
  (interactive "nLine: ")
  (if (not (region-active-p))
      (let* ((b (line-beginning-position))
             (e (line-end-position)))
        (save-excursion
          (evilnc--goto-line line-num)
          (if (< (line-beginning-position) b)
              (setq b (line-beginning-position)))
          (if (> (line-end-position) e)
              (setq e (line-end-position)))
          (evilnc--fix-buggy-major-modes)
          (evilnc-comment-or-uncomment-region b e)))))

;;;###autoload
(defun evilnc-quick-comment-or-uncomment-to-the-line (&optional last-digits)
  "Comment/uncomment to line number by LAST-DIGITS.
For example, you can use either \
\\<M-53>\\[evilnc-quick-comment-or-uncomment-to-the-line] \
or \\<M-3>\\[evilnc-quick-comment-or-uncomment-to-the-line] \
to comment to the line 6453"
  (interactive "p")
  (let* ((l (evilnc--find-destination-linenum last-digits)))
    (evilnc-comment-or-uncomment-to-the-line l)
    (evilnc--goto-line (+ 1 l))))

;;;###autoload
(defun evilnc-toggle-invert-comment-line-by-line ()
  "Please note this command may NOT work on complex evil text objects."
  (interactive)
  (if evilnc-invert-comment-line-by-line
      (setq evilnc-invert-comment-line-by-line nil)
    (setq evilnc-invert-comment-line-by-line t))
  (message (if evilnc-invert-comment-line-by-line
               "Each line's comment status will be inverted"
             "Each line's comment status will NOT be inverted")))

;;;###autoload
(defun evilnc-toggle-comment-empty-lines ()
  "Toggle the flag which decide if empty line will be commented."
  (interactive)
  (if comment-empty-lines
      (setq comment-empty-lines nil)
    (setq comment-empty-lines t))
  (message (if comment-empty-lines
               "Empty line(s) will be commented"
             "Empty line(s) will NOT be commented")))

(defun evilnc-visual-line-p ()
  "In Evil visual line mode."
  (and (fboundp 'evil-visual-type) (eq (evil-visual-type) 'line)))

(defun evilnc-guess-position-at-point ()
  "Guess current position when evil visual line state is on or off."
  (cond
   ((and (evilnc-visual-line-p)
         (eq (point) (nth 1 (evil-visual-range))))
    ;; In evil visual line state, point is beginning or end visual range
    (cons (line-number-at-pos) (1- (current-column))))
   (t
    (cons (line-number-at-pos) (current-column)))))

;;;###autoload
(defun evilnc-comment-or-uncomment-lines (&optional num)
  "Comment or uncomment NUM lines.  NUM could be negative.

Case 1: If no region selected, comment/uncomment on current line.
If NUM>1, comment/uncomment extra N-1 lines from next line.

Case 2: Selected region is expanded to make it contain whole lines.
Then we comment/uncomment the expanded region.  NUM is ignored.

Case 3: If a region inside of ONE line is selected,
we comment/uncomment that region.
CORRECT comment syntax will be used for C++/Java/Javascript."
  (interactive "p")
  (let* ((orig-pos (evilnc-guess-position-at-point)))
    ;; donot move the cursor
    ;; support negative number
    (cond
     ((and (= 1 num) (string-match "^[ \t]*$" (evilnc-sdk-cur-line)))
      ;; comment on current empty line
      (comment-dwim nil))
     (t
      (save-excursion
        (when (< num 0)
          (evilnc--forward-line (1+ num))
          (setq num (- 0 num)))
        (evilnc--operation-on-lines-or-region
         (lambda (b e)
           (evilnc--fix-buggy-major-modes)
           ;; when comment in evil visual state, the cursor may be rogue
           (when (evilnc-visual-line-p) (evil-normal-state))
           (evilnc-comment-or-uncomment-region b e))
         num))

      (evilnc--goto-line (car orig-pos))
      ;; make sure we stay on original line
      (goto-char (min (+ (line-beginning-position) (cdr orig-pos))
                     (1- (line-end-position))))))))

;;;###autoload
(defun evilnc-copy-and-comment-lines (&optional num)
  "Copy&paste NUM lines and comment out original lines.
NUM could be negative.

Case 1: If no region selected, operate on current line.
if NUM>1, comment/uncomment extra N-1 lines from next line

Case 2: Selected region is expanded to make it contain whole lines.
Then we operate the expanded region.  NUM is ignored."
  (interactive "p")
  ;; support negative number
  (when (< num 0)
    (evilnc--forward-line (1+ num))
    (setq num (- 0 num)))

  (let* ((original-column (current-column)))
    (evilnc--operation-on-lines-or-region
     '(lambda (start end)
        (evilnc--fix-buggy-major-modes)
        (let* ((str (buffer-substring-no-properties start end)))
          (cond
           (evilnc-original-above-comment-when-copy-and-comment
            (let* ((p (point)))
              (comment-region start end)
              (goto-char start)
              (insert-before-markers (concat str "\n"))
              (goto-char p)))
           (t
            (goto-char end)
            (newline 1)
            (insert-before-markers str)
            (comment-region start end)))))
     num)
    ;; Go to original column after evilnc-copy-and-comment-lines
    ;; @see https://github.com/redguardtoo/evil-nerd-commenter/issues/79
    ;; Thanks for Kevin Brubeck (AKA unhammer) for idea/implementation
    (move-to-column original-column)))

;;;###autoload
(defun evilnc-comment-and-kill-ring-save (&optional num)
  "Comment lines save origin lines into `kill-ring'.
NUM could be negative.

Case 1: If no region selected, operate on current line.
;; if NUM>1, comment/uncomment extra N-1 lines from next line

Case 2: Selected region is expanded to make it contain whole lines.
Then we operate the expanded region.  NUM is ignored."
  (interactive "p")
  ;; support negative number
  (when (< num 0)
    (evilnc--forward-line (1+ num))
    (setq num (- 0 num)))

  (evilnc--operation-on-lines-or-region
   '(lambda (start end)
      (evilnc--fix-buggy-major-modes)
      (kill-new (buffer-substring-no-properties start end))
      (comment-region start end))
   num))

;; {{ for non-evil user only
;;;###autoload
(defun evilnc-copy-to-line (&optional LINENUM)
  "Copy from current line to LINENUM line.  For non-evil user only."
  (interactive "nCopy to line: ")
  (if (not (region-active-p))
      (let* ((b (line-beginning-position))
             (e (line-end-position)))
        (save-excursion
          (evilnc--goto-line LINENUM)
          (if (< (line-beginning-position) b)
              (setq b (line-beginning-position)))
          (if (> (line-end-position) e)
              (setq e (line-end-position)))
          (kill-new (buffer-substring-no-properties b e))))))

;;;###autoload
(defun evilnc-kill-to-line (&optional linenum)
  "Kill from the current line to the LINENUM line.  For non-evil user only."
  (interactive "NKill to line: ")
  (if (not (region-active-p))
      (let* ((b (line-beginning-position))
             (e (line-end-position)))
        (save-excursion
          (evilnc--goto-line linenum)
          (if (< (line-beginning-position) b)
              (setq b (line-beginning-position)))
          (if (> (line-end-position) e)
              (setq e (line-end-position)))
          ;; +1 because we need remove the CR
          (setq e (+ 1 e))
          (if (> e (point-max)) (setq e (point-max)))
          (kill-region b e)))))
;; }}

;;;###autoload
(defun evilnc-version ()
  "The version number."
  (interactive)
  (message "3.5.8"))

(defvar evil-normal-state-map)
(defvar evil-visual-state-map)
(defvar evil-inner-text-objects-map)
(defvar evil-outer-text-objects-map)

;;;###autoload
(defun evilnc-default-hotkeys (&optional no-evil-keybindings no-emacs-keybindings)
  "Setup the key bindings of evil-nerd-comment.
If NO-EVIL-KEYBINDINGS is t, we don't define keybindings in EVIL,
if NO-EMACS-KEYBINDINGS is t, we don't define keybindings in EMACS mode."
  (interactive)
  ;; Install hotkeys for Emacs mode
  (unless no-emacs-keybindings
    (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
    (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
    (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
    (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs))

  ;; Install key bindings for evil
  (when (and (not no-evil-keybindings) (fboundp 'evil-mode))
    (define-key evil-normal-state-map ",ci" 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map ",ci" 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map ",cl" 'evilnc-quick-comment-or-uncomment-to-the-line)
    (define-key evil-normal-state-map ",ll" 'evilnc-quick-comment-or-uncomment-to-the-line)
    (define-key evil-normal-state-map ",cc" 'evilnc-copy-and-comment-lines)
    (define-key evil-visual-state-map ",cc" 'evilnc-copy-and-comment-lines)
    (define-key evil-normal-state-map ",cp" 'evilnc-comment-or-uncomment-paragraphs)
    (define-key evil-normal-state-map ",cs" 'evilnc-comment-box)
    (define-key evil-visual-state-map ",cs" 'evilnc-comment-box)
    (define-key evil-normal-state-map ",cr" 'comment-or-uncomment-region)
    (define-key evil-normal-state-map ",cv" 'evilnc-toggle-invert-comment-line-by-line)

    ;; comment itself is text object
    (define-key evil-inner-text-objects-map evilnc-comment-text-object 'evilnc-inner-commenter)
    (define-key evil-outer-text-objects-map evilnc-comment-text-object 'evilnc-outer-commenter)

    (when evilnc-use-comment-object-setup
      ;; Install operator for evil text objects
      (define-key evil-normal-state-map ",." 'evilnc-copy-and-comment-operator)
      (define-key evil-visual-state-map ",." 'evilnc-copy-and-comment-operator)
      (define-key evil-normal-state-map ",," 'evilnc-comment-operator)
      (define-key evil-visual-state-map ",," 'evilnc-comment-operator))))

(defun evilnc-frame-wide-string (s)
  "Build summary from S."
  (let* ((w (frame-width))
         ;; display kill ring item in one line
         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
    ;; strip the whitespace
    (setq key (replace-regexp-in-string "^[ \t]+" "" key))
    ;; fit to the minibuffer width
    (if (> (length key) w)
        (setq key (concat (substring key 0 (- w 4)) "...")))
    key))

;;;###autoload
(defun evilnc-imenu-create-index-function ()
  "Imenu function find comments."
  (let* (start end linenum str searching cands)
    (save-excursion
      (goto-char (point-min))
      ;; learn this skill from etags-select
      ;; use simple string search to speed up searching
      (setq searching t)
      (while searching
        ;; C/C++ might use "/* " as comment-start
        (setq start (search-forward (string-trim comment-start) (point-max) t))
        ;; OK, it's comment
        (cond
         ((not start)
          (setq searching nil))

         (t
          (setq start (1+ start))))

        (when (and searching (evilnc-comment-p start))
          (setq linenum (line-number-at-pos start) )

          (cond
           ((string= comment-end "")
            (setq end (line-end-position)))

           (t
            (setq end (search-forward comment-end (point-max) t))))

          (cond
           ((and end (> end start))
            (setq str (string-trim (buffer-substring-no-properties start end)))
            ;; no empty line
            (setq str (replace-regexp-in-string "[\r\n]+" "\n" str))
            ;; could be multi-lines comment
            (let* ((a (split-string str "[\r\n]+"))
                   (pre-pattern (concat "^[ \t]*["
                                        (string-trim comment-start)
                                        "]*[ \t]*"))
                   (post-pattern (concat "[ \t]*["
                                         (string-trim comment-end)
                                         "]*[ \t]*$")))
              ;; remove empty lines
              (setq a (delq nil (mapcar (lambda (s)
                                          (setq s (replace-regexp-in-string pre-pattern "" s))
                                          (setq s (replace-regexp-in-string post-pattern "" s))
                                          (setq s (string-trim s))
                                          (unless (string-match-p "^[ \t]*$" s) s))
                                        a)))
              (setq str (mapconcat 'identity a "\n" )))


            (when (and (not (string-match-p "^[ \t\n\r]*$" str))
                       (> (length str) evilnc-min-comment-length-for-imenu))
              (let* ((m (make-marker)))
                (set-marker m start)
                (push (cons (evilnc-frame-wide-string (format "%d:%s" linenum str)) m)
                      cands)))

            (goto-char (min (1+ end) (point-max))))

           (t
            (setq searching nil))))))
    (nreverse cands)))

(defun evilnc-html-comment-region (start end)
  "Comment region between START and END."
  (save-excursion
    (goto-char end)
    (insert (evilnc-html-comment-end))
    (goto-char start)
    (insert (evilnc-html-comment-start))))

(defun evilnc-html-uncomment-region (start end)
  "Uncomment HTML tag between START and END."
  (let* (mark-start-pos
         mark-end-pos
         (len-comment-start (length (evilnc-html-comment-start))))
    (save-excursion
      (goto-char start)
      (setq mark-start-pos (search-forward (evilnc-html-comment-start) end t))
      (goto-char end)
      (setq mark-end-pos (search-backward (evilnc-html-comment-end) start t))
      (when (and mark-start-pos mark-end-pos)
        (goto-char mark-end-pos)
        (delete-char (length (evilnc-html-comment-end)))
        (goto-char (- mark-start-pos len-comment-start))
        (delete-char len-comment-start)))))

(defun evilnc-html-tag-comment-p (start)
  "Test if html tag comment is at position START position."
  (save-excursion
    (goto-char start)
    (string-match-p (concat "^[ \t]*" (regexp-quote (evilnc-html-comment-start)))
                    (evilnc-sdk-cur-line))))

;;;###autoload
(defun evilnc-comment-or-uncomment-html-tag ()
  "Comment or uncomment html tag(s).
If no region is selected, current tag under focus is automatically selected.
In this case, only one tag is selected.
If users manually select region, the region could cross multiple sibling tags
and automatically expands to include complete tags.
Users can press \"v\" key in evil mode to select multiple tags.
This command is not dependent on any 3rd party package."
  (interactive)
  (let* (beg end beg-line-beg end-line-end)
    (cond
     ((region-active-p)
      (setq beg (region-beginning))
      (setq end (region-end))
      (save-excursion
        (goto-char beg)
        (setq beg-line-beg (line-beginning-position))
        (goto-char end)
        (setq end-line-end (line-end-position))
        (when (< beg-line-beg (line-beginning-position))
          ;; it's multiple lines region
          (goto-char beg-line-beg)
          (setq beg (re-search-forward "^[ \t]*" end t))
          (setq end end-line-end))))
     (t
      (save-excursion
        (sgml-skip-tag-backward 1)
        (setq beg (point))
        (setq beg-line-beg (line-beginning-position))
        (sgml-skip-tag-forward 1)
        (setq end (point))
        (setq end-line-end (line-end-position)))))

    (cond
     ((evilnc-html-tag-comment-p beg)
      ;; make sure all tags plus comment marks are selected
      (evilnc-html-uncomment-region beg-line-beg end-line-end))
     (t
      ;; the whole tags is already selected
      (evilnc-html-comment-region beg end)))))

;;;###autoload
(defun evilnc-comment-or-uncomment-html-paragraphs (&optional num)
  "Comment or uncomment NUM paragraphs contain html tag.
A paragraph is a continuation non-empty lines.
Paragraphs are separated by empty lines."
  (interactive "p")
  (evilnc-do-paragraphs
   (lambda (b e)
     (ignore e)
     ;; {{ make sure tag is focused
     (goto-char b)
     (sgml-skip-tag-forward 1)
     ;; }}
     (evilnc-comment-or-uncomment-html-tag))
   num))

(defun evilnc-comment-box (&optional num)
  "Comment out NUM lines, putting it inside a box.  NUM could be negative.
If NUM is nil, comment out current paragraph.  This command uses `comment-box'."
  (interactive "P")
  (let* ((orig-pos (evilnc-guess-position-at-point))
         range)

    (cond
     ((or (not num))
      (when (setq range (bounds-of-thing-at-point 'paragraph))
        (evilnc--fix-buggy-major-modes)
        (when (evilnc-visual-line-p) (evil-normal-state))
        (comment-box (car range) (cdr range) 1)))
     (t
      (save-excursion
        (when (< num 0)
          (evilnc--forward-line (1+ num))
          (setq num (- 0 num)))
        (evilnc--operation-on-lines-or-region
         (lambda (b e)
           (evilnc--fix-buggy-major-modes)
           ;; when comment in evil visual state, the cursor may be rogue
           (when (evilnc-visual-line-p) (evil-normal-state))
           (comment-box b e 1))
         num))))

    (unless (eq (evilnc-guess-position-at-point) orig-pos)
      (evilnc--goto-line (car orig-pos))
      ;; make sure we stay on original line
      (goto-char (min (+ (line-beginning-position) (cdr orig-pos))
                      (1- (line-end-position)))))))

;; Attempt to define the operator on first load.
;; Will only work if evil has been loaded
(with-eval-after-load 'evil
  (require 'evil-nerd-commenter-operator))

(provide 'evil-nerd-commenter)
;;; evil-nerd-commenter.el ends here
