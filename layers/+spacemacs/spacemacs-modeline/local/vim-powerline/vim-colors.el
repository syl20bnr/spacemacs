;; The colors in this file are from the original vim-powerline
;; repository at https://github.com/Lokaltog/vim-powerline/blob/develop/autoload/Powerline/Colorschemes/default.vim

(defun powerline--expand-alist (alist)
  (cl-loop for (keys . vals) in alist append
     (mapcar #'(lambda (key) (cons key vals)) keys)))

(defun powerline--build-face (fg &optional bg bold)
  `((t (:foreground ,fg
        :background ,bg
        :bold ,bold
        :overline ,(when (null powerline-default-separator) bg)
        :underline nil
        :box ,(when (eq powerline-default-separator 'utf-8) bg)))))

(defun powerline--gen-facedef (feature mode args)
  (let* ((name (format "powerline-%s-%s" feature mode))
         (doc (format "Powerline face %s" name)))
    `(defface ,(intern name)
       (quote ,(apply 'powerline--build-face args))
       ,doc)))

(defun powerline--generate-facedefs (alist)
  (cl-loop for (feature . modes) in alist append
    (cl-loop for (mode . args) in modes collect
       (powerline--gen-facedef feature mode args))))

;; Color definitions
(setf powerline-vim-colors-alist
      (let ((black "#000000")                 ; 16
            (white "#FFFFFF")                 ; 231

            (darkestgreen "#005F00")          ; 22
            (darkgreen "#008700")             ; 28
            (mediumgreen "#5faf00")           ; 70
            (brightgreen "#afd700")           ; 148

            (darkestcyan "#005f5f")           ; 23
            (mediumcyan "#87d7ff")            ; 117

            (darkestblue "#005f87")           ; 24
            (darkblue "#0087af")              ; 31

            (darkestred "#5f0000")            ; 52
            (darkred "#870000")               ; 88
            (mediumred "#af0000")             ; 124
            (brightred "#d70000")             ; 160
            (brightestred "#ff0000")          ; 196

            (darkestpurple "#5f00af")         ; 55
            (mediumpurple "#875fd7")          ; 98
            (brightpurple "#d7d7ff")          ; 189

            (brightorange "#ff8700")          ; 208
            (brightestorange "#ffaf00")       ; 214

            (gray0 "#121212")                 ; 233
            (gray1 "#262626")                 ; 235
            (gray2 "#303030")                 ; 236
            (gray3 "#4e4e4e")                 ; 239
            (gray4 "#585858")                 ; 240
            (gray5 "#626262")                 ; 241
            (gray6 "#808080")                 ; 244
            (gray7 "#8a8a8a")                 ; 245
            (gray8 "#9e9e9e")                 ; 247
            (gray9 "#bcbcbc")                 ; 250
            (gray10 "#d0d0d0"))               ; 252

        (powerline--expand-alist
         `((("SPLIT")
            (normal      ,white           ,gray2)
            (inactive    ,white           ,gray0)
            (insert      ,white           ,darkestblue))

           (("state_indicator")
            (normal       ,darkestgreen    ,brightgreen  t)
            (inactive     ,gray6           ,gray2        t)
            (insert       ,darkestcyan     ,white        t)
            (visual       ,darkred         ,brightorange t)
            (replace      ,white           ,brightred    t)
            (select       ,white           ,gray5        t)
            (motion       ,brightpurple    ,mediumpurple t)
            (emacs        ,darkestcyan     ,white        t)
            (iedit        ,darkred         ,brightestred t)
            (lisp         ,brightpurple    ,mediumpurple t)
            (hybrid       ,darkestblue     ,mediumcyan   t))

           (("branch" "scrollpercent" "raw" "filesize")
            (normal      ,gray9           ,gray4)
            (inactive    ,gray4           ,gray1)
            (insert      ,mediumcyan      ,darkblue))

           (("fileinfo" "filename")
            (normal      ,white           ,gray4        t)
            (inactive    ,gray7           ,gray1        t)
            (insert      ,white           ,darkblue     t))

           (("fileinfo.filepath")
            (normal      ,gray10)
            (inactive    ,gray5)
            (insert      ,mediumcyan))

           (("static_str")
            (normal      ,white           ,gray4)
            (inactive    ,gray7           ,gray1)
            (insert      ,white           ,darkblue))

           (("fileinfo.flags")
            (normal      ,brightestred    nil          t)
            (inactive    ,darkred)
            (insert      ,brightestred    nil          t))

           (("currenttag" "fullcurrenttag" "fileformat" "fileencoding"
             "pwd" "filetype" "rvm.string" "rvm.statusline"
             "virtualenv.statusline" "charcode" "currhigroup")
            (normal      ,gray8           ,gray2)
            (inactive    ,gray3           ,gray0)
            (insert      ,mediumcyan      ,darkestblue))

           (("lineinfo")
            (normal      ,gray2           ,gray10       t)
            (inactive    ,gray7           ,gray1        t)
            (insert      ,darkestcyan     ,mediumcyan   t))

           (("errors")
            (normal      ,brightestorange ,gray2 t)
            (insert      ,brightestorange ,darkestblue  t))

           (("lineinfo.line.tot")
            (normal      ,gray6)
            (inactive    ,gray5)
            (insert      ,darkestcyan))

           (("paste_indicator" "ws_marker")
            (normal      ,white           ,brightred    t))

           (("gundo.static_str.name" "command_t.static_str.name")
            (normal      ,white           ,mediumred    t)
            (inactive    ,brightred       ,darkestred   t))

           (("gundo.static_str.buffer" "command_t.raw.line")
            (normal      ,white           ,darkred)
            (inactive    ,brightred       ,darkestred))

           (("gundo.SPLIT" "command_t.SPLIT")
            (normal      ,white           ,darkred)
            (inactive    ,white           ,darkestred))

           (("lustyexplorer.static_str.name"
             "minibufexplorer.static_str.name"
             "nerdtree.raw.name" "tagbar.static_str.name")
            (normal      ,white           ,mediumgreen  t)
            (inactive    ,mediumgreen     ,darkestgreen t))

           (("lustyexplorer.static_str.buffer"
             "tagbar.static_str.buffer")
            (normal      ,brightgreen     ,darkgreen)
            (inactive    ,mediumgreen     ,darkestgreen))

           (("lustyexplorer.SPLIT" "minibufexplorer.SPLIT"
             "nerdtree.SPLIT" "tagbar.SPLIT")
            (normal      ,white           ,darkgreen)
            (inactive    ,white           ,darkestgreen))

           (("ctrlp.focus" "ctrlp.byfname")
            (normal      ,brightpurple    ,darkestpurple))

           (("ctrlp.prev" "ctrlp.next" "ctrlp.pwd")
            (normal      ,white           ,mediumpurple))

           (("ctrlp.item")
            (normal      ,darkestpurple   ,white         t))

           (("ctrlp.marked")
            (normal      ,brightestred    ,darkestpurple t))

           (("ctrlp.count")
            (normal      ,darkestpurple   ,white))

           (("ctrlp.SPLIT")
            (normal      ,white           ,darkestpurple))))))

(provide 'vim-colors)
;; Local Variables:
;; no-byte-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; indent-tabs-mode: nil
;; End:
;;; vim-colors.el ends here
