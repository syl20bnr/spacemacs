;; -*- coding:euc-jp-unix; -*-
;; close-open-paren.el : insert appropriate closing parenthesis.
;; Ref.: http://www.emacswiki.org/emacs/UniversialCloseParen
;;       http://emacswiki.org/emacs/EmacsSyntaxTable
;;       skk/skk-tankan.el

(defconst close-open-paren-syntax-table
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?{ "(}" table)
      (modify-syntax-entry ?} "){" table)
      (modify-syntax-entry ?\( "()" table)
      (modify-syntax-entry ?\) ")(" table)
      (modify-syntax-entry ?\[ "(]" table)
      (modify-syntax-entry ?\] ")[" table)
      (modify-syntax-entry ?< "(>" table)
      (modify-syntax-entry ?> ")<" table)
      (modify-syntax-entry ?¡Ê "(¡Ë" table) ; »Ï¤á¾®³ç¸Ì,»Ï¤á´Ý³ç¸Ì, LEFT PARENTHESIS
      (modify-syntax-entry ?¡Ë ")¡Ê" table) ; ½ª¤ï¤ê¾®³ç¸Ì,½ª¤ï¤ê´Ý³ç¸Ì, RIGHT PARENTHESIS
      (modify-syntax-entry ?¡Ì "(¡Í" table) ; »Ï¤á¤­¤Ã¤³¤¦(µµ¹Ã)³ç¸Ì, LEFT TORTOISE SHELL BRACKET
      (modify-syntax-entry ?¡Í ")¡Ì" table) ; ½ª¤ï¤ê¤­¤Ã¤³¤¦(µµ¹Ã)³ç¸Ì, RIGHT TORTOISE SHELL BRACKET
      (modify-syntax-entry ?¡Î "(¡Ï" table) ; »Ï¤áÂç³ç¸Ì,»Ï¤á³Ñ³ç¸Ì, LEFT SQUARE BRACKET
      (modify-syntax-entry ?¡Ï ")¡Î" table) ; ½ª¤ï¤êÂç³ç¸Ì,½ª¤ï¤ê³Ñ³ç¸Ì, RIGHT SQUARE BRACKET
      (modify-syntax-entry ?¡Ð "(¡Ñ" table) ; »Ï¤áÃæ³ç¸Ì,»Ï¤áÇÈ³ç¸Ì, LEFT CURLY BRACKET
      (modify-syntax-entry ?¡Ñ ")¡Ð" table) ; ½ª¤ï¤êÃæ³ç¸Ì,½ª¤ï¤êÇÈ³ç¸Ì, RIGHT CURLY BRACKET
      (modify-syntax-entry ?¡Ò "(¡Ó" table) ; »Ï¤á»³³ç¸Ì, LEFT ANGLE BRACKET
      (modify-syntax-entry ?¡Ó ")¡Ò" table) ; ½ª¤ï¤ê»³³ç¸Ì, RIGHT ANGLE BRACKET
      (modify-syntax-entry ?¡Ô "(¡Õ" table) ; »Ï¤áÆó½Å»³³ç¸Ì, LEFT DOUBLE ANGLE BRACKET
      (modify-syntax-entry ?¡Õ ")¡Ô" table) ; ½ª¤ï¤êÆó½Å»³³ç¸Ì, RIGHT DOUBLE ANGLE BRACKET
      (modify-syntax-entry ?¡Ö "(¡×" table) ; »Ï¤á¤«¤®³ç¸Ì, LEFT CORNER BRACKET
      (modify-syntax-entry ?¡× ")¡Ö" table) ; ½ª¤ï¤ê¤«¤®³ç¸Ì, RIGHT CORNER BRACKET
      (modify-syntax-entry ?¡Ø "(¡Ù" table) ; »Ï¤áÆó½Å¤«¤®³ç¸Ì, LEFT WHITE CORNER BRACKET
      (modify-syntax-entry ?¡Ù ")¡Ø" table) ; ½ª¤ï¤êÆó½Å¤«¤®³ç¸Ì, RIGHT WHITE CORNER BRACKET
      (modify-syntax-entry ?¡Ú "(¡Û" table) ; »Ï¤á¤¹¤ßÉÕ¤­³ç¸Ì, LEFT BLACK LENTICULAR BRACKET
      (modify-syntax-entry ?¡Û ")¡Ú" table) ; ½ª¤ï¤ê¤¹¤ßÉÕ¤­³ç¸Ì, RIGHT BLACK LENTICULAR BRACKET
      (modify-syntax-entry ?\\ "'" table)
      table)
    "A syntax table for pairs of parentheses.")

(defun close-open-paren ()
  (interactive)
  (let (pos closing)
  (with-syntax-table close-open-paren-syntax-table    ; ¸¡º÷¤Ë¤Ï¾å¤ÎÀìÍÑÊ¸Ë¡¥Æ¡¼¥Ö¥ë¤òÍøÍÑ
	  (setq pos (save-excursion (up-list -1) (point)))  ; ³«¤­³ç¸Ì¤Î°ÌÃÖ¤òpos¤ØÂåÆþ
	  (setq closing (matching-paren (char-after pos)))) ; pos¤Î°ÌÃÖ¤Ë¤¢¤ë³«¤­³ç¸ÌÂÐ±þ¤¹¤ëÊÄ¤¸³ç¸Ì¤òclosing¤Ø
	(insert closing)))                                  ; ÊÄ¤¸³ç¸Ì¤ò¸½ºß¤Î°ÌÃÖ¤ØÁÞÆþ

(provide 'auto-close-parens)
