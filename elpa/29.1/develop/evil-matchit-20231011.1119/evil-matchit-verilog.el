;;; evil-matchit-verilog.el --- verilog plugin of evil-matchit

;; Copyright (C) 2014-2020 Chen Bin

;; Author: Chen Bin

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; evil-matchit is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;; Code:

(require 'evil-matchit-sdk)

;; {{ Sample verilog code:
;; module dff_lab;
;;    reg data,rst;
;;    // Connecting ports by name.(map)
;;    dff d1 (.qb(outb), .q(out),
;;            .clk(clk),.d(data),.rst(rst));
;;    // overriding module parameters
;;    defparam
;;      dff_lab.dff.n1.delay1 = 5 ,
;;      dff_lab.dff.n2.delay2 = 6 ;
;;    // full-path referencing is used
;;    // over-riding by using #(8,9) delay1=8..
;;    dff d2 #(8,9) (outc, outd, clk, outb, rst);
;;    // clock generator
;;    always clk = #10 ~clk ;
;;    // stimulus ... contd
;;    initial begin: stimuli // named block stimulus
;;       clk = 1; data = 1; rst = 0;
;;       #20 rst = 1;
;;       #20 data = 0;
;;       #600 $finish;
;;    end
;;    initial // hierarchy: downward path referencing
;;      begin
;;         #100 force dff.n2.rst = 0 ;
;;         #200 release dff.n2.rst;
;;      end
;; endmodule
;; }}

;; should try next howto, the purpose is avoid missing any howto
(defvar evilmi-verilog-extract-keyword-howtos
  '(("^[ \t]*\\(while\\|module\\|primitive\\|case\\|function\\|specify\\|table\\|class\\|program\\|clocking\\|property\\|sequence\\|package\\|covergroup\\|generate\\|interface\\|task\\|fork\\|join[a-z]*\\)" 1)
    ("^[ \t]*\\(end[a-z]+\\)" 1)
    ("^[ \t]*\\(`[a-z]+\\)" 1) ; macro
    ("\\([^a-zA-Z_]\\|^\\)\\(begin\\|end\\)\\([^a-zA-Z_]\\|$\\)" 2)))

(defvar evilmi-verilog-match-tags
  '(("module" () "endmodule" "MONOGAMY")
    ("primitive" () "endprimitive" "MONOGAMY")
    ("case" () "endcase" "MONOGAMY")
    ("function" () "endfunction" "MONOGAMY")
    ("specify" () "endspecify" "MONOGAMY")
    ("table" () "endtable" "MONOGAMY")
    ("class" () "endclass" "MONOGAMY")
    ("program" () "endprogram" "MONOGAMY")
    ("clocking" () "endclocking" "MONOGAMY")
    ("property" () "endproperty" "MONOGAMY")
    ("sequence" () "endsequence" "MONOGAMY")
    ("package" () "endpackage" "MONOGAMY")
    ("covergroup" () "endgroup" "MONOGAMY")
    ("generate" () "endgenerate" "MONOGAMY")
    ("interface" () "endinterface" "MONOGAMY")
    ("task" () "endtask" "MONOGAMY")
    ("fork" () ("join" "join_any" "join_none") "MONOGAMY")
    ("begin" () "end")
    ("`ifn?def" "`else" "`endif" "MONOGAMY")
    ("`celldefine" () "`endcelldefine" "MONOGAMY")))

(defvar evilmi-verilog-block-begin-prefix
  "^if\\(n?def\\)?\\|else\\( if\\)?\\|initial\\|always$"
  "Keyword before the block \"begin\".")

(defun evilmi-verilog-open-tag-p (token)
  "TOKEN is the open tag."
  (string= "begin" (buffer-substring-no-properties (cadr token)
                                                   (cddr token))))

(defun evilmi-verilog-parse-at-point ()
  "Parse tokens at point."
  (let* ((tokens (evilmi-sdk-tokens 3))
         info)
    (when (and tokens (> (length tokens) 1))
      (let* ((first-token (car tokens))
             (b (cadr first-token))
             (e (cddr first-token))
             start
             tag)
        (when (and (string-match evilmi-verilog-block-begin-prefix
                                 (buffer-substring-no-properties b e))
                   (setq tag
                         (cl-find-if #'evilmi-verilog-open-tag-p (cdr tokens))))
          (setq start (line-beginning-position))
          ;; move focus to the "begin"
          (goto-char (cadr tag))
          (setq info (evilmi-sdk-get-tag evilmi-verilog-match-tags
                                         evilmi-verilog-extract-keyword-howtos))
          (setq info (cons start (cdr info))))))
    ;; "info" is the same type as `evilmi-sdk-get-tag' returns
    info))

;;;###autoload
(defun evilmi-verilog-get-tag ()
  "Get tag at point."
  (let* ((info (evilmi-sdk-get-tag evilmi-verilog-match-tags
                                   evilmi-verilog-extract-keyword-howtos)))
    (if evilmi-debug (message "evilmi-verilog-get-tag called => %s" info))
    (or info (evilmi-verilog-parse-at-point))))

;;;###autoload
(defun evilmi-verilog-jump (info num)
  "Use INFO returned by `evilmi-verilog-get-tag' and NUM to jump to matched tag."
  (when info
    (let* ((orig-keyword (evilmi-sdk-keyword (cadr info))))
      (if evilmi-debug (message "evilmi-verilog-jump called => %s" info))
      (evilmi-sdk-jump info
                       num
                       evilmi-verilog-match-tags
                       evilmi-verilog-extract-keyword-howtos))))

(provide 'evil-matchit-verilog)
;;; evil-matchit-verilog.el ends here
