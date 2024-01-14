;;; org-ref-bibtex.el -- org-ref-bibtex utilities

;; Copyright(C) 2014-2021 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 0.1
;; Keywords: org-mode, bibtex

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;; Functions to act on a bibtex entry or file
;;
;; ** Modifying bibtex entries
;; - `org-ref-sort-bibtex-entry' :: sort the fields in an entry
;; - `org-ref-downcase-bibtex-entry' :: downcase the fields in an entry
;; - `org-ref-clean-bibtex-entry' :: run hooks in `org-ref-clean-bibtex-entry-hook' on an entry
;; - `org-ref-title-case-article' :: title case the title in an article or book
;; - `org-ref-sentence-case-article' :: sentence case the title in an article.
;; org-ref-bibtex-generate-longtitles
;; org-ref-bibtex-generate-shorttitles
;; org-ref-stringify-journal-name :: replace a journal name with a string in
;; `org-ref-bibtex-journal-abbreviations'
;; org-ref-set-journal-string :: in a bibtex entry run this to replace the
;; journal with a string
;;

;; * Navigating bibtex entries
;; `org-ref-bibtex-next-entry' :: bound to M-n
;; `org-ref-bibtex-previous-entry' :: bound to M-p

;;
;; ** Operate on whole bibtex file
;; - `org-ref-build-full-bibliography'
;; org-ref-replace-nonascii :: replace nonascii characters in a bibtex
;; entry.  Replacements are in `org-ref-nonascii-latex-replacements'.
;;
;; ** hydra menu for bibtex files
;; `org-ref-bibtex-hydra/body' gives a hydra menu to a lot of useful functions.
;; `org-ref-bibtex-new-entry/body' gives a hydra menu to add new bibtex entries.
;; `org-ref-bibtex-file/body' gives a hydra menu of actions for the bibtex file
;;
;;; Code

(require 'bibtex)
(require 'dash)
(require 'hydra)
(require 'message)
(require 's)
(require 'doi-utils)
(require 'avy)

(defvar bibtex-completion-bibliography)
(declare-function bibtex-completion-show-entry "bibtex-completion")
(declare-function org-ref-find-bibliography "org-ref-core")
(declare-function org-element-property "org-element")

;;* Custom variables
(defgroup org-ref-bibtex nil
  "Customization group for org-ref-bibtex."
  :group 'org-ref-bibtex)


(defcustom org-ref-shorten-authors nil
  "If non-nil show only last names in the completion selection buffer."
  :type 'boolean
  :group 'org-ref-bibtex)


(defcustom org-ref-bibtex-sort-order
  '(("article"  . ("author" "title" "journal" "volume" "number" "pages" "year" "doi" "url"))
    ("inproceedings" . ("author" "title" "booktitle" "year" "volume" "number" "pages" "doi" "url"))
    ("book" . ("author" "title" "year" "publisher" "url")))
  "A-list of bibtex entry fields and the order to sort an entry with.
\(entry-type . (list of fields). This is used in
`org-ref-sort-bibtex-entry'. Entry types not listed here will
have fields sorted alphabetically."
  :type '(alist :key-type (string) :value-type (repeat string))
  :group 'org-ref)


(defcustom orcb-%-replacement-string " \\\\%"
  "Replacement for a naked % sign in cleaning a BibTeX entry.
The replacement string should be escaped for use with
`replace-match'. Compare to the default value. Common choices
would be to omit the space or to replace the space with a ~ for a
non-breaking space."
  :type 'regexp
  :group 'org-ref)


(defcustom org-ref-clean-bibtex-key-function
  (lambda (key)
    (replace-regexp-in-string ":" "" key))
  "Function to modify a bibtex key.
The default behavior is to remove : from the key."
  :type 'function
  :group 'org-ref)


(defcustom org-ref-clean-bibtex-entry-hook
  '(org-ref-bibtex-format-url-if-doi
    orcb-key-comma
    org-ref-replace-nonascii
    orcb-&
    orcb-%
    org-ref-title-case-article
    orcb-clean-year
    orcb-key
    orcb-clean-doi
    orcb-clean-pages
    orcb-check-journal
    org-ref-sort-bibtex-entry
    orcb-fix-spacing
    orcb-download-pdf)
  "Hook that is run in `org-ref-clean-bibtex-entry'.
The functions should have no arguments, and
operate on the bibtex entry at point. You can assume point starts
at the beginning of the entry. These functions are wrapped in
`save-restriction' and `save-excursion' so you do not need to
save the point position.

Org ref contains some functions that are not included by default
such as `orcb-clean-nil' or `orcb-clean-nil-opinionated' that
users may be interested in adding themselves."
  :group 'org-ref
  :type 'hook)

;; see https://github.com/fxcoudert/tools/blob/master/doi2bib for more replacements
(defcustom org-ref-nonascii-latex-replacements
  '(("í" . "{\\\\'i}")
    ("æ" . "{\\\\ae}")
    ("ć" . "{\\\\'c}")
    ("é" . "{\\\\'e}")
    ("ä" . "{\\\\\"a}")
    ("è" . "{\\\\`e}")
    ("à" . "{\\\\`a}")
    ("á" . "{\\\\'a}")
    ("ø" . "{\\\\o}")
    ("ë" . "{\\\\\"e}")
    ("ü" . "{\\\\\"u}")
    ("ñ" . "{\\\\~n}")
    ("ņ" . "{\\\\c{n}}")
    ("ñ" . "{\\\\~n}")
    ("å" . "{\\\\aa}")
    ("ö" . "{\\\\\"o}")
    ("Á" . "{\\\\'A}")
    ("í" . "{\\\\'i}")
    ("ó" . "{\\\\'o}")
    ("ó" . "{\\\\'o}")
    ("ú" . "{\\\\'u}")
    ("ú" . "{\\\\'u}")
    ("ý" . "{\\\\'y}")
    ("š" . "{\\\\v{s}}")
    ("č" . "{\\\\v{c}}")
    ("ř" . "{\\\\v{r}}")
    ("š" . "{\\\\v{s}}")
    ("İ" . "{\\\\.I}")
    ("ğ" . "{\\\\u{g}}")
    ("δ" . "$\\\\delta$")
    ("ç" . "{\\\\c{c}}")
    ("ß" . "{\\\\ss}")
    ("≤" . "$\\\\le$")
    ("≥" . "$\\\\ge$")
    ("θ" . "$\\\\theta$")
    ("μ" . "$\\\\mu$")
    ("→" . "$\\\\rightarrow$")
    ("⇌" . "$\\\\leftrightharpoons$")
    ("×" . "$\\\\times$")
    ("°" . "$\\\\deg$")
    ("ş" . "{\\\\c{s}}")
    ("γ" . "$\\\\gamma$")
    ("ɣ" . "$\\\\gamma$")
    ("º" . "degC")
    ("η" . "$\\\\eta$")
    ("µ" . "$\\\\mu$")
    ("α" . "$\\\\alpha$")
    ("β" . "$\\\\beta$")
    ("ɛ" . "$\\\\epsilon$")
    ("Ⅵ" . "\\textrm{VI}")
    ("Ⅲ" . "\\textrm{III}")
    ("Ⅴ" . "\\textrm{V}")
    ("λ" . "$\\\\lambda$")
    ("π" . "$\\\\pi$")
    ("∞" . "$\\\\infty$")
    ("χ" . "$\\\\chi$")
    ("∼" . "\\\\textasciitilde{}")
    ("‑" . "\\\\textemdash{}")
    (" " . " ")
    ("…" . "...")
    ("•" . "\\\\textbullet ")
    ;; I think these are non-ascii spaces. there seems to be more than one.
    (" " . " ")
    (" " . " ")
    (" " . " ")
    ("–" . "-")
    ("−" . "-")
    ("–" . "-")
    ("—" . "-")
    ("‒" . "\\\\textemdash{}")
    ("‘" . "'")
    ("’" . "'")
    ("’" . "'")
    ("“" . "\"")
    ("’" . "'")
    ("”" . "\""))
  "Cons list of non-ascii characters and their LaTeX representations.
This may be deprecated. When `org-ref' started, non-ascii
characters were often problematic with bibtex, but in 2021, it is
not obvious that is still try."
  :type '(alist :key-type (string) :value-type (string))
  :group 'org-ref-bibtex)

(defcustom org-ref-bibtex-assoc-pdf-with-entry-move-function 'rename-file
  "Function to use when associating pdf files with bibtex entries.
The value should be either `rename-file' or `copy-file'. The former
will move and rename the original file. The latter will leave the
original file in place while creating a renamed copy in some directory."
  :type 'function
  :group 'org-ref-bibtex)

;;* Journal abbreviations
(defcustom org-ref-bibtex-journal-abbreviations
  '(("ACR" "Accounts of Chemical Research" "Acc. Chem. Res.")
    ("ACAT" "ACS Catalysis" "ACS Catal.")
    ("AM" "Acta Materialia" "Acta Mater.")
    ("AMM" "Acta Metallurgica et Materialia" "Acta Metall. Mater.")
    ("AEM" "Advanced Energy Materials" "Adv. Energy Mater.")
    ("AAMI" "ACS Applied Materials \\& Interfaces"
     "ACS Appl. Mater. Interfaces")
    ("AMiner" "American Mineralogist" "Am. Mineral.")
    ("AngC" "Angewandte Chemie-International Edition"
     "Angew. Chem. Int. Edit.")
    ("APLM" "APL Materials" "APL Mat.")
    ("ACBE" "Applied Catalysis B: Environmental" "Appl. Catal. B-Environ.")
    ("APL" "Applied Physics Letters" "Appl. Phys. Lett.")
    ("ASS" "Applied Surface Science" "Appl. Surf. Sci.")
    ("CL" "Catalysis Letters" "Catal. Lett.")
    ("CC" "Catalysis Communications" "Catal. Commun.")
    ("CST" "Catalysis Science & Technology" "Catal. Sci. Technol.")
    ("CT" "Catalysis Today" "Catal. Today")
    ("ChC" "Chemical Communications" "Chem. Commun.")
    ("CPL" "Chemical Physics Letters" "Chem. Phys. Lett")
    ("CR" "Chemical Reviews" "Chem. Rev.")
    ("CSR" "Chemical Society Reviews" "Chem. Soc. Rev.")
    ("CSR" "Chemical Society Reviews" "Chem. Soc. Rev.")
    ("CM" "Chemistry of Materials" "Chem. Mater.")
    ("CSA" "Colloids and Surfaces, A: Physicochemical and Engineering Aspects"
     "Colloids Surf., A")
    ("CF" "Combustion and Flame" "Combust. Flame")
    ("CPMS" "Computational Materials Science" "Comp. Mater. Sci.")
    ("CPC" "Computer Physics Communications" "Comput. Phys. Commun.")
    ("CSE" "Computing in Science \\& Engineering" "Comput. Sci. Eng.")
    ("CGD" "Crystal Growth \\& Design" "Cryst. Growth Des.")
    ("CEC" "CrystEngComm" "CrystEngComm")
    ("EA" "Electrochimica Acta" "Electrochim. Acta")
    ("ECST" "ECS Transactions" "ECS Trans.")
    ("EES" "Energy \\& Environmental Science" "Energy Environ. Sci.")
    ("HPR" "High Pressure Research" "High Pressure Res.")
    ("IC" "Inorganic Chemistry" "Inorg. Chem.")
    ("IECR" "Industrial \\& Engineering Chemistry Research"
     "Ind. Eng. Chem. Res.")
    ("JJAP" "Japanese Journal of Applied Physics" "Jpn. J. Appl. Phys.")
    ("JMatR" "Journal of  Materials Research" "J. Mater. Res.")
    ("JALC" "Journal of Alloys and Compounds" "J. Alloy Compd.")
    ("JAC" "Journal of Applied Crystallography" "J. Appl. Crystallogr.")
    ("JAE" "Journal of Applied Electrochemistry" "J. Appl. Electrochem.")
    ("JAP" "Journal of Applied Physics" "J. Appl. Phys.")
    ("JC" "Journal of Catalysis" "J. Catal.")
    ("JCP" "Journal of Chemical Physics" "J. Chem. Phys.")
    ("JCC" "Journal of Computational Chemistry" "J. Comput. Chem.")
    ("JCG" "Journal of Crystal Growth" "J. Crys. Growth")
    ("JMC" "Journal of Materials Chemistry" "J. Mater. Chem.")
    ("JMC" "Journal of Materials Chemistry" "J. Mater. Chem.")
    ("JMSL" "Journal of Materials Science Letters" "J. Mater. Sci. Lett.")
    ("JMS" "Journal of Membrane Science" "J. Memb. Sci.")
    ("JPE" "Journal of Phase Equilibria" "J. Phase Equilib.")
    ("JPCS" "Journal of Physics and Chemistry of Solids"
     "J. Phys. Chem. Solids")
    ("JPCM" "Journal of Physics: Condensed Matter"
     "J. Phys.: Condens. Matter")
    ("JPS" "Journal of Power Sources" "J. Power Sources")
    ("JSSC" "Journal of Solid State Chemistry" "J. Solid State Chem.")
    ("JACerS" "Journal of the American Ceramic Society" "J. Am. Ceram. Soc.")
    ("JACS" "Journal of the American Chemical Society" "J. Am. Chem. Soc.")
    ("JASIST" "Journal of the American Society for Information Science and Technology"
     "J. Am. Soc. Inf. Sci. Technol.")
    ("JES" "Journal of The Electrochemical Society" "J. Electrochem. Soc.")
    ("JEaC" "Journal of Electroanalytical Chemistry" "J. Electroanal. Chem.")
    ("JMS" "Journal of Membrane Science" "J. Memb. Sci.")
    ("JRS" "Journal of Raman Spectroscopy" "J. Raman Spectrosc.")
    ("JVST" "Journal of Vacuum Science \\& Technology A"
     "J. Vac. Sci. Technol. A")
    ("ML" "Materials Letters" "Mater. Lett.")
    ("MSE-BS" "Materials Science and Engineering B" "Mat. Sci. Eng. B-Solid")
    ("MOLSIM" "Molecular Simulation" "Mol. Sim.")
    ("Nature" "Nature" "Nature")
    ("NM" "Nature Materials" "Nat. Mater.")
    ("NC" "Nature Chemistry" "Nat. Chem.")
    ("PML" "Philosophical Magazine Letters" "Phil. Mag. Lett.")
    ("PMA" "Philosophical Magazine A" "Phil. Mag. A")
    ("PA" "Physica A: Statistical Mechanics and its Applications" "Physica A")
    ("PB" "Physica B-Condensed Matter" "Physica B")
    ("PCCP" "Physical Chemistry Chemical Physics" "Phys. Chem. Chem. Phys.")
    ("PSSB" "physica status solidi (b)" "Phys. Status Solidi B")
    ("PRA" "Physical Review A" "Phys. Rev. A")
    ("PRB" "Physical Review B" "Phys. Rev. B")
    ("PRL" "Physical Review Letters" "Phys. Rev. Lett.")
    ("PCM" "Physics and Chemistry of Minerals" "Phys. Chem. Miner.")
    ("PNAS" "Proceedings of the National Academy of Sciences of the United States of America"
     "Proc. Natl. Acad. Sci. U. S. A.")
    ("PSurfSci" "Progress in Surface Science" "Prog. Surf. Sci.")
    ("Science" "Science" "Science")
    ("SM" "Scripta Materialia" "Scr. Mater.")
    ("SABC" "Sensors and Actuators B: Chemical" "Sensor. Actuat. B-Chem.")
    ("SS" "Surface Science" "Surf. Sci.")
    ("EPJB" "The European Physical Journal B" "Eur. Phys. J. B")
    ("JPC" "The Journal of Physical Chemistry" "J. Phys. Chem.")
    ("JPCB" "The Journal of Physical Chemistry B" "J. Phys. Chem. B")
    ("JPCC" "The Journal of Physical Chemistry C" "J. Phys. Chem. C")
    ("JPCL" "The Journal of Physical Chemistry Letters"
     "J. Phys. Chem. Lett.")
    ("JCP" "The Journal of Chemical Physics" "J. Chem. Phys.")
    ("MSMSE" "Modelling and Simulation in Materials Science and Engineering"
     "Modell. Simul. Mater. Sci. Eng.")
    ("TSF" "Thin Solid Films" "Thin Solid Films")
    ("TC" "Topics in Catalysis" "Top. Catal.")
    ("WR" "Water Research" "Water Res."))
  "List of (string journal-full-name journal-abbreviation). Find
  new abbreviations at http://cassi.cas.org/search.jsp."
  :type '(list (repeat (list string string)))
  :group 'org-ref-bibtex)


(defcustom org-ref-title-case-types '(("article" . ("title"))
				      ("book" . ("booktitle")))

  "An a-list of bibtex entry types and fields that will be converted to
title-case by org-ref-title-case."
  :type '(repeat string)
  :group 'org-ref-bibtex)


(defcustom org-ref-bibtex-pdf-download-dir
  (cond
   ((stringp bibtex-completion-library-path)
    bibtex-completion-library-path)
   (t
    (car bibtex-completion-library-path)))
  "Default directory to look for downloaded pdfs.
Used in `org-ref-bibtex-assoc-pdf-with-entry' when looking for a
PDF to associate with an entry. Defaults to the first entry in
`bibtex-completion-library-path'."
  :group 'org-ref-bibtex
  :type 'directory)


;; * Modifying journal titles
;;;###autoload
(defun org-ref-bibtex-generate-longtitles ()
  "Generate longtitles.bib which are @string definitions.
The full journal names are in `org-ref-bibtex-journal-abbreviations'."
  (interactive)
  (with-temp-file "longtitles.bib"
    (dolist (row org-ref-bibtex-journal-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
                      (nth 0 row)
                      (nth 1 row))))))


;;;###autoload
(defun org-ref-bibtex-generate-shorttitles ()
  "Generate shorttitles.bib which are @string definitions.
The abbreviated journal names in `org-ref-bibtex-journal-abbreviations'."
  (interactive)
  (with-temp-file "shorttitles.bib"
    (dolist (row org-ref-bibtex-journal-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
                      (nth 0 row)
                      (nth 2 row))))))


;;;###autoload
(defun org-ref-stringify-journal-name (&optional key start end)
  "Replace journal name in a bibtex entry with a string.
The strings are defined in
`org-ref-bibtex-journal-abbreviations'.  The optional arguments KEY,
START and END allow you to use this with `bibtex-map-entries'"
  (interactive)
  (bibtex-beginning-of-entry)
  (when
      (string= "article"
               (downcase
                (cdr (assoc "=type=" (bibtex-parse-entry)))))
    (let* ((full-names (mapcar
                        (lambda (row)
                          (cons  (nth 1 row) (nth 0 row)))
                        org-ref-bibtex-journal-abbreviations))
           (abbrev-names (mapcar
                          (lambda (row)
                            (cons  (nth 2 row) (nth 0 row)))
                          org-ref-bibtex-journal-abbreviations))
           (journal (s-trim (bibtex-autokey-get-field "journal")))
           (bstring (or
                     (cdr (assoc journal full-names))
                     (cdr (assoc journal abbrev-names)))))
      (when bstring
        (bibtex-set-field "journal" bstring t)
        (bibtex-fill-entry)))))


;;;###autoload
(defun org-ref-set-journal-string (full-journal-name)
  "Set a bibtex journal name to the string that represents FULL-JOURNAL-NAME.
This is defined in `org-ref-bibtex-journal-abbreviations'."
  (interactive (list
                (completing-read
                 "Journal: "
                 (mapcar
                  (lambda (x)
                    (nth 1 x))
                  org-ref-bibtex-journal-abbreviations))))
  ;; construct data alist for the string lookup.
  (let ((alist (mapcar
                (lambda (x)
                  (cons (nth 1 x) (nth 0 x)))
                org-ref-bibtex-journal-abbreviations)))
    (bibtex-set-field "journal" (cdr (assoc full-journal-name alist)) t)
    (bibtex-fill-entry)
    (bibtex-clean-entry)))

;;* Non-ascii character replacement

;;;###autoload
(defun org-ref-replace-nonascii ()
  "Replace non-ascii characters with LaTeX representations in a
bibtex entry."
  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (goto-char (point-min))
    (dolist (char (mapcar (lambda (x)
			    (car x))
			  org-ref-nonascii-latex-replacements))
      (while (re-search-forward char nil t)
        (replace-match (cdr (assoc char org-ref-nonascii-latex-replacements))))
      (goto-char (point-min)))))


;;* Title case transformations
(defvar org-ref-lower-case-words
  '("a" "an" "on" "and" "for"
    "the" "of" "in")
  "List of words to keep lowercase when changing case in a title.")


;;;###autoload
(defun org-ref-title-case (&optional key start end)
  "Convert a bibtex entry title and booktitle to title-case.
Convert only if the entry type is a member of the list
`org-ref-title-case-types'. The arguments KEY, START and END are
optional, and are only there so you can use this function with
`bibtex-map-entries' to change all the title entries in articles and
books."
  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (let* ((entry-type (downcase (cdr (assoc "=type=" (bibtex-parse-entry)))))
	   (fields (cdr (assoc entry-type org-ref-title-case-types)))
	   ;; temporary variables
	   title words)
      (when fields
	(cl-loop for field in fields
		 when (bibtex-autokey-get-field field)
		 do
		 (setq title (bibtex-autokey-get-field field)
		       words (split-string title)
		       start 0)
		 (setq words (mapcar
			      (lambda (word)
				(cond
				 ;; words containing more than one . are probably
				 ;; abbreviations. We do not change those.
				 ((with-temp-buffer
				    (insert word)
				    (goto-char (point-min))
				    (> (count-matches "\\.") 1))
				  word)
				 ;; match words containing {} or \ which are probably
				 ;; LaTeX or protected words, ignore
				 ((string-match "\\$\\|{\\|}\\|(\\|)\\|\\\\" word)
				  word)
				 ;; these words should not be capitalized, unless they
				 ;; are the first word
				 ((-contains? org-ref-lower-case-words
					      (s-downcase word))
				  (s-downcase word))
				 ;; Words that are quoted
				 ((s-starts-with? "\"" word)
				  (concat "\"" (s-capitalize (substring word 1))))
				 (t
				  (s-capitalize word))))
			      words))

		 ;; Check if first word should be capitalized
		 (when (-contains? org-ref-lower-case-words (car words))
		   (setf (car words) (s-capitalize (car words))))

		 (setq title (mapconcat 'identity words " "))

		 ;; Capitalize letters after a dash
		 (while
		     (string-match "[a-zA-Z]-\\([a-z]\\)" title start)
		   (let ((char (substring title (match-beginning 1) (match-end 1))))
		     (setf (substring title (match-beginning 1) (match-end 1))
			   (format "%s" (upcase char)))
		     (setq start (match-end 1))))

		 ;; this is defined in doi-utils
		 (bibtex-set-field
		  field
		  title)
		 (bibtex-fill-entry))))))


;;;###autoload
(defun org-ref-title-case-article (&optional key start end)
  "Convert a bibtex entry article or book title to title-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles and books."
  (interactive)
  (let ((org-ref-title-case-types '(("article" . ("title")))))
    (org-ref-title-case)))


;;;###autoload
(defun org-ref-sentence-case-article (&optional key start end)
  "Convert a bibtex entry article title to sentence-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles."
  (interactive)
  (bibtex-beginning-of-entry)

  (let* ((title (bibtex-autokey-get-field "title"))
         (words (split-string title))
         (start 0))
    (when
        (string= "article"
		 (downcase
		  (cdr (assoc "=type="
			      (bibtex-parse-entry)))))
      (setq words (mapcar
                   (lambda (word)
                     (if
                         ;; match words containing {} or \ which are probably
                         ;; LaTeX or protected words
                         (string-match "\\$\\|{\\|}\\|\\\\" word)
                         word
                       (s-downcase word)))
                   words))

      ;; capitalize first word
      (setf (car words) (s-capitalize (car words)))

      ;; join the words
      (setq title (mapconcat 'identity words " "))

      ;; capitalize a word after a :, eg. a subtitle, and protect it
      (while
          (string-match "[a-z]:\\s-+\\([A-Z]\\)" title start)
        (let ((char (substring title (match-beginning 1) (match-end 1))))
          (setf (substring title (match-beginning 1) (match-end 1))
                (format "%s" (upcase char)))
          (setq start (match-end 1))))

      ;; this is defined in doi-utils
      (bibtex-set-field "title" title)

      ;; clean and refill entry so it looks nice
      (bibtex-clean-entry)
      (bibtex-fill-entry))))


;;* Navigation in bibtex file
;;;###autoload
(defun org-ref-bibtex-next-entry (&optional n)
  "Jump to the beginning of the next bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries
forward.  Negative numbers do nothing."
  (interactive "P")
  ;; Note if we start at the beginning of an entry, nothing
  ;; happens. We need to move forward a char, and call again.
  (when (= (point) (save-excursion
                     (bibtex-beginning-of-entry)))
    (forward-char)
    (org-ref-bibtex-next-entry))

  ;; search forward for an entry
  (when
      (re-search-forward bibtex-entry-head nil t (and (numberp n) n))
    ;; go to beginning of the entry
    (bibtex-beginning-of-entry)))


;;;###autoload
(defun org-ref-bibtex-previous-entry (&optional n)
  "Jump to beginning of the previous bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries back."
  (interactive "P")
  (bibtex-beginning-of-entry)
  (when
      (re-search-backward bibtex-entry-head nil t (and (numberp n) n))
    (bibtex-beginning-of-entry)))


;;;###autoload
(defun org-ref-bibtex-visible-entry ()
  "Jump to visible entry."
  (interactive)
  (avy-with avy-ve
    (avy-process
     (save-excursion
       (goto-char (window-start))
       (let ((positions ()))
	 (while (re-search-forward "^@.*{" (window-end) t)
	   (push (match-beginning 0) positions))
	 (reverse positions))))
    (avy--style-fn avy-style)))


;;;###autoload
(defun org-ref-bibtex-visible-field ()
  "Jump to visible field."
  (interactive)
  (avy-with avy-vf
    (avy-process
     (save-excursion
       (goto-char (window-start))
       (let ((positions ()))
	 (while (re-search-forward ".*=\\s-*." (window-end) t)
	   (push (match-end 0) positions))
	 (reverse positions))))
    (avy--style-fn avy-style)))


(defun org-ref-bibtex-mode-keys ()
  "Modify keymaps used by `bibtex-mode'."
  (local-set-key (kbd "M-n") 'org-ref-bibtex-next-entry)
  (local-set-key (kbd "M-p") 'org-ref-bibtex-previous-entry))

;; add to bibtex-mode-hook
(add-hook 'bibtex-mode-hook 'org-ref-bibtex-mode-keys)

;;* Functions to act on an entry with a doi

(defun org-ref-bibtex-entry-doi ()
  "Get doi from entry at point."
  (save-excursion
    (bibtex-beginning-of-entry)
    (when (not (looking-at bibtex-any-valid-entry-type))
      (error "This entry does not appear to be a valid type."))
    (let ((entry (bibtex-parse-entry t)))
      (when (null entry)
	(error "Unable to parse this bibtex entry."))
      (cdr (assoc "doi" entry)))))

;; function that ensures that the url field of a bibtex entry is the
;; properly-formatted hyperlink of the DOI. See
;; http://blog.crossref.org/2016/09/new-crossref-doi-display-guidelines.html
;; for more information.
;;;###autoload
(defun org-ref-bibtex-format-url-if-doi ()
  "Hook function to format url to follow the current DOI conventions."
  (interactive)
  ;; Don't overwrite an existing url field though.
  (unless (bibtex-autokey-get-field "url")
    (if (eq (org-ref-bibtex-entry-doi) "") nil
      (let ((front-url "https://doi.org/")
            (doi (org-ref-bibtex-entry-doi)))
	(bibtex-set-field "url"
                          (concat front-url doi))))))


;;;###autoload
(defun org-ref-bibtex-wos ()
  "Open bibtex entry in Web Of Science if there is a DOI."
  (interactive)
  (doi-utils-wos (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-wos-citing ()
  "Open citing articles for bibtex entry in Web Of Science if
there is a DOI."
  (interactive)
  (doi-utils-wos-citing (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-wos-related ()
  "Open related articles for bibtex entry in Web Of Science if
there is a DOI."
  (interactive)
  (doi-utils-wos-related (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-crossref ()
  "Open the bibtex entry in Crossref by its doi."
  (interactive)
  (doi-utils-crossref (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-google-scholar ()
  "Open the bibtex entry at point in google-scholar by its doi."
  (interactive)
  (let ((doi (org-ref-bibtex-entry-doi)))
    (doi-utils-google-scholar
     (if (string= "" doi)
	 (save-excursion
	   (bibtex-beginning-of-entry)
	   (cdr (assoc "title" (bibtex-parse-entry t))))
       doi))))


;;;###autoload
(defun org-ref-bibtex-pubmed ()
  "Open the bibtex entry at point in Pubmed by its doi."
  (interactive)
  (doi-utils-pubmed (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-pdf (&optional _)
  "Open the pdf for the bibtex entry at point.
Thin wrapper to get `org-ref-bibtex' to open pdf, because it
calls functions with a DOI argument."
  (interactive)
  (org-ref-open-bibtex-pdf))


(defun org-ref-bibtex-get-file-move-func (prefix)
  "Determine whether to use `rename-file' or `copy-file' for `org-ref-bibtex-assoc-pdf-with-entry'.
When called with a PREFIX argument,
`org-ref-bibtex-assoc-pdf-with-entry-move-function' switches to the
opposite function from that which is defined in
`org-ref-assoc-pdf-with-entry-move-function'."
  (message (format "%s" prefix))
  (if (eq prefix nil)
      org-ref-bibtex-assoc-pdf-with-entry-move-function
    (if (eq org-ref-bibtex-assoc-pdf-with-entry-move-function 'rename-file)
	'copy-file
      'rename-file)))


(defvar bibtex-completion-library-path)
(declare-function bibtex-completion-find-pdf-in-library "bibtex-completion")


;;;###autoload
(defun org-ref-bibtex-assoc-pdf-with-entry (&optional prefix)
  "Prompt for pdf associated with entry at point and rename it.
Check whether a pdf already exists in `bibtex-completion-library' with the
name '[bibtexkey].pdf'. If the file does not exist, rename it to
'[bibtexkey].pdf' using
`org-ref-bibtex-assoc-pdf-with-entry-move-function' and place it in
a directory. Optional PREFIX argument toggles between
`rename-file' and `copy-file'."
  (interactive "P")
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((file (read-file-name
		  "Select file associated with entry: "
		  org-ref-bibtex-pdf-download-dir))
	   (bibtex-expand-strings t)
           (entry (bibtex-parse-entry t))
           (key (cdr (assoc "=key=" entry)))
	   (file-move-func (org-ref-bibtex-get-file-move-func prefix))
	   pdf)
      (if (bibtex-completion-find-pdf-in-library key)
	  (message (format "A file named %s already exists" (bibtex-completion-find-pdf-in-library key)))
	(setq pdf (expand-file-name (concat key ".pdf") (cond
							 ((stringp bibtex-completion-library-path)
							  bibtex-completion-library-path)
							 ((and (listp bibtex-completion-library-path)
							       (= 1 (length bibtex-completion-library-path)))
							  (car bibtex-completion-library-path))
							 (t
							  (completing-read "Dir: " bibtex-completion-library-path)))))
	(funcall file-move-func file pdf)
	(message (format "Created file %s" pdf))))))


;;* Hydra menus
;;** Hydra menu for bibtex entries
;; hydra menu for actions on bibtex entries
(defhydra org-ref-bibtex-hydra (:color blue :hint nil)
  "Bibtex actions:
"
  ;; Open-like actions
  ("p" org-ref-open-bibtex-pdf "PDF" :column "Open")
  ("n" org-ref-open-bibtex-notes "Notes" :column "Open")
  ("b" org-ref-open-in-browser "URL" :column "Open")

  ;; edit/modify
  ("K" (lambda ()
         (interactive)
         (org-ref-set-bibtex-keywords
          (read-string "Keywords: "
                       (bibtex-autokey-get-field "keywords"))
          t))
   "Keywords" :column "Edit")
  ("a" org-ref-replace-nonascii "Replace nonascii" :column "Edit")
  ("s" org-ref-sort-bibtex-entry "Sort fields" :column "Edit")
  ("T" org-ref-title-case-article "Title case" :column "Edit")
  ("S" org-ref-sentence-case-article "Sentence case" :column "Edit")
  ("U" (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)) "Update entry" :column "Edit")
  ("u" doi-utils-update-field "Update field" :column "Edit" :color red)
  ("<backspace>" (cl--set-buffer-substring (line-beginning-position) (+ 1 (line-end-position)) "")
   "Delete line" :column "Edit" :color red)
  ("d" bibtex-kill-entry "Kill entry" :column "Edit")
  ("L" org-ref-clean-bibtex-entry "Clean entry" :column "Edit")
  ("A" org-ref-bibtex-assoc-pdf-with-entry "Add pdf" :column "Edit")
  ("r" (lambda ()
	 (interactive)
         (bibtex-beginning-of-entry)
         (bibtex-kill-entry)
         (find-file (completing-read
                     "Bibtex file: "
		     (append bibtex-completion-bibliography
			     (f-entries "." (lambda (f) (f-ext? f "bib"))))))
         (goto-char (point-max))
         (bibtex-yank)
         (save-buffer)
         (kill-buffer))
   "Refile entry" :column "Edit")

  ;; www
  ("P" org-ref-bibtex-pubmed "Pubmed" :column "WWW")
  ("w" org-ref-bibtex-wos "WOS" :column "WWW")
  ("c" org-ref-bibtex-wos-citing "WOS citing" :column "WWW")
  ("a" org-ref-bibtex-wos-related "WOS related" :column "WWW")
  ("R" org-ref-bibtex-crossref "Crossref" :column "WWW")
  ("g" org-ref-bibtex-google-scholar "Google Scholar" :column "WWW")
  ("e" org-ref-email-bibtex-entry "Email" :column "WWW")


  ;; Copy
  ("o" (lambda ()
	 (interactive)
	 (bibtex-copy-entry-as-kill)
	 (message "Use %s to paste the entry"
		  (substitute-command-keys (format "\\[bibtex-yank]"))))
   "Copy entry" :column "Copy")

  ("y" (save-excursion
	 (bibtex-beginning-of-entry)
	 (when (looking-at bibtex-entry-maybe-empty-head)
	   (kill-new (bibtex-key-in-head))))
   "Copy key" :column "Copy")

  ("f" (save-excursion
	 (bibtex-beginning-of-entry)
	 (kill-new (bibtex-completion-apa-format-reference
		    (cdr (assoc "=key=" (bibtex-parse-entry t))))))
   "Formatted entry" :column "Copy")

  ;; Navigation
  ("[" org-ref-bibtex-next-entry "Next entry" :column "Navigation" :color red)
  ("]" org-ref-bibtex-previous-entry "Previous entry" :column "Navigation" :color red)
  ("<down>" next-line "Next line" :column "Navigation" :color red)
  ("<up>" previous-line "Previous line" :column "Navigation" :color red)
  ("<next>" scroll-up-command "Scroll up" :column "Navigation" :color red)
  ("<prior>" scroll-down-command "Scroll down" :column "Navigation" :color red)
  ("v" org-ref-bibtex-visible-entry "Visible entry" :column "Navigation" :color red)
  ("V" org-ref-bibtex-visible-field "Visible field" :column "Navigation" :color red)


  ;; Miscellaneous
  ("F" org-ref-bibtex-file/body "File hydra" :column "Misc")
  ("N" org-ref-bibtex-new-entry/body "New entry" :column "Misc")
  ("q" nil))


(declare-function biblio-lookup "biblio")
(declare-function arxiv-add-bibtex-entry "org-ref-arxiv")
(declare-function doi-insert-bibtex "doi-utils")

;;** Hydra menu for new bibtex entries
;; A hydra for adding new bibtex entries.
(defhydra org-ref-bibtex-new-entry (:color blue)
  "New Bibtex entry:"
  ("d" doi-insert-bibtex "from DOI" :column "Automatic")
  ("c" crossref-add-bibtex-entry "from Crossref" :column "Automatic")
  ("a" arxiv-add-bibtex-entry "From Arxiv" :column "Automatic")
  ("b" biblio-lookup "From biblio" :column "Automatic")
  ;; Bibtex types
  ("ma" bibtex-Article "Article" :column "Manual")
  ("mb" bibtex-Book "Book" :column "Manual")
  ("mi" bibtex-InBook "In book" :column "Manual")
  ("ml" bibtex-Booklet "Booklet" :column "Manual")
  ("mP" bibtex-Proceedings "Proceedings" :column "Manual")
  ("mp" bibtex-InProceedings "In proceedings" :column "Manual")
  ("mm" bibtex-Misc "Misc." :column "Manual")
  ("mM" bibtex-Manual "Manual" :column "Manual")
  ("mT" bibtex-PhdThesis "PhD Thesis" :column "Manual")
  ("mt" bibtex-MastersThesis "MS Thesis" :column "Manual")
  ("mR" bibtex-TechReport "Report" :column "Manual")
  ("mu" bibtex-Unpublished "unpublished" :column "Manual")
  ("mc" bibtex-InCollection "Article in collection" :column "Manual")
  ("q" nil "quit"))


;;** Hydra menu of functions to act on a bibtex file.
(defhydra org-ref-bibtex-file (:color blue)
  "Bibtex file functions: "
  ("v" bibtex-validate "Validate entries")
  ("s" bibtex-sort-buffer "Sort entries")
  ("r" bibtex-reformat "Reformat entries")
  ("c" bibtex-count-entries "Count entries")
  ("p" org-ref-build-full-bibliography "PDF bibliography"))


;;* Email a bibtex entry
(declare-function bibtex-completion-find-pdf "bibtex-completion")

;;;###autoload
(defun org-ref-email-bibtex-entry ()
  "Email current bibtex entry at point and pdf if it exists."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((key (cdr (assoc "=key=" (bibtex-parse-entry t))))
	 (pdfs (bibtex-completion-find-pdf key)))

    (bibtex-copy-entry-as-kill)
    (compose-mail)
    (message-goto-body)
    (insert (pop bibtex-entry-kill-ring))
    (message-goto-subject)
    (insert key)
    (cl-loop for pdf in pdfs do (mml-attach-file pdf))
    (message-goto-to)
    ;; I am not sure why I have to put an empty string here, but it prevents a
    ;; bell error.
    ""))

;;* org-ref bibtex keywords
;; adapted from bibtex-utils.el
;; these are candidates for selecting keywords/tags
(defun org-ref-bibtex-keywords ()
  "Get keywords defined in current bibtex file.
These are in the keywords field, and are comma or semicolon separated."
  (save-excursion
    (goto-char (point-min))
    (let (keywords kstring)
      (while (re-search-forward "^\\s-*keywords.*{\\([^}]+\\)}" nil t)
        ;; TWS - remove newlines/multiple spaces:
        (setq kstring (replace-regexp-in-string
		       "[ \t\n]+" " "
		       (match-string 1)))
        (mapc
         (lambda (v)
           (add-to-list 'keywords v t))
         (split-string kstring "\\(,\\|;\\)[ \n]*\\|{\\|}" t)))
      keywords)))


;;;###autoload
(defun org-ref-set-bibtex-keywords (keywords &optional arg)
  "Add KEYWORDS to a bibtex entry.
If KEYWORDS is a list, it is converted to a comma-separated
string.  The KEYWORDS are added to the beginning of the
field.  Otherwise KEYWORDS should be a string of comma-separate
keywords.  Optional argument ARG prefix arg to replace keywords."
  (interactive
   (list
    (completing-read "Keyword: " (org-ref-bibtex-keywords))
    current-prefix-arg))
  (bibtex-set-field
   "keywords"
   (if arg
       ;; replace with arg
       (if (listp keywords)
           (mapconcat 'identity keywords ", ")
         keywords)
     ;; else concatenate
     (concat
      (if (listp keywords)
          (mapconcat 'identity keywords ", ")
        keywords)
      (when (not (string= "" (bibtex-autokey-get-field "keywords")))
        (concat ", "  (bibtex-autokey-get-field "keywords"))))))
  (when (buffer-file-name)
    (save-buffer)))


(defun org-ref-save-all-bibtex-buffers ()
  "Save all bibtex-buffers."
  (cl-loop for buffer in (buffer-list)
	   do
	   (with-current-buffer buffer
	     (when (and (buffer-file-name) (f-ext? (buffer-file-name) "bib"))
	       (save-buffer)))))


;; * Extract bibtex blocks from an org-file

;;;###autoload
(defun org-ref-extract-bibtex-blocks (bibfile)
  "Extract all bibtex blocks in buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u), which
will clobber the file."
  (interactive
   (list (read-file-name "Bibfile: " nil nil nil
			 (file-name-nondirectory
			  (concat (file-name-sans-extension
				   (buffer-file-name))
				  ".bib")))))

  (let ((contents ""))
    (when (and (file-exists-p bibfile)
	       (not current-prefix-arg))
      (setq contents (with-temp-buffer
		       (insert-file-contents bibfile)
		       (buffer-string))))

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#\\+BEGIN_SRC bibtex" nil t)
	(setq contents
	      (concat
	       contents
	       (org-element-property :value (org-element-at-point))))))

    (with-temp-file bibfile
      (insert contents))))


;;** create text citations from a bibtex entry
(declare-function bibtex-completion-apa-format-reference "bibtex-completion")
(declare-function bibtex-completion-get-key-bibtex "bibtex-completion")

(defun org-ref-bib-citation ()
  "From a bibtex entry, create and return a lightly formatted citation string."
  (bibtex-completion-apa-format-reference (list (bibtex-completion-get-key-bibtex))))


;;** Open pdf in bibtex entry
(declare-function bibtex-completion-open-pdf "bibtex-completion")
;;;###autoload
(defun org-ref-open-bibtex-pdf ()
  "Open pdf for a bibtex entry, if it exists."
  (interactive)
  (bibtex-completion-open-pdf (list (bibtex-completion-get-key-bibtex))))


;;** Open notes from bibtex entry
(declare-function bibtex-completion-edit-notes "bibtex-completion")

;;;###autoload
(defun org-ref-open-bibtex-notes ()
  "From a bibtex entry, open the notes if they exist."
  (interactive)
  (bibtex-completion-edit-notes (list (bibtex-completion-get-key-bibtex))))


;;** Open bibtex entry in browser
(declare-function bibtex-completion-open-url-or-doi "bibtex-completion")

;;;###autoload
(defun org-ref-open-in-browser ()
  "Open the bibtex entry at point in a browser using the url field or doi field."
  (interactive)
  (bibtex-completion-open-url-or-doi (list (bibtex-completion-get-key-bibtex))))


;;** Build a pdf of the bibtex file
;;;###autoload
(defun org-ref-build-full-bibliography ()
  "Build pdf of all bibtex entries, and open it."
  (interactive)
  (let* ((bibfile (file-name-nondirectory (buffer-file-name)))
         (bib-base (file-name-sans-extension bibfile))
         (texfile (concat bib-base ".tex"))
         (pdffile (concat bib-base ".pdf")))
    (find-file texfile)
    (erase-buffer)
    (insert (format "\\documentclass[12pt]{article}
\\usepackage[version=3]{mhchem}
\\usepackage{url}
\\usepackage[numbers]{natbib}
\\usepackage[colorlinks=true, linkcolor=blue, urlcolor=blue, pdfstartview=FitH]{hyperref}
\\usepackage{doi}
\\begin{document}
\\nocite{*}
\\bibliographystyle{unsrtnat}
\\bibliography{%s}
\\end{document}" bib-base))
    (save-buffer)
    (shell-command (concat "pdflatex " bib-base))
    (shell-command (concat "bibtex " bib-base))
    (shell-command (concat "pdflatex " bib-base))
    (shell-command (concat "pdflatex " bib-base))
    (kill-buffer texfile)
    (org-open-file pdffile)))


;;** Sort fields in a bibtex entry
;;;###autoload
(defun org-ref-sort-bibtex-entry ()
  "Sort fields of entry in standard order."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (entry-fields)
         (other-fields)
         (type (cdr (assoc "=type=" entry)))
         (key (cdr (assoc "=key=" entry)))
	 (field-order (cdr (assoc (if type (downcase type))
				  org-ref-bibtex-sort-order))))

    ;; these are the fields we want to order that are in this entry
    (setq entry-fields (mapcar (lambda (x) (car x)) entry))
    ;; we do not want to reenter these fields
    (setq entry-fields (remove "=key=" entry-fields))
    (setq entry-fields (remove "=type=" entry-fields))

    ;;these are the other fields in the entry, and we sort them alphabetically.
    (setq other-fields
	  (sort (-remove (lambda(x) (member x field-order)) entry-fields)
		'string<))

    (save-restriction
      (bibtex-kill-entry)
      (insert
       (concat "@" type "{" key ",\n"
	       (mapconcat
	        (lambda (field)
		  (when (member field entry-fields)
		    (format "%s = %s,"
			    field
			    (cdr (assoc field entry)))))
	        field-order "\n")
	       ;; now add the other fields
	       (mapconcat
	        (lambda (field)
		  (cl-loop for (f . v) in entry concat
			   (when (string= f field)
			     (format "%s = %s,\n" f v))))
	        (-uniq other-fields) "\n")
	       "\n}"))
      (bibtex-find-entry key)
      (bibtex-fill-entry)
      (bibtex-clean-entry))))

;; downcase entries
;;;###autoload
(defun org-ref-downcase-bibtex-entry ()
  "Downcase the entry type and fields."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (entry-fields)
         (type (downcase (cdr (assoc "=type=" entry))))
         (key (cdr (assoc "=key=" entry))))

    (setq entry-fields (mapcar (lambda (x) (car x)) entry))
    ;; we do not want to reenter these fields
    (setq entry-fields (remove "=key=" entry-fields))
    (setq entry-fields (remove "=type=" entry-fields))

    (bibtex-kill-entry)
    (insert
     (concat "@" (downcase type) "{" key ",\n"
	     (mapconcat
	      (lambda (field)
		(format "%s = %s,"
			(downcase field)
			(cdr (assoc field entry))))
	      entry-fields "\n")
	     "\n}\n\n"))
    (bibtex-find-entry key)
    (bibtex-fill-entry)
    (bibtex-clean-entry)))


;;** Clean a bibtex entry
;; These functions operate on a bibtex entry and "clean" it in some way.

(defun orcb-clean-nil (&optional arg)
  "Remove nil from some article fields.
The removal is conditional. Sometimes it is useful to have nil
around, e.g. for ASAP articles where the fields are not defined
yet but will be in the future.

With \\[universal-argument], run `bibtex-clean-entry' after."
  (interactive "P")
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (type (downcase (cdr (assoc "=type=" entry)))))
    (when (string= type "article")
      (cond
       ;; we have volume and pages but number is nil.
       ;; remove the number field.
       ((and (string= type "article")
	     (not (string= (cdr (assoc "volume" entry)) "{nil}"))
	     (not (string= (cdr (assoc "pages" entry)) "{nil}"))
	     (string= (cdr (assoc "number" entry)) "{nil}"))
	(bibtex-set-field "number" "")
	(if arg
            (bibtex-clean-entry)))))))


(defun orcb-clean-nil-opinionated ()
  "Remove nil from all article fields.

Note that by default, this will leave the entry empty, which may
then get deleted by `bibtex-clean-entry.' To disable this
behavior, remove opts-or-alts from `bibtex-entry-format'. This
will leave the empty entries so that you may fill them in later."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (type (downcase (cdr (assoc "=type=" entry)))))
    (when (string= type "article")
      (cl-loop for (field . text) in entry do
               (if (string= text "{nil}")
                   (bibtex-set-field field ""))))))


(defun orcb-clean-doi ()
  "Remove http://dx.doi.org/ or https://doi.org in the doi field."
  (let ((doi (bibtex-autokey-get-field "doi")))
    (when (or (string-match "^http://dx.doi.org/" doi)
	      (string-match "^https://doi.org/" doi))
      (setq doi (replace-match "" nil nil doi))
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "doi" t))))
      (bibtex-kill-field)
      (bibtex-make-field "doi")
      (backward-char)
      (insert doi))))


(defun orcb-clean-year (&optional new-year)
  "Fix years set to 0.
If optional NEW-YEAR set it to that, otherwise prompt for it."
  ;; asap articles often set year to 0, which messes up key
  ;; generation. fix that.
  (let ((year (bibtex-autokey-get-field "year")))
    (when (string= "0" year)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "year" t))))
      (bibtex-kill-field)
      (bibtex-make-field "year")
      (backward-char)
      (insert (or new-year (read-string "Enter year: "))))))


(defun orcb-clean-pages ()
  "Check for empty pages, and put eid in its place if it exists."
  (let ((pages (bibtex-autokey-get-field "pages"))
	(eid (bibtex-autokey-get-field "eid")))
    (when (and (not (string= "" eid))
	       (or (string= "" pages)))
      (bibtex-set-field "pages" eid))))


(defun orcb-& ()
  "Replace naked & with \& in a bibtex entry."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (while (re-search-forward " & " nil t)
      (replace-match " \\\\& "))))


(defun orcb-% ()
  "Replace naked % with % in a bibtex entry.
Except when it is already escaped or in a URL. The replacement
for the % is defined by `orcb-%-replacement-string'."
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (while (re-search-forward "\\([^\\]\\)%\\([^[:xdigit:]]\\)" nil t)
      (replace-match (concat "\\1"
                             orcb-%-replacement-string
                             "\\2")))))


(defun orcb-key-comma ()
  "Make sure there is a comma at the end of the first line."
  (bibtex-beginning-of-entry)
  (end-of-line)
  ;; some entries do not have a key or comma in first line. We check and add it,
  ;; if needed.
  (unless (string-match ", *$" (thing-at-point 'line))
    (end-of-line)
    (insert ",")))


(defun orcb-key (&optional allow-duplicate-keys)
  "Replace the key in the entry.
Prompts for replacement if the new key duplicates one already in
the file, unless ALLOW-DUPLICATE-KEYS is non-nil."
  (let ((key (funcall org-ref-clean-bibtex-key-function
		      (bibtex-generate-autokey))))
    ;; remove any \\ in the key
    (setq key (replace-regexp-in-string "\\\\" "" key))
    ;; first we delete the existing key
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
	(delete-region (match-beginning bibtex-key-in-head)
		       (match-end bibtex-key-in-head)))
    ;; check if the key is in the buffer
    (when (and (not allow-duplicate-keys)
               (save-excursion
                 (bibtex-search-entry key)))
      (save-excursion
	(bibtex-search-entry key)
	(bibtex-copy-entry-as-kill)
	(switch-to-buffer-other-window "*duplicate entry*")
	(bibtex-yank))
      (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))

    (insert key)
    (kill-new key)))


(defun orcb-check-journal ()
  "Check entry at point to see if journal exists in `org-ref-bibtex-journal-abbreviations'.
If not, issue a warning."
  (interactive)
  (when
      (string= "article"
               (downcase
                (cdr (assoc "=type=" (bibtex-parse-entry)))))
    (save-excursion
      (bibtex-beginning-of-entry)
      (let* ((entry (bibtex-parse-entry t))
             (journal (cdr (assoc "journal" entry))))
        (when (null journal)
          (warn "Unable to get journal for this entry."))
        (unless (member journal (-flatten org-ref-bibtex-journal-abbreviations))
          (message "Journal \"%s\" not found in org-ref-bibtex-journal-abbreviations." journal))))))


(defun orcb-fix-spacing ()
  "Delete whitespace and fix spacing between entries."
  (let (beg end)
    (save-excursion
      (save-restriction
    	(widen)
	(bibtex-beginning-of-entry)
	(setq beg (point))
	(bibtex-end-of-entry)
	(setq end (if (re-search-forward bibtex-any-entry-maybe-empty-head nil t)
		      (progn (beginning-of-line)
			     (point))
		    (point-max)))
	;; 1. delete whitespace
	(narrow-to-region beg end)
	(delete-trailing-whitespace)
	;; 2. delete consecutive empty lines
	(goto-char end)
	(while (re-search-backward "\n\n\n+" nil 'move)
	  (replace-match "\n\n"))
	;; 3. add one line between entries
	(goto-char end)
	(forward-line -1)
	(when (looking-at "[}][ \t]*\\|@Comment.+\\|%.+")
	  (end-of-line)
	  (newline))))))


(defun orcb-download-pdf ()
  "Try to get the pdf in an entry."
  ;; try to get pdf
  (when doi-utils-download-pdf
    (if doi-utils-async-download
	(doi-utils-async-download-pdf)
      (doi-utils-get-bibtex-entry-pdf))))


(defun orcb-clean-<>-tags ()
  "Try removing <tags> from the entry."
  (sgml-mode)
  (ignore-errors
    (while (sgml-skip-tag-forward 1)
      (sgml-skip-tag-backward 1)
      (sgml-delete-tag 1)))
  (bibtex-mode))


;;;###autoload
(defun org-ref-clean-bibtex-entry ()
  "Clean and replace the key in a bibtex entry.
See functions in `org-ref-clean-bibtex-entry-hook'."
  (interactive)
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      (bibtex-beginning-of-entry)
      ;; run hooks. each of these operates on the entry with no arguments.
      ;; this did not work like  i thought, it gives a symbolp error.
      ;; (run-hooks org-ref-clean-bibtex-entry-hook)
      (mapc (lambda (x)
	      (save-restriction
		(save-excursion
		  (funcall x))))
	    org-ref-clean-bibtex-entry-hook))))


;; * Missing functions in bibtex-completion
;; I couldn't find these, and they seem useful.

(defun org-ref-bibtex-get-entry (key)
  "Return a parsed bibtex entry."
  (save-window-excursion
    (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
      (bibtex-completion-show-entry (list key))
      (bibtex-beginning-of-entry)
      (bibtex-parse-entry))))


(defun org-ref-bibtex-get-entry-value (key field)
  "For the entry associated with KEY get the FIELD value."
  (cdr (assoc field (org-ref-bibtex-get-entry key))))


(defun org-ref-get-citation-year (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (org-ref-bibtex-get-entry-value key "year"))


;;* The end
(provide 'org-ref-bibtex)

;;; org-ref-bibtex.el ends here
