%%% TEMPLATE.cls.tpl --- (>>>COMMENT<<<)

%% Author: (>>>AUTHOR<<<)
%% Version: $Id: (>>>FILE<<<),v 0.0 (>>>VC_DATE<<<) (>>>LOGIN_NAME<<<) Exp $

%%% Commentary:

%% (>>>1<<<)

%% Put this file into your TEXINPUTS.  A typical document looks like:
%%   \documentclass[OPTIONS]{(>>>FILE_SANS<<<)}
%%   \begin{document}
%%   (>>>2<<<)
%%   \end{document}

%% OPTIONS defaults to (>>>3<<<)

%%% Code:

\NeedsTeXFormat{LaTeX2e}(>>>POINT<<<)[1995/12/01]
\def\@rcs@ $#1Date: #2 #3$$#4Revision: #5$ {
   \ProvidesPackage{(>>>FILE_SANS<<<)}[#2 v#5(>>>COMMENT<<<)]}
\@rcs@ $Date: (>>>VC_DATE<<<) $$Revision: 0.0 $

\DeclareOption{(>>>4<<<)}{%%%
  }

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ExecuteOptions{(>>>5<<<)}
\ProcessOptions
\LoadClass[a4paper]{article}



%%%%##########################################################################
%%%%  Main code
%%%%##########################################################################

(>>>6<<<)


\endinput

%%% Local Variables:
%%% TeX-auto-save: nil
%%% TeX-auto-parse-length: 99999
%%% End:
