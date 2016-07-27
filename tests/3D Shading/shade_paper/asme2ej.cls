% ASME DOCUMENT CLASS - asme2e.cls
% 	for LaTeX version 2e - 2/14/01
%
% WARNING: This is an unauthorized class file. It has not been thoroughly
% tested so it probably still has bugs. I'll do my best to fix them when 
% found.
%
% ---------------------------------------------------------------------------
%   filename:      asme2e.cls
%   version 1.4, May 12, 2005. 
%        (1) Modified asme2e.cls to support pdflatex  
%        (2) Modified asme2e.cls to support multiple lines in conference name.
%             --- Matt Campbell mtycampbell@ucdavis.edu
%   version:       1.2
%   date:          6/29/01
%   ported by:     Kenneth Sprott
%   address:       Department of Mechanical Engineering
%		   University of California, Davis
%		   Davis, California 95616
%   e-mail:        kssprott@ucdavis.edu
%   description:   This is a new version of the asme2e class file v1.0.
%		   New features in this file include:
%		   1. Eliminating the pifont package from the file.
%		      It was not required and had caused some problems.
%		   2. Fixed the font size definitions.  It is now possible
%		      to use 10, 11, and 12pt font.  The old size definition
%		      was incorrect and the default 12pt was actually 10pt.
%		      The default is now 10pt.
%		   3. Add support for the new reference format required by
%		      ASME. This requires the use of Bibtex.
%                  4. Eliminate the subfigure code held over from the original 
%                     ASME style file. Usepackage subfigure instead.
%                  5. Add cleanhead option to eliminate the banner on the title
%                     page.
%                  6. Add cleanfoot option to eliminate the copyright on each 
%                     footer.
%   usage:         \documentclass[options]{asme2e}
% ---------------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{asme2e}[2001/2/14 v2.0 Standard LaTeX document class]

\RequirePackage{ifthen}
\RequirePackage{times}      % Loads the Times-Roman Fonts
\RequirePackage{mathptm}    % Loads the Times-Roman Math Fonts
\RequirePackage{ifpdf}      % Needed to pick between latex and pdflatex

\newboolean{@titlepage}
\newcommand\@ptsize{}
\newboolean{@cleanfoot}
\newboolean{@cleanhead}

% -------------------------------------------------------
%                   Options
% -------------------------------------------------------

\DeclareOption{twoside}{\@twosidetrue \@mparswitchtrue}
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\DeclareOption{final}{\setlength\overfullrule{0pt}}
\DeclareOption{titlepage}{\@titlepagetrue}
\DeclareOption{notitlepage}{\@titlepagefalse}
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{cleanfoot}{\@cleanfoottrue}
\DeclareOption{cleanhead}{\@cleanheadtrue}
\DeclareOption{10pt}{\renewcommand\@ptsize{0}} % Corresponds to 10pt font
\DeclareOption{11pt}{\renewcommand\@ptsize{1}} % Corresponds to 11pt font
\DeclareOption{12pt}{\renewcommand\@ptsize{2}} % Corresponds to 12pt font

\ExecuteOptions{oneside,onecolumn,10pt,final}
\ProcessOptions


\newcommand\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\newcommand\currenttime{\minute\time\hour\minute\divide\hour60
\the\hour:\multiply\hour60\advance\minute-\hour\the\minute}



% ------------------------------------------------------
%                Font and Size Definitions 
% ------------------------------------------------------

\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand{\baselinestretch}{1}

% These next conditional statements set up the font size definitions
% for either 10, 11, or 12 pt font.  The default is 10pt.

\ifthenelse{\@ptsize = 0}{
\typeout{Ten Point}
\renewcommand\normalsize{%
   \@setfontsize\normalsize\@xpt{12}%
   \abovedisplayskip 18\p@ \@plus2\p@ \@minus5\p@
   \abovedisplayshortskip \abovedisplayskip 
   \belowdisplayshortskip \abovedisplayskip 
   \belowdisplayskip \abovedisplayskip
   \let\@listi\@listI}
\normalsize
\newcommand\small{%
   \@setfontsize\small\@ixpt{11}%
   \abovedisplayskip 8.5\p@ \@plus3\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
   \belowdisplayskip \abovedisplayskip}
\newcommand\footnotesize{%
   \@setfontsize\footnotesize\@viiipt{10}
   \abovedisplayskip 6\p@ \@plus2\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus\p@
   \belowdisplayshortskip 3\p@ \@plus\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 3\p@ \@plus\p@ \@minus\p@
               \parsep \z@ 
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip}
\newcommand\scriptsize{\@setfontsize\scriptsize\@viipt{8}}
\newcommand\tiny{\@setfontsize\tiny\@vpt{6}}
\newcommand\large{\@setfontsize\large\@xiipt{14}}
\newcommand\Large{\@setfontsize\Large\@xivpt{18}}
\newcommand\LARGE{\@setfontsize\LARGE\@xviipt{22}}
\newcommand\huge{\@setfontsize\huge\@xxpt{25}}
\newcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}


\def\@listI{\leftmargin\leftmargini
            \parsep 4\p@ \@plus2\p@ \@minus\p@
            \topsep 8\p@ \@plus2\p@ \@minus4\p@
            \itemsep4\p@ \@plus2\p@ \@minus\p@}
\let\@listi\@listI
\@listi
\def\@listii {\leftmargin\leftmarginii
              \labelwidth\leftmarginii
              \advance\labelwidth-\labelsep
              \topsep    4\p@ \@plus2\p@ \@minus\p@
              \parsep    2\p@ \@plus\p@  \@minus\p@
              \itemsep   \parsep}
\def\@listiii{\leftmargin\leftmarginiii
              \labelwidth\leftmarginiii
              \advance\labelwidth-\labelsep
              \topsep    2\p@ \@plus\p@\@minus\p@
              \parsep    \z@
              \partopsep \p@ \@plus\z@ \@minus\p@
              \itemsep   \topsep}
\def\@listiv {\leftmargin\leftmarginiv
              \labelwidth\leftmarginiv
              \advance\labelwidth-\labelsep}
\def\@listv  {\leftmargin\leftmarginv
              \labelwidth\leftmarginv
              \advance\labelwidth-\labelsep}
\def\@listvi {\leftmargin\leftmarginvi
              \labelwidth\leftmarginvi
              \advance\labelwidth-\labelsep}
}{}

\ifthenelse{\@ptsize = 1}{
\typeout{Eleven Point}
\renewcommand\normalsize{%
   \@setfontsize\normalsize\@xipt{13.6}%
   \abovedisplayskip 19\p@ \@plus3\p@ \@minus6\p@
   \abovedisplayshortskip \abovedisplayskip
   \belowdisplayshortskip \abovedisplayskip
   \belowdisplayskip \abovedisplayskip
   \let\@listi\@listI}
\normalsize
\newcommand\small{%
   \@setfontsize\small\@xpt\@xiipt
   \abovedisplayskip 10\p@ \@plus2\p@ \@minus5\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 6\p@ \@plus2\p@ \@minus2\p@
               \parsep 3\p@ \@plus2\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\newcommand\footnotesize{%
   \@setfontsize\footnotesize\@ixpt{11}%
   \abovedisplayskip 8\p@ \@plus2\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus\p@
   \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 4\p@ \@plus2\p@ \@minus2\p@
               \parsep \z@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\newcommand\scriptsize{\@setfontsize\scriptsize\@viiipt{9.5}}
\newcommand\tiny{\@setfontsize\tiny\@vipt\@viipt}
\newcommand\large{\@setfontsize\large\@xiipt{14}}
\newcommand\Large{\@setfontsize\Large\@xivpt{18}}
\newcommand\LARGE{\@setfontsize\LARGE\@xviipt{22}}
\newcommand\huge{\@setfontsize\huge\@xxpt{25}}
\newcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}
\def\@listI{\leftmargin\leftmargini
            \parsep 4.5\p@ \@plus2\p@ \@minus\p@
            \topsep 9\p@   \@plus3\p@ \@minus5\p@
            \itemsep4.5\p@ \@plus2\p@ \@minus\p@}
\let\@listi\@listI
\@listi
\def\@listii {\leftmargin\leftmarginii
              \labelwidth\leftmarginii
              \advance\labelwidth-\labelsep
              \topsep    4.5\p@ \@plus2\p@ \@minus\p@
              \parsep    2\p@   \@plus\p@  \@minus\p@
              \itemsep   \parsep}
\def\@listiii{\leftmargin\leftmarginiii
              \labelwidth\leftmarginiii
              \advance\labelwidth-\labelsep
              \topsep    2\p@ \@plus\p@\@minus\p@
              \parsep    \z@
              \partopsep \p@ \@plus\z@ \@minus\p@
              \itemsep   \topsep}
\def\@listiv {\leftmargin\leftmarginiv
              \labelwidth\leftmarginiv
              \advance\labelwidth-\labelsep}
\def\@listv  {\leftmargin\leftmarginv
              \labelwidth\leftmarginv
              \advance\labelwidth-\labelsep}
\def\@listvi {\leftmargin\leftmarginvi
              \labelwidth\leftmarginvi
              \advance\labelwidth-\labelsep}
}{}

\ifthenelse{\@ptsize = 2}{
\typeout{Twelve Point}
\renewcommand\normalsize{%
   \@setfontsize\normalsize\@xiipt{14.5}%
   \abovedisplayskip 20\p@ \@plus3\p@ \@minus7\p@
   \abovedisplayshortskip \abovedisplayskip
   \belowdisplayshortskip \abovedisplayskip
   \belowdisplayskip \abovedisplayskip
   \let\@listi\@listI}
\normalsize
\newcommand\small{%
   \@setfontsize\small\@xipt{13.6}%
   \abovedisplayskip 11\p@ \@plus3\p@ \@minus6\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 6.5\p@ \@plus3.5\p@ \@minus3\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 9\p@ \@plus3\p@ \@minus5\p@
               \parsep 4.5\p@ \@plus2\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\newcommand\footnotesize{%
   \@setfontsize\footnotesize\@xpt\@xiipt
   \abovedisplayskip 10\p@ \@plus2\p@ \@minus5\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 6\p@ \@plus2\p@ \@minus2\p@
               \parsep 3\p@ \@plus2\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\newcommand\scriptsize{\@setfontsize\scriptsize\@viiipt{9.5}}
\newcommand\tiny{\@setfontsize\tiny\@vipt\@viipt}
\newcommand\large{\@setfontsize\large\@xivpt{18}}
\newcommand\Large{\@setfontsize\Large\@xviipt{22}}
\newcommand\LARGE{\@setfontsize\LARGE\@xxpt{25}}
\newcommand\huge{\@setfontsize\huge\@xxvpt{30}}
\let\Huge=\huge
\def\@listI{\leftmargin\leftmargini
            \parsep 5\p@  \@plus2.5\p@ \@minus\p@
            \topsep 10\p@ \@plus4\p@   \@minus6\p@
            \itemsep5\p@  \@plus2.5\p@ \@minus\p@}
\let\@listi\@listI
\@listi
\def\@listii {\leftmargin\leftmarginii
              \labelwidth\leftmarginii
              \advance\labelwidth-\labelsep
              \topsep    5\p@   \@plus2.5\p@ \@minus\p@
              \parsep    2.5\p@ \@plus\p@    \@minus\p@
              \itemsep   \parsep}
\def\@listiii{\leftmargin\leftmarginiii
              \labelwidth\leftmarginiii
              \advance\labelwidth-\labelsep
              \topsep    2.5\p@\@plus\p@\@minus\p@
              \parsep    \z@
              \partopsep \p@ \@plus\z@ \@minus\p@
              \itemsep   \topsep}
\def\@listiv {\leftmargin\leftmarginiv
              \labelwidth\leftmarginiv
              \advance\labelwidth-\labelsep}
\def\@listv  {\leftmargin\leftmarginv
              \labelwidth\leftmarginv
              \advance\labelwidth-\labelsep}
\def\@listvi {\leftmargin\leftmarginvi
              \labelwidth\leftmarginvi
              \advance\labelwidth-\labelsep}
}{}



% These are specific fonts used in the file.

\DeclareFixedFont{\ninsfb}{OT1}{phv}{b}{n}{9}
\DeclareFixedFont{\ninsf}{OT1}{phv}{m}{n}{9}
\DeclareFixedFont{\egtsf}{OT1}{phv}{m}{n}{8}
\DeclareFixedFont{\egtsfb}{OT1}{phv}{b}{n}{8}
\DeclareFixedFont{\sevsf}{OT1}{phv}{m}{n}{7}
\DeclareFixedFont{\bannerfnt}{OT1}{phv}{b}{n}{10}
\DeclareFixedFont{\pnumfnt}{OT1}{phv}{b}{n}{18}
\DeclareFixedFont{\twlsfb}{OT1}{phv}{b}{n}{12}
\DeclareFixedFont{\tensfb}{OT1}{phv}{b}{n}{10}
\DeclareFixedFont{\tensf}{OT1}{phv}{m}{n}{10}
\DeclareFixedFont{\phvtf}{OT1}{phv}{b}{n}{18}


\DeclareOldFontCommand{\rmbf}{\normalfont\rmfamily\bf}{\mathrm}
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch{\relax}{\mathcal}}
\DeclareRobustCommand*\mit{\@fontswitch{\relax}{\mathnormal}}

\DeclareFontFamily{OT1}{msbm}{}
\DeclareFontShape{OT1}{msbm}{m}{n}{<5> <6> <7> <8> <9> <10> gen * msbm }{}

\newcommand\bb{\fontfamily{msbm}\selectfont}
\newcommand\bi{\fontseries{bc}\fontshape{it}\selectfont}

% -----------------------------------------------------------
%                 Page Layout
% -----------------------------------------------------------

\setlength{\paperheight}{11in}
\setlength{\paperwidth}{8.5in}
%\setlength{\oddsidemargin}{48pt}
%\setlength{\evensidemargin}{48pt}
\setlength{\oddsidemargin}{0.925in}
\setlength{\evensidemargin}{0.925in}
% Added by Matt Campbell to make compatible with pdflatex 3/8/2005
\ifpdf%
   %\setlength{\topmargin}{1.3in}% use 1.3in top margin for pdflatex
   \setlength{\topmargin}{0.8in}% use 1.3in top margin for pdflatex
\else
   \setlength{\topmargin}{1.5in}% use 2.0in top marging for latex - same as original
   %\setlength{\topmargin}{2.0in}% use 2.0in top marging for latex - same as original
\fi
%\setlength{\topmargin}{2.0in}% original line 
% End pdflatex section
\setlength{\marginparwidth}{0pt}
\setlength{\marginparsep}{11pt}
\setlength{\marginparpush}{5pt}
%\setlength{\columnsep}{.375in}
\setlength{\columnsep}{.17in}
\setlength{\columnseprule}{0pt}
\setlength{\headheight}{10pt}
\setlength{\headsep}{18pt}
\setlength{\topskip}{10pt}
\setlength{\footskip}{24pt}
%\setlength{\textheight}{9.00in}
\setlength{\textheight}{10.00in}
\advance\textheight by -\headheight
\advance\textheight by -\headsep
\setlength{\textwidth}{6.85in}
%\setlength{\textwidth}{7.5in}
\setlength{\footnotesep}{6.65pt}
\setlength{\skip\footins}{9pt plus4pt minus 2pt}
\setlength{\floatsep}{18pt plus2pt minus2pt}
\setlength{\textfloatsep}{20pt plus4pt minus2pt}
\setlength{\intextsep}{24pt plus4pt minus2pt}
\setlength{\dblfloatsep}{12pt plus2pt minus2pt}
\setlength{\dbltextfloatsep}{20pt plus2pt minus4pt}
\setlength{\@fptop}{0pt plus 1fil}
\setlength{\@fpsep}{8pt plus 2fil}
\setlength{\@fpbot}{0pt plus 1fil}
\setlength{\@dblfptop}{0pt plus 1fil}
\setlength{\@dblfpsep}{8pt plus 2fil}
\setlength{\@dblfpbot}{0pt plus 1fil}
\setlength{\parskip}{0pt plus 1pt}
\setlength{\parindent}{.25in}
\setlength{\partopsep}{2pt plus 1pt minus 1pt}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty

\setlength{\leftmargini}{.25in}
\setlength{\leftmarginii}{22pt}
\setlength{\leftmarginiii}{18.7pt}
\setlength{\leftmarginiv}{17pt}
\setlength{\leftmarginv}{10pt}
\setlength{\leftmarginvi}{10pt}

% -------------------------------------------------------------------------
%                          Sections and Enumerate
% -------------------------------------------------------------------------

% Changes the enumerate environments

\setlength{\leftmargin}{\leftmargini}
\setlength{\labelsep}{5pt}
\setlength{\labelwidth}{\leftmargini}
\addtolength{\labelwidth}{-\labelsep}
\renewcommand{\@listI}{\leftmargin\leftmargini\parsep\z@
\topsep 6\p@ plus2\p@ minus1\p@\itemsep\z@}
\let\@listi\@listI
\@listi
\renewcommand{\@listii}{\setlength{\leftmargin}{\leftmarginii}
   \setlength{\labelwidth}{\leftmarginii}
   \addtolength{\labelwidth}{-\labelsep}
   \setlength{\topsep}{4\p@ plus2\p@ minus\p@}
   \setlength{\parsep}{\z@}
   \setlength{\itemsep}{\parsep}}
\renewcommand{\@listiii}{\setlength{\leftmargin}{\leftmarginiii}
    \setlength{\labelwidth}{\leftmarginiii}
    \addtolength{\labelwidth}{-\labelsep}
    \setlength{\topsep}{2\p@ plus\p@ minus\p@}
    \setlength{\parsep}{\z@}
    \setlength{\partopsep}{\p@ plus\z@ minus\p@}
    \setlength{\itemsep}{\parsep}}
\renewcommand{\@listiv}{\setlength{\leftmargin}{\leftmarginiv}
     \setlength{\labelwidth}{\leftmarginiv}
     \addtolength{\labelwidth}{-\labelsep}}
\renewcommand{\@listv}{\setlength{\leftmargin}{\leftmarginv}
     \setlength{\labelwidth}{\leftmarginv}
     \addtolength{\labelwidth}{-\labelsep}}
\renewcommand{\@listvi}{\setlength{\leftmargin}{\leftmarginvi}
     \setlength{\labelwidth}{\leftmarginvi}
     \addtolength{\labelwidth}{-\labelsep}}
\renewcommand\theenumi{\@arabic\c@enumi}
\renewcommand\theenumii{\@alph\c@enumii}
\renewcommand\theenumiii{\@roman\c@enumiii}
\renewcommand\theenumiv{\@Alph\c@enumiv}
\newcommand\labelenumi{\theenumi.}
\newcommand\labelenumii{(\theenumii)}
\newcommand\labelenumiii{\theenumiii.}
\newcommand\labelenumiv{\theenumiv.}


\newenvironment{verse}
               {\let\\=\@centercr
                \list{}{\itemsep      \z@
                        \itemindent   -1.5em%
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \advance\leftmargin 1.5em}%
                \item[]}
               {\endlist}
\newenvironment{quotation}
               {\list{}{\listparindent 1.5em%
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item[]}
               {\endlist}
\newenvironment{quote}
               {\list{}{\rightmargin\leftmargin}%
                \item[]}
               {\endlist}

\newenvironment{sequence}{      % Special enumerate environments for asme
    \renewcommand{\theenumi}{\alph{enumi}}
    \renewcommand{\labelenumi}{(\theenumi)}
    \begin{enumerate}}
    {\end{enumerate}}
\newenvironment{enumeroman}{
     \renewcommand{\theenumi}{\roman{enumi}}
     \renewcommand{\labelenumi}{(\theenumi)}
     \begin{enumerate}}
     {\end{enumerate}}
\newenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand*\descriptionlabel[1]{\hspace\labelsep
                                \normalfont\bfseries #1}

\renewcommand\theequation{\@arabic\c@equation}  % equation environment

\setlength\arraycolsep{1.5pt}  %Different from article class
\setlength\tabcolsep{6pt}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}

\renewcommand{\footnoterule}{    % This modifies the default footnote style
     \kern12\p@
     \hrule width.5in height.25pt
     \kern10\p@}

\long\def\@makefntext#1{\parindent 1em\noindent
    \hbox to1.8em{\hss$\m@th^{\@thefnmark}$}#1}

\def\@fnsymbol#1{\ifcase#1\or *\or \dagger\or \ddagger\or
  \Section\or \Paragraph\or \|\or **\or \dagger\dagger
   \or \ddagger\ddagger \else\@ctrerr\fi\relax}

% Title page definition

\newenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      \if@compatibility
        \setcounter{page}{0}
      \else
        \setcounter{page}{1}%
      \fi}%
    {\if@restonecol\twocolumn \else \newpage \fi
      \setcounter{page}{1}%
    }


% Section Definitions

\setcounter{secnumdepth}{3}
\newcounter {part}
\newcounter {section}
\newcounter {subsection}[section]
\newcounter {subsubsection}[subsection]
\newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph]
\renewcommand\thepart         {\@Roman\c@part}
\renewcommand\thesection      {\@arabic\c@section}
\renewcommand\thesubsection   {\thesection.\@arabic\c@subsection}
\renewcommand\thesubsubsection{\thesubsection .\@arabic\c@subsubsection}
\renewcommand\theparagraph    {\thesubsubsection.\@arabic\c@paragraph}
\renewcommand\thesubparagraph {\theparagraph.\@arabic\c@subparagraph}
\newcommand\part{\par
   \addvspace{4ex}%
   \@afterindentfalse
   \secdef\@part\@spart}

\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >\m@ne
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    {\parindent \z@ \raggedright
     \interlinepenalty \@M
     \normalfont
     \ifnum \c@secnumdepth >\m@ne
       \Large\bfseries \partname~\thepart
       \par\nobreak
     \fi
     \huge \bfseries #2%
     \markboth{}{}\par}%
    \nobreak
    \vskip 3ex
    \@afterheading}
\def\@spart#1{%
    {\parindent \z@ \raggedright
     \interlinepenalty \@M
     \normalfont
     \huge \bfseries #1\par}%
     \nobreak
     \vskip 3ex
     \@afterheading}
\def\@endpart{\vfil\newpage
              \if@twoside
                \hbox{}%
                \thispagestyle{empty}%
                \newpage
              \fi
              \if@tempswa
                \twocolumn
              \fi}

% This changes the section headings style

\newcommand{\section}{\@startsection {section}{1}{\z@}
                        {24pt plus3pt minus2pt}
                        {0.0001pt}
                        %{\tensfb}}
                        {\rmbf}}
\newcommand{\subsection}{\@startsection {subsection}{2}{\z@}
                           {18pt plus2pt minus1pt}
                           {0.0001pt}
                           %{\tensfb}}
                           {\rmbf}}
%\newcommand{\subsubsection}{\@startsection {subsubsection}{3}{\parindent}
\newcommand{\subsubsection}{\@startsection {subsubsection}{3}{\z@}
                           {18pt plus2pt minus1pt}
                           {0.0001pt}
                           %{-1em}
                           %{\tensfb}}
                           {\rmbf}}
\newcommand{\paragraph}{\@startsection {paragraph}{4}{\z@}
                          {12pt plus2pt minus1pt}
                          {-1em}
                          {\normalfont\normalsize\bfseries}}
\newcommand{\subparagraph}{\@startsection {subparagraph}{4}{\parindent}
                            {12pt plus2pt minus1pt}
                            {-1em}
                            {\normalfont\normalsize\bfseries}}

% -----------------------------------------------------------------------
%                         Table of Contents
% -----------------------------------------------------------------------

\newcommand\@pnumwidth{1.55em}
\newcommand\@tocrmarg{2.55em}
\newcommand\@dotsep{4.5}
\setcounter{tocdepth}{3}
\newcommand\tableofcontents{%
    \section*{\contentsname
        \@mkboth{%
           \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}%
    \@starttoc{toc}%
    }
\newcommand*\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty\@secpenalty
    \addvspace{2.25em \@plus\p@}%
    \begingroup
      \setlength\@tempdima{3em}%
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \large \bfseries #1\hfil \hb@xt@\@pnumwidth{\hss #2}}\par
       \nobreak
       \if@compatibility
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}
      \fi
    \endgroup
  \fi}
\newcommand*\l@section[2]{%
  \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \addvspace{1.0em \@plus\p@}%
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
    \endgroup
  \fi}
\newcommand*\l@subsection{\@dottedtocline{2}{1.5em}{2.3em}}
\newcommand*\l@subsubsection{\@dottedtocline{3}{3.8em}{3.2em}}
\newcommand*\l@paragraph{\@dottedtocline{4}{7.0em}{4.1em}}
\newcommand*\l@subparagraph{\@dottedtocline{5}{10em}{5em}}
\newcommand\listoffigures{%
    \section*{{List of Figures}
      \@mkboth{\MakeUppercase{List of Figures}}
              {\MakeUppercase{List of Figures}}}
    \@starttoc{lof}%
    }
\newcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand\listoftables{%
    \section*{{List of Tables}
      \@mkboth{%
          \MakeUppercase{List of Tables}}
         {\MakeUppercase{List of Tables}}}
    \@starttoc{lot}%
    }
\let\l@table\l@figure

% ------------------------------------------------------------------------
%                         Tables and Figures
% ------------------------------------------------------------------------
\setcounter{topnumber}{4}         % These change the float placement 
\renewcommand{\topfraction}{.9}
\setcounter{bottomnumber}{4}
\renewcommand{\bottomfraction}{.9}
\setcounter{totalnumber}{4}
\renewcommand{\textfraction}{.1}
\renewcommand{\floatpagefraction}{.901}
\setcounter{dbltopnumber}{4}
\renewcommand{\dbltopfraction}{.9}
\renewcommand{\dblfloatpagefraction}{.91}         

%\newcommand\figurename{Figure}
\newcommand\figurename{Fig.}
\newcommand\tablename{Table}
\newcommand\partname{Part}
\newcommand\appendixname{APPENDIX}
\newcommand\refname{References}
%\newcommand\refname{REFERENCES}
\newcommand\contentsname{CONTENTS}


\long\def\@makecaption#1#2{ % This remains the same from asme.sty
   \par\addvspace{6pt}{\tensf
   \setbox\@tempboxa\hbox{\egtsf#1.\quad#2}%
   \ifdim \wd\@tempboxa >\hsize
       {\egtsf#1.\quad\ignorespaces#2}\par
     \else
       \hbox to\hsize{\hfil\box\@tempboxa\hfil}%
   \fi}} % \vskip4pt\global\c@subfigure0}
% Take out subfigure usepackage subfigure to achieve same result
%\newcounter{subfigure}
%\long\def\subcaption#1{%
%\global\advance\c@subfigure1
%   \setbox\@tempboxa\hbox{{\egtsfb (\alph{subfigure})\hskip.5em#1}}%
%   \ifdim \wd\@tempboxa >\hsize
%       {{\sffamily\fonsize{8}{\baselineskip}
%       (\alph{subfigure})\hskip.5em\ignorespaces#1\par}}
%     \else\hbox to\hsize{\hfil\box\@tempboxa\hfil}\fi\par\addvspace{6pt}}
\newbox\tempbox
\newcommand{\figurebox}[2]{%
            \dimen99=#2
            \divide\dimen99 by2
            \centerline{\vbox{\hrule\hbox to #1{\vrule\hfill\vbox to #2{\hbox to
            #1{\hfill{\vrule height\dimen99 width0pt{\hfill}}\hfill}\vfill}
            \vrule}\hrule}}}
\newcounter{figure}
\renewcommand\thefigure{\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename~\thefigure}
\newenvironment{figure}
               {\@float{figure}}
               {\end@float}
\newenvironment{figure*}
               {\@dblfloat{figure}}
               {\end@dblfloat}
\newcounter{table}
\renewcommand\thetable{\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename~\thetable}
\newenvironment{table}{%
            \small
            \renewcommand{\arraystretch}{1.5}
            \@float{table}}
            {\end@float}
\newenvironment{table*}{%
            \small
            \renewcommand{\arraystretch}{1.5}
            \@dblfloat{table}}
            {\end@dblfloat}

\newcommand\x{@{\extracolsep{\hfill}}} % Command to control column space in
                                         % table environment.
% --------------------------------------------------------------------------
%                               Title Page
% --------------------------------------------------------------------------




\newcommand\@confshortname{}
\newcommand\@conffullname{}
\newcommand\@confdate{}
\newcommand\@confmonth{}
\newcommand\@confyear{}
\newcommand\@confcity{}
\newcommand\@confcountry{}
\newcommand\@papernum{}
\newcommand{\confshortname}[1]{\gdef\@confshortname{#1}}
\newcommand{\conffullname}[1]{\gdef\@conffullname{#1}}
\newcommand{\confdate}[1]{\gdef\@confdate{#1}}
\newcommand{\confmonth}[1]{\gdef\@confmonth{#1}}
\newcommand{\confyear}[1]{\gdef\@confyear{#1}}
\newcommand{\confcountry}[1]{\gdef\@confcountry{#1}}
\newcommand{\confcity}[1]{\gdef\@confcity{#1}}
\newcommand{\papernum}[1]{\gdef\@papernum{#1}}

\if@titlepage
  \newcommand{\maketitle}{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \null\vfil
  \vskip 60\p@
  \begin{center}%
    {\LARGE \@title \par}%
    \vskip 3em%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
      \vskip 1.5em%
    {\large \@date \par}%       % Set date in \large size.
  \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \let\thanks\relax\let\maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}}
\else
\newcommand{\maketitle}{\par
  \begingroup
    \renewcommand{\thefootnote}{\fnsymbol{footnote}}%
    \def\@makefnmark{\hbox to\z@{$\m@th^{\@thefnmark}$\hss}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
            \hbox to1.8em{\hss$\m@th^{\@thefnmark}$}##1}%
    \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
      \fi
    \else
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \thispagestyle{plain}\@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \let\thanks\relax
  \let\maketitle\relax\let\@maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}}
\fi
 
%\renewcommand\title[1]{\gdef\@title{\uppercase{#1}}}
\renewcommand\title[1]{\gdef\@title{#1}}

\if@cleanhead
\def\@maketitle{%
  \newpage
  \null\vspace*{-26pt}
   \vbox{\hbox to \textwidth{\begin{tabular}{@{}c@{\hskip3pc}}
   \hbox to 46pt{\vbox to 46pt{\vss\hsize46pt\vss}}\end{tabular}\hss
  \vbox{\hsize37pc\scriptsize\sf\vskip\baselineskip%

  \vskip.5in}
  \hskip1pc}}
  \vskip.25in
  \vskip .25pc{\large\twlsfb
               \begin{center}\leftskip.5in plus1fill\rightskip\leftskip
               \@title\par\end{center}}
  \vskip2pc{\begin{center}\@author\par\end{center}}\vskip12pt}

\else

\def\@maketitle{%
  \newpage
  \null\vspace*{-26pt}             % Took out ASMElogo provision
   \vbox{\hbox to \textwidth{\begin{tabular}{@{}c@{\hskip3pc}}
   \hbox to 46pt{\vbox to 46pt{\vss\hsize46pt\vss}}\end{tabular}\hss
  \vbox{\hsize37pc\scriptsize\sf\vskip\baselineskip%
  \bannerfnt\begin{flushright}% added flushright to properly wrap long conference names
  %\hfill Proceedings of \@confshortname\par
  %\hfill \@conffullname\par
  %\hfill \@confmonth\ \@confdate, \@confyear, \@confcity, \@confcountry

  %\vskip.5in
  \hfill {\pnumfnt\@papernum}\end{flushright}}
  \hskip1pc}}
  %\vskip.15in%.25in original value w/out flushright
  %\vskip .25pc{\large\twlsfb
  \vskip .5pc{\Large\phvtf
               \begin{center}\leftskip.5in plus1fill\rightskip\leftskip
               \@title\par\end{center}}
  \vskip2pc{\begin{center}\@author\par\end{center}}\vskip12pt}
\fi

\if@titlepage
  \newenvironment{abstract}{%
     \titlepage
     \null\vfil
     \begin{center}
        {\tensfb ABSTRACT}
        \@endparpenalty\@M
     \end{center}}
     {\par\vfil\null\endtitlepage}
\else
  \newenvironment{abstract}{%
     \if@twocolumn
        %\normalsize\par\addvspace{12pt}\noindent
        %{\tensfb ABSTRACT}\par
        \normalsize\noindent
        %{\tensfb Abstract}\\
        \noindent
     \else
        \begin{center}
          {{\tensfb ABSTRACT}\vspace{-.5em}\vspace{\z@}}
        \end{center}
        \quotation
     \fi}
     {\if@twocolumn\else\endquotation\fi}
\fi 

% Get the multiple author set

\newcount\@authcnt
\newcount\@tmpcnt\@tmpcnt\z@

\def\@author{%
 \ifnum\@authcnt>7
  \ifnum\@tmpcnt<\@authcnt
   \global\advance\@tmpcnt1
   \leavevmode\hbox{\hss\csname @auth\romannumeral\the\@tmpcnt\endcsname\hss}%
   \quad
   \let\next\@author
  \else
   \let\next\relax
  \fi
 \else
  \ifnum\@tmpcnt<\@authcnt
   \global\advance\@tmpcnt1
   \ifnum\@tmpcnt=\@authcnt
    \expandafter\expandafter\expandafter\au@car\csname @auseq\romannumeral\the%
\@authcnt\endcsname\@nil
    \leavevmode\hbox to \@au@car{\hss\csname @auth\romannumeral\the\@tmpcnt%
\endcsname\hss}
   \else
    \expandafter\expandafter\expandafter\au@car\csname @auseq\romannumeral%
    \the\@authcnt\endcsname\@nil
    \leavevmode\hbox to \@au@car{\hss\csname @auth\romannumeral\the\@tmpcnt%
    \endcsname\hss}
   \fi
   \let\next\@author
  \else
   \let\next\relax
  \fi
 \fi
 \next}


\newcommand{\affiliation}[1]{\\ \tensf #1}

\def\@auseqi{{7.4in}}
\def\@auseqii{{3.45in}{3.45in}}
\def\@auseqiii{{2.4in}{2.4in}{2.4in}}
\def\@auseqiv{{3.45in}{3.45in}{3.45in}{3.45in}}
\def\@auseqv{{2.4in}{2.4in}{2.4in}{3.45in}{3.45in}}
\def\@auseqvi{{2.4in}{2.4in}{2.4in}{2.4in}{2.4in}{2.4in}}
\def\@auseqvii{{3.45in}{3.45in}{2.4in}{2.4in}{2.4in}{3.45in}{3.45in}}
\def\au@car#1#2\@nil{\gdef\@au@car{#1}}
\def\au@cdr#1#2\@nil{\expandafter\gdef\csname @auseq\romannumeral\the\@authcnt%
\endcsname{#2}}

\renewcommand{\author}[1]{%
    \global\advance\@authcnt1
    \expandafter\gdef\csname @auth\romannumeral\the\@authcnt\endcsname
    {\normalsize\sf\begin{tabular}[t]{@{}c@{}}\tensfb#1\\[20pt]
     \end{tabular}}}

% ---------------------------------------------------------------------------
%                           Bibliography 
% ---------------------------------------------------------------------------

\newcommand\appendix{\par
  \setcounter{section}{0}%
  \setcounter{subsection}{0}%
  \renewcommand\thesection{\@Alph\c@section}}

\newcommand\newblock{}
\newif\if@openbib
\@openbibfalse

% This generates the format for the bibliography enties. It is taken from
% the article class.

\newdimen\bibindent
\bibindent=1.5em
\newenvironment{thebibliography}[1]
     {\section*{\refname
        \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \if@openbib
              \advance\leftmargin\bibindent
              \itemindent -\bibindent
              \listparindent \itemindent
              \parsep \z@
            \fi
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \if@openbib
        \renewcommand\newblock{\par}%
      \else
        \renewcommand\newblock{\hskip .11em \@plus.33em \@minus.07em}%
      \fi
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.=\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

% This code was borrowed from the cite.sty package.
%---------------------------------------------------------------------------
%     C I T E . S T Y
%
%     version 3.8  (Apr 1999)
%
%     Compressed, sorted lists of numerical citations: [11-16]
%     see also OVERCITE.STY and DRFTCITE.STY
%
%     Copyright (C) 1989-1999 by Donald Arseneau
%     These macros may be freely transmitted, reproduced, or modified
%     provided that this notice is left intact.
%
%     Instructions follow \endinput.
%  ------------------------------------
% First, ensure that some catcodes have the expected values


\edef\citenum{% to restore funny codes
  \catcode\string`\string ` \the\catcode\string`\`
  \catcode\string`\string ' \the\catcode\string`\'
  \catcode\string`\string = \the\catcode\string`\=
  \catcode\string`\string _ \the\catcode\string`\_
  \catcode\string`\string : \the\catcode\string`\:}
\catcode\string`\`12
\catcode`\'12
\catcode`\=12
\catcode`\_8
\catcode`\:12

%   Handle optional variations:  [verbose,nospace,space,nosort,noadjust],
%   \citeform,\citeleft,\citeright,\citemid,\citepunct,\citedash
%
%   Set defaults:

%   [ on the left.
\@ifundefined{citeleft}{\let\citeleft=[}{}

%   ] on the right:
\@ifundefined{citeright}{\let\citeright=]}{}

%   , (comma space) before note
\@ifundefined{citemid}{\def\citemid{,\penalty\@medpenalty\ }}{}

%   , (comma thin-space) between entries; [nospace] eliminates the space
\@ifundefined{citepunct}{
   \def\citepunct{,\penalty\@m\hskip.13emplus.1emminus.1em}%
 }{}

%   -- (endash) designating range of numbers:
% (using \hbox avoids easy \exhyphenpenalty breaks)
\@ifundefined{citedash}{\def\citedash{\hbox{--}\penalty\@m }}{}

%   Each number left as-is:
\@ifundefined{citeform}{\def\citeform{}}{}

%   Do not repeat warnings.  [verbose] reverses
\let\oc@verbo\relax

%----------------------
% \citen uses \@nocite to ignore spaces after commas, and write the aux file
% \citation. \citen then loops over the citation tags, using \@make@cite@list
% to make a sorted list of numbers.  Finally, \citen executes \@citelist to
% compress ranges of numbers and print the list. \citen can be used by itself
% to give citation numbers without the brackets and other formatting; e.g.,
% "See also ref.~\citen{junk}."
%
\edef\citen{\noexpand\protect \expandafter\noexpand\csname citen \endcsname}

\@namedef{citen }#1{%
\begingroup
\let\@safe@activesfalse\@empty
\@nocite{#1}% ignores spaces, writes to .aux file, returns #1 in \@no@sparg
\@tempcntb\m@ne    % \@tempcntb tracks highest number
\let\@h@ld\relax   % nothing held from list yet
\let\@citea\@empty % no punctuation preceding first
\let\@celt\delimiter % an unexpandable, but identifiable, token
\def\@cite@list{}% % empty list to start
\@for \@citeb:=\@no@sparg\do{\@make@cite@list}% make a sorted list of numbers
% After sorted citelist is made, execute it to compress citation ranges.
\@tempcnta\m@ne    % no previous number
\let\@celt\@compress@cite \@cite@list % output number list with compression
\@h@ld % output anything held over
\endgroup}

% For each citation, check if it is defined and if it is a number.
% if a number: insert it in the sorted \@cite@list
% otherwise: output it immediately.
%
\def\@make@cite@list{%
 \expandafter\let \expandafter\@B@citeB
          \csname b@\@citeb\@extra@b@citeb \endcsname
 \ifx\@B@citeB\relax % undefined: output ? and warning
    \@citea {\bf{?}}\let\@citea\citepunct \G@refundefinedtrue
    \@warning {Citation `\@citeb' on page \thepage\space undefined}%
    \oc@verbo \global\@namedef{b@\@citeb\@extra@b@citeb}{?}%
 \else %  defined               % remove previous line to repeat warnings
    \ifcat _\ifnum\z@<0\@B@citeB _\else A\fi % a positive number, put in list
       \@addto@cite@list
    \else % citation is not a number, output immediately
       \@citea \citeform{\@B@citeB}%
       \let\@citea\citepunct
 \fi\fi}

% Regular definition for adding entry to cite list, with sorting

\@ifundefined{@addto@cite@list}{% sort option either declared or defaulted
  \def\@addto@cite@list{\@tempcnta\@B@citeB \relax
     \ifnum \@tempcnta>\@tempcntb % new highest, add to end (efficiently)
        \edef\@cite@list{\@cite@list \@celt{\@B@citeB}}%
        \@tempcntb\@tempcnta
     \else % arbitrary number: insert appropriately
        \edef\@cite@list{\expandafter\@sort@celt \@cite@list \@gobble @}%
     \fi}
%
% \@sort@celt inserts number (\@tempcnta) into list of \@celt{num} (#1{#2})
% \@celt must not be expandable; list should end with two vanishing tokens.
%
  \def\@sort@celt#1#2{\ifx \@celt #1% parameters are \@celt {num}
       \ifnum #2<\@tempcnta % number goes later in list
          \@celt{#2}%
          \expandafter\expandafter\expandafter\@sort@celt % continue
       \else % number goes here
          \@celt{\number\@tempcnta}\@celt{#2}% stop comparing
    \fi\fi}
}% end sort option definitions
{}% nosort definition is done in options processing

% Check if each number follows previous and can be put in a range
%
\def\@compress@cite#1{%  % This is executed for each number
  \advance\@tempcnta\@ne % Now \@tempcnta is one more than the previous number
  \ifnum #1=\@tempcnta   % Number follows previous--hold on to it
     \ifx\@h@ld\relax    % first pair of successives
        \edef\@h@ld{\@citea \noexpand\citeform{#1}}%
     \else               % compressible list of successives
        \def\@h@ld{\citedash \citeform{#1}}%
     \fi
  \else   %  non-successor -- dump what's held and do this one
     \@h@ld \@citea \citeform{#1}\let\@h@ld\relax
  \fi \@tempcnta#1\let\@citea\citepunct
}

\edef\cite{\noexpand\protect\expandafter\noexpand\csname cite \endcsname}

\@namedef{cite }{\@ifnextchar [{\@tempswatrue\@citex}{\@tempswafalse\@citex[]}}

%  Make \@citex refer to \citen:
%
\def\@citex[#1]#2{\@cite{\citen{#2}}{#1}}%

%  Replacement for \@cite which defines the formatting normally done
%  around the citation list.  Minor changes can
%  be done by changing \citeleft, \citemid, and \citeright; but in other
%  cases it might be necessary to redefine the whole macro.
%
\def\@cite#1#2{\leavevmode \cite@adjust
  \citeleft{#1\if@tempswa \citemid #2\fi
  \spacefactor\@m % punctuation in note doesn't affect outside
  }\citeright}

%  Put a penalty before the citation, and adjust the spacing: if no space
%  already or if there is extra space due to some punctuation, then change
%  to one inter-word space.
%
\@ifundefined{cite@adjust}{\def\cite@adjust{%
  \@tempskipa\lastskip \edef\@tempa{\the\@tempskipa}\unskip
  \ifnum\lastpenalty=\z@ \penalty\@highpenalty \fi
  \ifx\@tempa\@zero@skip \spacefactor1001 \fi % if no space before, set flag
  \ifnum\spacefactor>\@m \ \else \hskip\@tempskipa \fi}}{}

\edef\@zero@skip{\the\z@skip}

% \nocite: This is changed to ignore *ALL* spaces and be robust.  The
% parameter list, with spaces removed, is `returned' in \@no@sparg, which
% is used by \citen.
%
\edef\nocite{\noexpand\protect\expandafter\noexpand\csname nocite \endcsname}

\@namedef{nocite }#1{\@bsphack \@nocite{#1}%
 \@for \@citeb:=\@no@sparg\do{\@ifundefined{b@\@citeb\@extra@b@citeb}%
    {\G@refundefinedtrue\@warning{Citation `\@citeb' undefined}%
    \oc@verbo \global\@namedef{b@\@citeb\@extra@b@citeb}{?}}{}}%
 \@esphack}

\def\@nocite#1{\xdef\@no@sparg{\@ignsp#1 \! }% remove *ALL* spaces
 \if@filesw \immediate\write\@auxout {\string\citation {\@no@sparg}}\fi}

% for ignoring *ALL* spaces in the input.  This presumes there are no
% \outer tokens and no \if-\fi constructs in the parameter.  Spaces inside
% braces are retained.
%
\def\@ignsp#1 {\ifx\!#1\@empty\else #1\expandafter\@ignsp\fi}

\let\nocitecount\relax  % in case \nocitecount was used for drftcite

\@ifundefined{@extra@b@citeb}{\def\@extra@b@citeb{}}{}
%  in case no fancy bib package (chapterbib) defines this

\@ifundefined{G@refundefinedtrue}{\let\G@refundefinedtrue\relax}{}

\citenum % execute restore-catcodes

% Aliases:
\let\citenum\citen
\let\citeonline\citen

% End of Cite.sty code.
%-------------------------------------------------------------------------


% ---------------------------------------------------------------------------
%                          Special ASME declarations
% --------------------------------------------------------------------------

% Nomenclature environment

\newbox\tempbox
\newenvironment{nomenclature}{%
   \newcommand\entry[2]{%
       \setbox\tempbox\hbox{##1.\quad}
       \hangindent\wd\tempbox\noindent{##1}\quad\ignorespaces##2\par}
       %\section*{NOMENCLATURE}}{\par\addvspace{12pt}}
       \section*{Nomenclature}}{\par\addvspace{12pt}}

% Acknowledgement environment

\newenvironment{acknowledgment}{%
   \section*{Achnowledgements}}{\par\addvspace{12pt}}

% The following are the new theorem definitions


\AtEndOfClass{%
  \def\@begintheorem#1#2{\trivlist \item[\hskip \labelsep%
  {\tensfb #1\ #2.}]\it}
  \def\@opargbegintheorem#1#2#3{\trivlist
        \item[\hskip \labelsep%
  {\tensfb #1\ #2\ (#3).}]\it}
  \def\newtheorem{\@ifstar{\@newstheorem}{\@newtheorem}}
  \def\@newtheorem#1{\@ifnextchar[{\@othm{#1}}{\@nthm{#1}}}
  \def\@newstheorem#1#2{\@namedef{#1*}
  {\@ifnextchar[{\@opargbeginstheorem{#2}}{\@beginstheorem{#2}}}%
  \@namedef{end#1*}{\@endtheorem}}
  \def\@beginstheorem#1{\trivlist \item[\hskip \labelsep%
  {\tensfb #1.}]\it}
  \def\@opargbeginstheorem#1[#2]{\trivlist\item[\hskip \labelsep%
  {\tensfb #1\ (#2).  }]\it}
%
  \def\proof{\@ifnextchar[{\@optargproof}{\@proof}}
  \def\@proof{\trivlist \item[\hskip \labelsep{\it Proof.}]}
  \def\@optargproof[#1]{\trivlist \item[\hskip \labelsep{\it #1.}]}
  \def\endproof{\endtrivlist}
  \def\qed{\vskip-\lastskip\vskip-\baselineskip\hbox to \hsize{\hfill$\Box$}} 
}


% -------------------------------------------------------------------------
%                 Header and Footer Definitions
% -------------------------------------------------------------------------

\if@twoside
  \def\ps@headings{%
      \let\@oddfoot\@empty\let\@evenfoot\@empty
      \def\@evenhead{\thepage\hfil\slshape\leftmark}%
      \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
      \let\@mkboth\markboth
    \def\sectionmark##1{%
      \markboth {\MakeUppercase{%
        \ifnum \c@secnumdepth >\z@
          \thesection\hskip 1em\relax
        \fi
        ##1}}{}}%
    \def\subsectionmark##1{%
      \markright {%
        \ifnum \c@secnumdepth >\@ne
          \thesubsection\quad
          \fi
          ##1}}}
\else
  \def\ps@headings{%
    \let\@oddfoot\@empty
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\markboth
    \def\sectionmark##1{%
      \markright {\MakeUppercase{%
        \ifnum \c@secnumdepth >\m@ne
          \thesection\quad
        \fi
        ##1}}}}
\fi

\if@cleanfoot
 \def\ps@myheadings{%
    \let\@mkboth\@gobbletwo
    \def\@oddfoot{\hfil\thepage\hfil}
    \def\@evenfoot{\hfil\thepage\hfil}
    \let\@oddhead\@empty\let\@evenhead\@empty
    \def\sectionmark##1{}
    \def\subsectionmark##1{}
    }
\def\ps@plain{\let\@mkboth\@gobbletwo
     \def\@oddhead{$\smash{\raise72pt\hbox%
                  {}}$\hfill}%
                  %{\scriptsize\today\qquad\currenttime}}$\hfill}%
     \def\@oddfoot{\reset@font\rm \hfil\thepage%
     \hfil}%
     \let\@evenhead\@oddhead\let\@evenfoot\@oddfoot}

\else
 \def\ps@myheadings{%
     \let\@mkboth\@gobbletwo
     %\def\@oddfoot{\hskip1.55in\hfil\thepage\hfil Copyright\ \ \copyright\ \@confyear%
    %~by ASME}%
     %\def\@evenfoot{\hskip1.55in\hfil\thepage\hfil Copyright\ \ \copyright\ \@confyear%
    %~by ASME}%

    \let\@oddhead\@empty
    \let\@evenhead\@empty
    \def\sectionmark##1{}
    \def\subsectionmark##1{}
    }
\def\ps@plain{\let\@mkboth\@gobbletwo
     \def\@oddhead{$\smash{\raise72pt\hbox%
                  {}}$\hfill}%
                  %{\scriptsize\today\qquad\currenttime}}$\hfill}%
     %\def\@oddfoot{\reset@font\rm\hskip1.55in\hfil\thepage%
     %\hfil Copyright\ \ \copyright\ \@confyear\ by ASME}%
     \let\@evenhead\@oddhead\let\@evenfoot\@oddfoot}

\fi

 
%\def\ps@myheadings{%
%    \let\@mkboth\@gobbletwo
%    \ifthenelse{\@cleanfoot = 1}{
%      \def\@oddfoot{\hskip1.55in\hfil\thepage}
%      \def\@evenfoot{\hskip1.55in\hfil\thepage}}
%      {\def\@oddfoot{\hskip1.55in\hfil\thepage\hfil Copyright\ \ \copyright\ \confyear%
%    ~by ASME}%
%      \def\@evenfoot{\hskip1.55in\hfil\thepage\hfil Copyright\ \ \copyright\ \confyear%
%    ~by ASME}%}
%
%\let\@oddhead\@empty
%\let\@evenhead\@empty
%%    \def\@evenhead{\hfil\sl\leftmark\hfil}%
%%    \def\@oddhead{\hfil\sl\rightmark\hfil}%
%    \def\sectionmark##1{}
%    \def\subsectionmark##1{}
%    }


\mark{{}{}}
%\def\ps@plain{\let\@mkboth\@gobbletwo
%     \def\@oddhead{$\smash{\raise72pt\hbox%
%                  {\scriptsize\today\qquad\currenttime}}$\hfill}%
%     \def\@oddfoot{\reset@font\rm\hskip1.55in\hfil\thepage%
%     \hfil Copyright\ \ \copyright\ \confyear\ by ASME}%
%     \let\@evenhead\@oddhead\let\@evenfoot\@oddfoot}

\newcount\minute\newcount\hour
\def\currenttime{\minute\time\hour\minute\divide\hour60
\the\hour:\multiply\hour60\advance\minute-\hour\the\minute}

\markboth{}{}
\ps@myheadings
\pagenumbering{arabic}
\if@twocolumn\twocolumn\sloppy\flushbottom\else\onecolumn\fi
\advance\hoffset-7pc
\advance\voffset-10pc
%
%
\endinput

% End of Class


