\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{puctesis}[2017/12/19 v1.3 PUC Tesis class]
\wlog{| Miguel Torres-Torriti <Apr 2007>: Modified from UdeM thesis |}

\setcounter{errorcontextlines}{5}

\PassOptionsToClass{oneside}{amsbook}
\newif\if@nopunc\@nopuncfalse
\DeclareOption{nopunc}{\@nopunctrue}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{amsbook}}
\def\pdftex@true{0}
\DeclareOption{pdftex}{
  \PassOptionsToPackage{pdftex}{graphicx}
  \def\pdftex@true{1}
}
\ProcessOptions\relax
\LoadClass{amsbook}

\def\@makefnmark{\hbox{\@textsuperscript{\normalfont\@thefnmark}}}

\if@twoside
\oddsidemargin 12.6mm
\evensidemargin 0mm
\else
\oddsidemargin 12.6mm
\evensidemargin\oddsidemargin
\fi

\textwidth 6in

\headsep=20pt
\topskip=10pt
\topmargin 10mm
\textheight 9in
\footskip=30pt
\headheight=7pt

\normalparindent=21\p@
\parindent=\normalparindent
\parskip=5pt

\RequirePackage{setspace}
\let\@normalsize=\normalsize
\def\baselinestretch{1.5}
\@currsize
\normalsize

\renewcommand\footnotesize{\Small
  \def\@listi{\topsep \z@ \parsep \z@ \itemsep \z@}}

\RequirePackage{graphicx}

\RequirePackage{hyperref}
\hypersetup{
    pdffitwindow=false,
    pdfstartview={FitH},
    pdfnewwindow=true,
    colorlinks=true,
    linkcolor=blue,
    citecolor=blue,
    filecolor=blue,
    urlcolor=blue
}

\RequirePackage{apacite}
\renewcommand\newblock{\hspace{.7ex}}

\renewcommand{\topfraction}{.90}
\renewcommand{\bottomfraction}{.90}
\renewcommand{\textfraction}{.10}
\renewcommand{\floatpagefraction}{.90}
\renewcommand{\dbltopfraction}{.90}
\renewcommand{\dblfloatpagefraction}{.90}

\newlength{\auxdist}

\newcount\TestCount
\def\AmS{{\the\textfont2 A}\kern-.1667em\lower.5ex\hbox
        {\the\textfont2 M}\kern-.125em{\the\textfont2 S}}
\def\AmSTeX{\AmS-\TeX}
\def\La{\TestCount=\the\fam \leavevmode L\raise.42ex
        \hbox{$\fam\TestCount\scriptstyle\kern-.3em A$}}
\def\LaTeX{\La\kern-.15em\TeX}
\def\AmSLaTeX{\AmS-\LaTeX}

\def\contentsname{INDICE GENERAL}    
\def\listfigurename{INDICE DE FIGURAS}
\def\listtablename{INDICE DE TABLAS}

\def\figurename{Figura}
\def\tablename{Tabla}

\def\proofname{Demostraci\'on}
\def\chaptername{}
\def\thechapter{\@arabic\c@chapter}
\def\appendix{\par%
  \c@chapter\z@ \c@section\z@%
  \def\chaptername{ANEXO}%
  \def\thechapter{\@Alph\c@chapter}
}


\numberwithin{section}{chapter}

\c@tocdepth=2
\def\@toclevel{0}
\def\l@subsection{\@tocline{2}{0pt}{2pc}{5pc}{}}
\def\@dotsep{4.5}
\def\@tocline#1#2#3#4#5#6#7{\relax
  \ifnum #1>\c@tocdepth
  \else
    \par \addpenalty\@secpenalty\addvspace{#2}%
    \begingroup \hyphenpenalty\@M
    \@ifempty{#4}{%
      \@tempdima\csname r@tocindent\number#1\endcsname\relax
    }{%
      \@tempdima#4\relax
    }%
    \parindent\z@ \leftskip#3\relax \advance\leftskip\@tempdima\relax
    \rightskip\@pnumwidth plus1em \parfillskip-\@pnumwidth
    #5\leavevmode\hskip-\@tempdima #6\relax
    \nobreak\leaders\hbox{$\m@th \mkern \@dotsep mu.\mkern \@dotsep mu$}\hfill%
    \hbox to\@pnumwidth{\@tocpagenum{#7}}\par
    \nobreak
    \endgroup
  \fi
}

\def\l@figure{\@tocline{0}{3pt plus2pt}{0pt}{2.3em}{}}
\let\l@table=\l@figure

\def\@chapter[#1]#2{\refstepcounter{chapter}
  \ifnum\c@secnumdepth<\z@ \let\@secnumber\@empty
  \else \let\@secnumber\thechapter \fi
  \typeout{\chaptername\space\@secnumber}
  \def\@toclevel{0}
  \ifx\chaptername\appendixname \@tocwriteb\tocappendix{chapter}{#1}
  \else \@tocwriteb\tocchapter{chapter}{#1}\fi
  \chaptermark{#1}
  \addtocontents{lof}{\protect\addvspace{10\p@}}
  \addtocontents{lot}{\protect\addvspace{10\p@}}
  \@makechapterhead{#2}\@afterheading}

\def\@schapter#1{\typeout{#1}
  \let\@secnumber\@empty
  \def\@toclevel{0}
  \ifx\chaptername\appendixname \@tocwriteb\tocappendix{chapter}{#1}
  \else \@tocwriteb\tocchapter{chapter}{#1}\fi
  \chaptermark{#1}
  \addtocontents{lof}{\protect\addvspace{10\p@}}
  \addtocontents{lot}{\protect\addvspace{10\p@}}
  \@makeschapterhead{#1}\@afterheading}

\def\NoChapterPageNumber{
  \def\chapter{\cleardoublepage \thispagestyle{onlypagenumber} \pagestyle{headings}
    \global\@topnum\z@
    \secdef\@chapter\@schapter}
}

\def\PageNumbersFootCentered{
  \def\chapter{\cleardoublepage \thispagestyle{romanFC} \pagestyle{romanFC}
    \global\@topnum\z@
    \secdef\@chapter\@schapter}
}

\def\chapter{\cleardoublepage \thispagestyle{headings} \pagestyle{headings}
  \global\@topnum\z@
  \secdef\@chapter\@schapter
}

\def\@makechapterhead#1{\global\topskip 0pc\relax
  \begingroup\normalfont\bfseries
      \rlap{\vbox to\z@{\vss
{       
              \mdseries\@normalsize
  \ifx \chaptername\empty
  \settowidth{\auxdist}{\bfseries
                                  \thechapter.\enspace}
  \addtolength{\auxdist}{-\textwidth}
  \setlength{\auxdist}{-\auxdist}
  {\noindent\bfseries
  \thechapter.\enspace\parbox[t]{\auxdist}{\uppercase{#1}}}
  \else
  \settowidth{\auxdist}{\bfseries
                                  \chaptername\space \thechapter.\enspace}
  \addtolength{\auxdist}{-\textwidth}  
  \setlength{\auxdist}{-\auxdist}
  {\noindent\bfseries
   \chaptername\space \thechapter.\enspace\parbox[t]{\auxdist}{\uppercase{#1}}}
  \fi
  \enspace}
}}
     \vskip 22pt
\par
\endgroup
  \skip@16\p@ \advance\skip@-\normalbaselineskip
  \vskip\skip@ }

\def\@makeschapterhead#1{\global\topskip 0pc\relax
  \begingroup\normalfont\bfseries
  \centering\uppercase{#1}
     \vskip 22pt
\par
\endgroup
\ifx#1\contentsname\skip@36\p@
        \else
\ifx#1\bibname\skip@36\p@
         \else
                 \skip@36\p@
        \fi
  \fi
\advance\skip@-\normalbaselineskip
  \vskip\skip@ 
}

\def\@sect#1#2#3#4#5#6[#7]#8{%
  \edef\@toclevel{\ifnum#2=\@m 0\else\number#2\fi}
  \ifnum #2>\c@secnumdepth \let\@secnumber\@empty
  \else \@xp\let\@xp\@secnumber\csname the#1\endcsname\fi
 \ifnum #2>\c@secnumdepth
   \let\@svsec\@empty
 \else
    \refstepcounter{#1}
    \edef\@svsec{\ifnum#2<\@m
       \@ifundefined{#1name}{}{
         \ignorespaces\csname #1name\endcsname\space}\fi
       \@nx\textup{
         \csname the#1\endcsname.}\enspace
    }
  \fi
  \@tempskipa #5\relax
  \ifdim \@tempskipa>\z@
    \begingroup #6\relax
    \@hangfrom{\hskip #3\relax\@svsec}{\interlinepenalty\@M #8\par}
    \endgroup
  \csname #1mark\endcsname{#7}
    \ifnum#2>\@m \else \@tocwrite{#1}{#8}\fi
  \else
  \def\@svsechd{#6\hskip #3\@svsec
    \@ifnotempty{#8}{\ignorespaces#8\unskip\if@nopunc\else\@addpunct.\fi}
    \ifnum#2>\@m\else\@tocwrite{#1}{#8}\fi
  }
  \fi
  \global\@nobreaktrue
  \@xsect{#5}}

\def\section{\@startsection{section}{1}
  \z@{.7\linespacing\@plus\linespacing}{.5\linespacing}
  {\normalfont\bfseries}}

\def\subsection{\@startsection{subsection}{2}
  \z@{.5\linespacing\@plus.5\linespacing}{.1\linespacing}
  {\normalfont\bfseries}}

\def\subsubsection{\@startsection{subsubsection}{3}
  \z@{.0\linespacing\@plus.2\linespacing}{.01\linespacing}
  {\normalfont\bfseries}}

\def\paragraph{\@startsection{paragraph}{4}
  \z@{-\fontdimen2\font}
  \normalfont}

\def\subparagraph{\@startsection{subparagraph}{5}
  \z@\z@{-\fontdimen2\font}
  \normalfont}

\def\@listI{\leftmargin\leftmargini
  \parsep\z@skip \topsep\z@skip \itemsep\z@skip}
\let\@listi\@listI
\@listi

\let\sectionmark\@gobble
\def\sectionrunhead#1#2#3{
  \@ifnotempty{#2}{{#1
#2.}\@ifnotempty{#3}{\hskip1em}}
  \def\@tempa{#3}
  \ifx\@empty\@tempa\else\uppercasenonmath\@tempa\@tempa\fi}
\def\ps@onlypagenumber{\ps@empty
  \def\@oddfoot{\normalsize\hfil\llap{\thepage}}
  \def\@evenfoot{\normalsize\rlap{\thepage}{}{}\hfil}}
\def\ps@headings{\ps@empty
  \def\@oddfoot{\normalsize\hfil\llap{\thepage}}
  \def\@evenfoot{\normalsize\rlap{\thepage}{}{}\hfil}
  \let\@mkboth\markboth
  }
\def\ps@myheadings{\let\@mkboth\@gobbletwo
  \def\@oddhead{\hfil\Small\rightmark\hbox{} }
  \def\@oddfoot{\rm\hfil\thepage\hbox{}}
  \let\@evenhead\@oddhead
  \let\@evenfoot\@oddfoot\def\sectionmark##1{}\def\subsectionmark##1{}
  \def\chaptermark##1{\markright {\ifnum \c@secnumdepth
      >\m@ne
      \thechapter. \ ##1\fi}}}

\def\clap#1{\hbox to 0pt{\hss#1\hss}}
\def\ps@romanFC{\ps@empty
  \def\@oddfoot{\normalsize\hfil\clap{\thepage}\hfil}
  \def\@evenfoot{\normalsize\hfil\clap{\thepage}\hfil}
  \let\@mkboth\markboth
  }

\def\draft{
  \def\ps@headings{
    \def\@oddfoot{\normalfont
      \footnotesize[Draft:\,\today---\hhmm\,]
      \hfil\normalsize\thepage}
    \def\@evenfoot{\normalfont\normalsize\thepage\hfil
      \footnotesize\hfil[Draft:\,\today---\hhmm\,]}        
    \def\@evenhead{\Small{\scshape\leftmark}\hfil}
    \def\@oddhead{\Small\hfil{\scshape\rightmark}}}
  \def\ps@myheadings{\let\@mkboth\@gobbletwo
    \def\@oddfoot {\null\normalfont\footnotesize
      \hfil[Draft:\,\today---\hhmm\,]\hfil}          
    \def\@evenfoot{\null\normalfont\footnotesize
      \hfil[Draft:\,\today---\hhmm\,]\hfil}        
    \def\@oddhead{\null\normalfont\normalsize\rm\rightmark \hfil\thepage}
    \def\@evenhead{\normalfont\normalsize\rm\thepage\hfil\leftmark\null}}
  \ps@headings
}

\advance\textheight by -\headheight
\advance\textheight by -\headsep
\advance\textheight by -\normalbaselineskip
\advance\textheight by \topskip

\advance\textheight by.5\normalbaselineskip
\divide\textheight by\normalbaselineskip
\multiply\textheight by\normalbaselineskip
\advance\topmargin by -.25 true in
\renewcommand{\labelenumi}{(\roman{enumi})}
\def\enumerate{\ifnum \@enumdepth >3 \@toodeep\else
      \advance\@enumdepth \@ne
      \edef\@enumctr{enum\romannumeral\the\@enumdepth}\list
      {\csname label\@enumctr\endcsname}{\usecounter
    {\@enumctr}\partopsep6\p@\def\makelabel##1{\hss\llap{\upshape##1}}}\fi}
\def\itemize{\ifnum\@itemdepth>3 \@toodeep
  \else \advance\@itemdepth\@ne
  \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
  \list{\csname\@itemitem\endcsname}%
  {\partopsep6\p@\def\makelabel##1{\hss\llap{\upshape##1}}}\fi}

\def\refname{BIBLIOGRAFIA}
\def\bibname{}      
\def\thebibliography#1{\vspace{1pc}
       \chapter*\refname
  \edef\@tempa{{\noexpand\@mkboth{\refname}{\refname}}}
  \@tempa
  \normalfont\labelsep .5em\relax
  \list{[\@arabic\c@enumi]\quad}{\settowidth\labelwidth{[#1]\quad}
  \itemsep 0.5\baselineskip
  \leftmargin\labelwidth \advance\leftmargin\labelsep
  \usecounter{enumi}}
  \sloppy 
  \sfcode`\.\@m}

\def\@bibitem#1{\item\if@filesw \immediate\write\@auxout
       {\string\bibcite{#1}{\the\c@enumi}}\smallskip\fi\ignorespaces}

\def\@maketitle{
\clearpage  \thispagestyle{empty}  
\begingroup
  \normalfont\normalsize
  \topskip0\p@  % 
  \advance\topskip-\headsep \advance\topskip-\headheight
  \vskip12\p@\@plus12\p@
  \vspace*{-20mm}
  \vtop{
       {
       \noindent
        \raisebox{0mm}[0em][0em]{
                         \rule[-\textheight]{.0pt}{\textheight}}\\[-\normalbaselineskip]
        \raisebox{0mm}[0em][0em]{
                         \rule[0pt]{\textwidth}{.0pt}}\\[-\normalbaselineskip]
        \hspace*{80pt}
        \vspace*{-4mm}
        \setlength{\auxdist}{\textheight}
        \addtolength{\auxdist}{19mm}
        \raisebox{4mm}[0em][0em]{
                         \rule[-\auxdist]{.5pt}{\auxdist}}\\[-0\normalbaselineskip]
        \hspace*{-7.5mm}
        \parbox{80pt}
        {\centering 
            \includegraphics[width=23mm]{formato-puc/logouc.pdf}\\[.2\normalbaselineskip]
        }
       \hspace{7.5mm}
       \hspace{20pt}
       \parbox{.75\textwidth}{
       \@university\\
       \@school
       \ifx\@empty\@department {}
       \else{\\\@department}\fi}\\
       \setlength{\auxdist}{\textwidth}
       \addtolength{\auxdist}{13mm}
       \addtolength{\auxdist}{15mm}
       \hspace*{-13mm}
       \rule[0pt]{\auxdist}{.5pt}
       }
     }
  \vskip105\p@\@plus24\p@
  \setlength{\auxdist}{\textwidth}
  \addtolength{\auxdist}{-120pt}
  \addtolength{\auxdist}{15mm}
  \vtop{
       {
       \hspace{100pt}
       \parbox[t]{\auxdist}{\centering \noindent
       \uppercasenonmath{\@title}
       \bfseries\Large
       \@title\@@par}
       }
     }
  \prevdepth\dimen@i
  \vskip65\p@
  \ifx\@empty\authors\else
    \vtop{\andify\authors
       {
       \hspace{100pt}
       \parbox[t]{\auxdist}{\centering \noindent
       {\bfseries\large\MakeUppercase\authors}\@@par}
       }
     }\relax
   \prevdepth\dimen@i
  \fi
  \vskip15\p@\@plus12\p@
  \prepresentedto
  \vtop{
      {
      \hspace{100pt}
      \parbox[t]{.75\textwidth}{
      \noindent
      Tesis presentada a la Direcci\'on de Investigaci\'on y Postgrado\\
      como parte de los requisitos para optar al grado de\\
      \ifx\@empty\@degree {.........}\else\@degree\fi}
      }
    }
  \vskip10\p@\@plus14\p@
  \prepresentedto
  \vtop{
      {
      \hspace{100pt}
      \parbox[t]{.75\textwidth}{
      \noindent
      Profesor Supervisor:\\
      {
      \ifx\@empty\@advisor {.........}\else{\uppercase\@advisor}\fi}}
      }
    }
  \vskip5\p@\@plus14\p@
  \vfill
  \hspace{100pt}
  \parbox[t]{.75\textwidth}{
  \noindent
  \rm Santiago de Chile, \ifx\@empty\@date {\MoYr}\else\@date\fi
  }
  \vskip5\p@\@plus14\p@
  \hspace{100pt}
  \parbox[t]{.75\textwidth}{
  \noindent
  \copyrightline}
\endgroup
\clearpage  \thispagestyle{empty}
\cleardoublepage  \thispagestyle{empty}  
\begingroup
  \normalfont\normalsize
  \topskip0\p@  
  \advance\topskip-\headsep \advance\topskip-\headheight
  \vskip12\p@\@plus12\p@
  \vspace*{-20mm}
  \vtop{
       {
       \noindent
        \raisebox{0mm}[0em][0em]{
                         \rule[-\textheight]{.0pt}{\textheight}}\\[-\normalbaselineskip]
        \raisebox{0mm}[0em][0em]{
                         \rule[0pt]{\textwidth}{.0pt}}\\[-\normalbaselineskip]
        \hspace*{80pt}
        \vspace*{-4mm}
        \setlength{\auxdist}{\textheight}
        \addtolength{\auxdist}{19mm}
        \raisebox{4mm}[0em][0em]{
                         \rule[-\auxdist]{.5pt}{\auxdist}}\\[-0\normalbaselineskip]
        \hspace*{-7.5mm}
        \parbox{80pt}
        {\centering
            \includegraphics[width=23mm]{formato-puc/logouc.pdf}\\[.2\normalbaselineskip]
        }
       \hspace{7.5mm}
       \hspace{20pt}
       \parbox{.75\textwidth}{
       \@university\\
       \@school
       \ifx\@empty\@department {}
       \else{\\\@department}\fi}\\
       \setlength{\auxdist}{\textwidth}
       \addtolength{\auxdist}{13mm}
       \addtolength{\auxdist}{15mm}
       \hspace*{-13mm}
       \rule[0pt]{\auxdist}{.5pt}
       }
     }
  \vskip105\p@\@plus24\p@
  
  \setlength{\auxdist}{\textwidth}
  \addtolength{\auxdist}{-120pt}
  \addtolength{\auxdist}{15mm}
  \vtop{
       {
       \hspace{100pt}
       \parbox[t]{\auxdist}{\centering \noindent
       \uppercasenonmath{\@title}
       \bfseries\Large
       \@title\@@par}
       }
     }
  \prevdepth\dimen@i
  \vskip65\p@
  \ifx\@empty\authors\else
    \vtop{\andify\authors
       {
       \hspace{100pt}
       \parbox[t]{\auxdist}{\centering \noindent
       {\bfseries\large\MakeUppercase\authors}\@@par}
       }
     }\relax
   \prevdepth\dimen@i
  \fi
  \vskip15\p@\@plus12\p@
  \prepresentedto
  \vtop{
      {
      \hspace{100pt}
      \parbox[t]{.75\textwidth}{
      \noindent
      Miembros de la comisi\'on:\\
      {
      \ifx\@empty\@advisor {.........}\else{\uppercase\@advisor}\fi\\
      \ifx\@empty\@committeememberA {.........}\else{\uppercase\@committeememberA}\fi\\
      \ifx\@empty\@committeememberB {.........}\else{\uppercase\@committeememberB}\fi\\
      \ifx\@empty\@committeememberC {.........}\else{\uppercase\@committeememberC}\fi}
      }}
    }
  \vfill
  \hspace{100pt}
  \parbox[t]{.75\textwidth}{
  \noindent
  \rm Santiago de Chile, \ifx\@empty\@date {\MoYr}\else\@date\fi
  }
  \vskip5\p@\@plus14\p@
  \hspace{100pt}
  \parbox[t]{.75\textwidth}{
  \noindent
  \copyrightline}
\endgroup
\clearpage  \thispagestyle{empty}
 \ifx \@empty\@dedication {}
 \else
 \cleardoublepage  \thispagestyle{empty}
 \begingroup
 ~\vspace*{.7\textheight}\\[-\normalbaselineskip]
 \mbox{}\hspace*{0.6\textwidth}
 \parbox[t]{0.4\textwidth}{\centering
 {\em \@dedication}}
 \endgroup
 \clearpage \thispagestyle{empty}
 \fi
}

\def\mdate#1{\def\@mdate{{#1}}}
\def\@mdate{}
\def\version#1{\def\@version{{#1}}}
\version{0}

\def\university#1{\def\@university{{\normalfont#1}}}
\university{PONTIFICIA UNIVERSIDAD CATOLICA DE CHILE}

\def\school#1{\def\@school{{\normalfont#1}}}
\school{FACULTAD DE MATEM\'ATICAS}

\def\facultyto#1{\def\@facultyto{{\normalfont\rm#1}}}
\facultyto{}

\def\department#1{\def\@department{{\normalfont\rm#1}}}
\def\@department{}

\def\faculty#1{\def\@faculty{{\normalfont\rm#1}}}
\def\@faculty{}

\def\advisor#1{\def\@advisor{{\normalfont\rm#1}}}
\def\@advisor{}

\def\committeememberA#1{\def\@committeememberA{{\normalfont\rm#1}}}
\def\@committeememberA{}

\def\committeememberB#1{\def\@committeememberB{{\normalfont\rm#1}}}
\def\@committeememberB{}

\def\committeememberC#1{\def\@committeememberC{{\normalfont\rm#1}}}
\def\@committeememberC{}

\def\date#1{\def\@date{{\normalfont\rm#1}}}
\def\@date{}

\def\copyrightname#1{\def\@copyrightname{{\scshape#1}}}
\def\@copyrightname{}

\def\copyrightyear#1{\def\@copyrightyear{{\normalfont\footnotesize#1}}}
\def\@copyrightyear{}

\def\copyrightline{
    {\footnotesize \raisebox{.2ex}{$\copyright$}
    \@copyrightyear,
    \ifx\@copyrightname\empty{..........}\else 
    {\@copyrightname}\fi
    }
}
\def\prepresentedto{\vskip1.75pc}
\def\degree#1{\def\@degree{{\normalfont\rm#1}}}
\def\@degree{}

\def\subject#1{\def\@subject{{\normalfont\rm#1}}}
\def\@subject{}

\def\shortauthors{}
\def\shorttitle{}

\def\dedication#1{\def\@dedication{#1}}
\def\@dedication{}

\def\maketitle{\par
  \@topnum\z@
  \ifx\@empty\shortauthors\let\shortauthors\shorttitle\fi
  \begingroup
  \@maketitle
  \endgroup
  \andify\shortauthors
  \thispagestyle{headings}
  \c@footnote\z@
  \def\do##1{\let##1\relax}%
  \do\maketitle \do\@maketitle
  \do\address \do\@xaddress
  \do\email \do\@xemail \do\curraddr \do\@xcurraddr
  \do\department \do\@department
  \do\faculty \do\@faculty
  \do\degree \do\@degree
  \do\subject \do\@subject
  \do\date \do\@date
  \do\copyrightyear \do\@copyrightyear
}

\newbox\checkbox
\setbox\checkbox=\hbox{{\ignorespaces\footnotesize\it
                \indent Direcci\'on actual}:\ }

\def\@setaddresses{\par
  \nobreak \begingroup \footnotesize
  \def\author##1{\nobreak\addvspace\bigskipamount}
  \def\\{\unskip, \ignorespaces}
  \interlinepenalty\@M
  \def\address##1##2{\begingroup
    \par\nobreak 
    \@ifnotempty{##1}{(\ignorespaces##1\unskip) }
    {\noindent \scshape\ignorespaces##2}\par\endgroup}
  \def\curraddr##1##2{\begingroup
    \@ifnotempty{##2}{\nobreak\indent{\itshape Direcci\'on actual}
      \@ifnotempty{##1}{, \ignorespaces##1\unskip}\/:\space
      ##2\par\endgroup}}
  \def\email##1##2{\begingroup
    \@ifnotempty{##2}{\nobreak\indent{\itshape E-mail}
      \@ifnotempty{##1}{, \ignorespaces##1\unskip}\/:\space
      \ttfamily##2\par\endgroup}}
  \addresses
  \endgroup
}

\def\@captionheadfont{\scshape}
\def\@captionfont{\normalfont\small}

\def\today{\number\day\space
    \ifcase\month
    \or Enero\or Febrero\or Marzo\or Abril\or Mayo\or Junio%
    \or Julio\or Agosto\or Septiembre\or Octubre\or Noviembre\or Diciembre%
    \fi%
    \space\number\year}

\def\Month{
    \ifcase\month
    \or Enero\or Febrero\or Marzo\or Abril\or Mayo\or Junio
    \or Julio\or Agosto\or Septiembre\or Octubre\or Noviembre\or Diciembre
    \fi}

\def\Year{\number\year}

\def\MoYr{
    \ifcase\month
    \or Enero\or Febrero\or Marzo\or Abril\or Mayo\or Junio
    \or Julio\or Agosto\or Septiembre\or Octubre\or Noviembre\or Diciembre
    \fi
    \space\number\year}

% \def\todayfr{\space\number\day\space
%              \ifcase\month
%              \or
%   janvier\or f\'evrier\or mars\or avril\or mai\or juin\or
%   juillet\or ao\^ut\or septembre\or octobre\or novembre\or d\'ecembre\fi
%   \space \number\year}

\def\MoYrfr{
    \ifcase\month\or
  janvier\or f\'evrier\or mars\or avril\or mai\or juin\or
  juillet\or ao\^ut\or septembre\or octobre\or novembre\or d\'ecembre
    \fi
    \space\number\year}

\def\hhmm{
    \count1=\time
    \count2=\count1
    \divide \count1 by 60
    \count3=\count1
    \multiply \count1 by 60
    \advance\count2 by -\count1
    \number\count3:\number\count2
   }

\begingroup
\lccode`\~=`\ 
\lowercase{\gdef\verbspaces{\obeyspaces
  \def~{\leavevmode\space}}}
\endgroup
 
\def\verbinput#1{
  \begingroup
  \def\do##1{\catcode`##1=12}
  \dospecials \catcode`\@=12 
  \verbspaces
  \def\par{\ifvmode\null\else\endgraf\fi}\obeylines
  \Small\tt
  \input #1\relax
  \endgroup}

\def\verbinputx[#1]#2{
  \begingroup
  \def\do##1{\catcode`##1=12}
  \dospecials \catcode`\@=12 
  \verbspaces
  \def\par{\ifvmode\null\else\endgraf\fi}\obeylines
  \Small\tt
  {#1
  \input #2\relax
  }
  \endgroup}

\def\enddocument{\@checkend{document}\par

 \clearpage\begingroup  
 \if@filesw \immediate\closeout\@mainaux 
 \def\newlabel{\@testdef r}
 \def\bibcite{\@testdef b}\@tempswafalse \makeatletter\input \jobname.aux
 \if@tempswa \@warning{Label(s) may have changed.  Rerun to get
 cross-references right}\fi\fi\endgroup\deadcycles\z@\@@end}

\ps@headings
\numberwithin{figure}{chapter}
\numberwithin{table}{chapter}
\numberwithin{equation}{chapter}

\endinput
