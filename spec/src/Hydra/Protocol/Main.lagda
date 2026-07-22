\documentclass[11pt, titlepage]{article}

\include{preamble}
\include{macros}

\title{\Large \textbf{Hydra HeadV2 Specification: Coordinated Head protocol}\\[2ex] DRAFT}
\author{
  Sebastian Nagel \texttt{sebastian.nagel@iohk.io} \and
  Sasha Bogicevic \texttt{sasha.bogicevic@iohk.io} \and
  Franco Testagrossa \texttt{franco.testagrossa@iohk.io} \and
  Daniel Firth \texttt{daniel.firth@iohk.io} \and
  Noon van der Silk \texttt{noon.vandersilk@iohk.io} \and
  Veronika Romashkina \texttt{veronika.romashkina@iohk.io} 
  % NOTE: add yourself
}
\begin{document}
\maketitle

\newpage

\tableofcontents

\begin{code}[hide]
module Hydra.Protocol.Main where
\end{code}

\input{Hydra/Protocol/Introduction}
\input{Hydra/Protocol/Overview}
\input{Hydra/Protocol/Preliminaries}
\input{Hydra/Protocol/Setup}
\input{Hydra/Protocol/OnChain}
\input{Hydra/Protocol/OffChain}
\input{Hydra/Protocol/Security}

\bibliographystyle{plain}
\bibliography{short}

\end{document}
