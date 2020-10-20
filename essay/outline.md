# Essay outline

## Background
- Editors
    - The early editors (ED?)
    - First modeless editor
    - Emacs and vim (multimodal editors)
- Accessibility
    - Disabilities (e.g Carpal tunnel)
    - WCAG
    - Assistive technology
        - Eye-tracker
        - Text-to-speech
        - Screen readers
- Voice programming
    - History
        - Dragon naturally speaking
        - Natlink
        - dragonfly
        - That one talk on youtube
    - Talon
    - Other solutions? (Caster, voicecoder, etc)

## Structural editing
- Definition
- Examples
    - Scratch
    - Paredit (Cursive, fireplace.vim, etc)
- Language theory (mostly parsing)

## Mouse free selection
- Tabbing around
- Vimium
- Better autocomplete menus

## Architecture
- The elm architecture
    - Can this be a significantly relevant part, or is this only interesting for my implementation?
- Client/server
    - Why is keyword emulation not enough?
    - How can this improve accessibility? (intellij, jupyter)

## Problem
Editing the structure of complex languages efficiently without the mouse.

## Solution
- Searching through an abstract syntax tree
- Action based architecture
- Lenses? (formal editing semantics)

## Literature
Really unsure about this point so far.

## Evaluation
How would i evaluate the results?
User tests are difficult due to the expected high learning curve.
Also the target demographic is relatively small.
If i were to do user tests, i would probably have to reach out to online communities (talon slack, caster chat).


Possibility for static analysis?
It would be interesting if i could formally prove some accessibility features.
This might work due to how elm works. Could be just using the type system, or peripheral tool.
Not sure if this is feasible though.


# Moved from thesis.tex


\chapter{Motivation}
\begin{itemize}
    \item{Injuries}
    \item{Try to find statistics}
\end{itemize}

\chapter{Background}

\section{Assistive Technologies}

\begin{itemize}
    \item{What are assistive technologies?}
    \item{Who uses them?}
\end{itemize}

\subsection{Eye Trackers}
\begin{itemize}
    \item{tobii}
    \item{issues (instability, neck strain)}
\end{itemize}

\subsection{Screen Readers}
Probably not a major part of this project, but should probably be considered.

\subsection{Speech Recognition}
\begin{itemize}
    \item{Signal processing}
    \item{Natural language processing}
\end{itemize}



\section{Programming By Voice}
\begin{itemize}
    \item{How is this achieved?}
    \item{How does programming differ from normal dictation?}
    \item{NLP vs command based}
    \item{Different solutions}
    \item{Programming with talon.}
\end{itemize}

\begin{center}
\begin{tabular}{|c|c|}
    Normal mode&a\\
    Visual mode&\\
    Select mode&\\
    Insert mode&\\
    Cmdline mode&\\
    Ex mode&\\
\end{tabular}
\end{center}


% \section{Code Editors}
% \begin{itemize}
%     \item{Difference between text editor and code editor}
%     \item{Plain and rich text formats}
%     \item{IDEs}
%     \item{Modal Editors}
% \end{itemize}
