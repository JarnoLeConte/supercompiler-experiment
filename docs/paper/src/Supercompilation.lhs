%
% The main focus of the project report and presentation should be the following:
% - Research question. Identify a clear and motivated question to answer.
% - Research contribution. Develop an implementation or new knowledge to answer the research question.
% - Research method. Discuss how you arrived at your answer.
% - Related work. Compare your answer with other contributions.
%

\documentclass[fleqn,runningheads]{llncs}

\usepackage[utf8]{inputenc}
\usepackage[usenames,dvipsnames]{xcolor}
\usepackage{graphicx}
\usepackage{calc}
\usepackage{ulem}\normalem
\usepackage{deduction}
\usepackage{fontenc}
\usepackage{amsmath,amssymb}% http://ctan.org/pkg/amssymb
\usepackage{pifont}% http://ctan.org/pkg/pifont
\usepackage{qtree}
\usepackage{hyperref}
\usepackage{float}




% First-class contracts
\pagestyle{headings}
\mainmatter
\title{Supercompilation Experiment}
\titlerunning{Supercompilation Experiment}
\author{Jarno Le Conté}
\authorrunning{J.~Le Conté}
\institute{Utrecht University}

%-------------------------------=  --------------------------------------------

%options ghci -fglasgow-exts

%include polycode.fmt
%include Supercompilation.fmt

\mathindent0pt
\hfuzz2.75pt





%%% SETTINGS

% \setlength\parindent{0pt}

%%% COMMANDS

\newcommand{\cmark}{\ding{51}}%
\newcommand{\xmark}{\ding{55}}%
\newcommand{\none}{$\epsilon$}

\newcommand{\entry}[6]%
{%
\noindent\textbf{{#1}}\\
\begin{minipage}[t]{0.17\textwidth}{#2}\end{minipage}%
\begin{minipage}[t]{0.55\textwidth}{#3}\end{minipage}%
\begin{minipage}[t]{0.3\textwidth}{#4}\end{minipage}%
\begin{minipage}[t]{0.08\textwidth}{#5}\end{minipage} \\
\scriptsize{#6}\normalsize \par
% \rule{1.2\textwidth}{0.2pt}
\
}

\newcommand{\compile}[2]%
{\noindent
\begin{minipage}[c]{0.45\textwidth}{#1}\end{minipage}%
\begin{minipage}[c]{0.1\textwidth}\red\end{minipage}%
\begin{minipage}[c]{0.45\textwidth}{#2}\end{minipage}%
}

\newcommand{\kw}[1]{\mbox{\texttt{#1}}}
\newcommand{\arr}{\rightarrow}
\newcommand{\red}{$\leadsto$ }

\newcommand{\LET}{\kw{let}\,}
\newcommand{\MLET}{\mkw{let}\,}
\newcommand{\LETREC}{\kw{letrec}\,}
\newcommand{\IN}{\,\kw{in}\,}
\newcommand{\MIN}{\mkw{in}\,}
\newcommand{\CASE}{\kw{case}\,}
\newcommand{\OF}{\,\kw{of}\,}

\newcommand{\state}[3]{$\langle${#1}$\mid${#2}$\mid${#3}$\rangle$}
\newcommand{\qstate}[4]{$\langle${#1}$\mid${#2}$\mid${#3}$\mid${#4}$\rangle$}
\newcommand{\nstate}[4]{\state{#1}{#2}{#3}$_{#4}$}
\newcommand{\stat}[1]{$\langle${#1}$\rangle$}
\newcommand{\nstat}[2]{\stat{#1}$_{#2}$}
\newcommand{\UpdateFrame}[2]{$\kw{update}^{#1}${#2}}
\newcommand{\AppFrame}[2]{$\bullet^{#1}${#2}}
\newcommand{\CaseFrame}[2]{\{$\overline{{#2}}$\}}
\newcommand{\terminal}[1]{$^{\ast}${#1}}
\newcommand{\memo}[2]{{#1}$_{#2}^{\diamond}$}

\newcommand{\evalrule}[3]{{#1} & {#2} & $\leadsto$ & {#3}}
\newcommand{\specialcell}[2][l]{%
  \begin{tabular}[#1]{@{}l@{}}#2\end{tabular}}

\newcommand{\err}[1]{\textcolor{red}{{#1}}}
\newcommand{\gray}[1]{\textcolor{gray}{{#1}}}
% \newcommand{\new}[1]{\textcolor{NavyBlue}{{#1}}}
% \newcommand{\unknow}[1]{\textcolor{red}{{#1}}}
% \newcommand{\verify}[1]{\textcolor{orange}{{#1}}}
% \newcommand{\unrelated}[1]{\textcolor{red}{{#1}}}
\newcommand{\new}[1]{{{#1}}}
\newcommand{\unknow}[1]{{{#1}}}
\newcommand{\verify}[1]{{{#1}}}
\newcommand{\unrelated}[1]{{{#1}}}


%%% END COMMANDS

\begin{document}

\maketitle

\begin{abstract}
Minimal reimplementation of Bolingbroke's supercompiler for call-by-need languages, making use of attribute grammars instead of monadic structures. Additionally we formalized some rules and explain the difficult parts on the basis of pictures.
\end{abstract}



\section{Introduction}
\label{sec:introduction}
% Supercompilation is a program transformation that combine several optimization strategies in order to optimise a program.

We implement a supercompiler from scratch for a call-by-need language following the algorithm provided by Bolingbroke \cite{bolingbroke-sc-evaluation}, but we only focus on a minimal subset that only contains the required parts to make the supercompiler work. We implement most parts using the UU Attribute Grammar System \cite{uuag}. Also we point out difficult parts we have encountered when looking at Bolingbroke's implementation. Furthermore we try to visualize the supercompilation process with the help of examples and figures.

We implement the supercompiler for an extended lambda calculus with data types and recursive let. Because Bolingbroke's algorithm uses Sestoft-style operational semantics as basis for evaluation, we will see that we stay close the the standard evaluation rules.

Important to note is that we not fully succeed making a compiler which compiles all programs correctly. But we give some examples of failing programs and try to point out which code is responsible for giving wrong results.

This paper have the following structure. In section \ref{sec:supercompilation} we first discuss the idea of supercompilation. In section \ref{sec:components} we give an overview of the implementation and discuss each component in detail. With this knowledge we will look in section \ref{sec:examples} at some examples, and visualize the steps that are performed by the supercompiler. At each example we point out the parts that are difficult to understand and we try to indicate the implementation errors. We conclude in section \ref{sec:conclusion}.

\pagebreak

\section{Contributions}
\label{sec:contribution}

The main goal of this project is to implement a supercompiler as described in the literature by Bolingbroke. The experience by doing this have lead to the following contributions.\\

\textbf{Main contributions}
\begin{itemize}
\item We encounter some difficulties implementing Bolingbroke's approach. Therefore we point out these parts and explain it in more detail, using figures to visualize the compilation process.
\item We focus on a minimal supercompiler, containing only the parts that are strictly necessary to correctly supercompile, thus leave out optimizations and not obtain work sharing, making the final supercompiler more easy to understand.
\item Furthermore we use Attribute Grammars (AG) \cite{uuag} as replacement for the monadic structure in order to have a stateful supercompiler. Now programming becomes easier because you only have to deal with attributes. They are even simpler to reason with, because we can use tree figures to visualize the flow.
\end{itemize}

\textbf{Additional contributions}
\begin{itemize}
\item Bolingbroke extend the operational semantics by Sestoft but do not specify how to deal with renaming and computing free variables. We try to make this explicit in the semantic rules.
\item We created a single multi-step reducer, which fully reduce a state until no reduction could happen anymore. This differs from bolingbroke's implementation where it requires separate calls to $reduce$ and $normalize$ and where only one reduction step at the time happens.
\item We make a pure functional implementation not making use of unsafe IO operations.
\end{itemize}


\section{Supercompilatation}
\label{sec:supercompilation}

Supercompilation is the process of program transformation whereby the resulting program is optimized and should run faster. It combines together different optimization strategies that already exists such as partial evaluation, specialization and deforestation, which lead to an stronger optimization than when these strategies applied individually.

The idea of supercompilation is quite old, introduced by Turchin \cite{turchin}. The research from last years brings these ideas to call-by-need languages as well.
The approach by Mitchell \cite{mitchell} is more less ad-hoc, while Bolingbroke \cite{bolingbroke-sc-evaluation} came up with an implementation that closely follow the standard operational semantics for evaluation and also allow recursive let bindings. Bolingbroke showed that supercompilation is an abstraction that could be used for different lazy languages, as long you make necessary changes to adopt the operational semantics.

For our experiment we use a extended untyped lambda calculus, including \emph{variables}, lambda \emph{abstractions}, function \emph{applications}, \emph{recursive let}, datatype \emph{constructors} and pattern matches through \emph{case expressions}. Let us first take a look at two examples to become familiar with the language and see which kind of optimization can be reached with this supercompiler.

\compile{
  \begin{code}
  let
    inc = \x -> x+1
    map = \f xs ->
      case xs of
        [] -> []
        (y::ys) -> f y :: map f ys
  in
    map inc zs
  \end{code}
}{
  \begin{code}
  let
    h0 xs =
      case xs of
        [] -> []
        (y::ys) -> (y+1) :: h0 ys
  in
    h0 zs
  \end{code}
}

\noindent On the left you see the original program, mapping the function |inc| over all numbers in the list |zs|, which increments each item by one. You can see in the supercompiled version on the right the evaluation proceeds by specializing the call to |map| on its arguments. Then it will evaluate both branches despite of the fact |xs| is unknown yet. Then it will also specialize the function |inc| by inlining it at |f y|. Now the supercompiler will also proceed on the tail of the list despite of the fact the head isn't a value yet. When supercompiling the tail it encounters a expression that have the same pattern as the input program and therefore tiebacks on the term just supercompiled, which is bounded to |h0|. The result is a program where the function |inc| is fully inlined.

The following example shows that the supercompiling can do deforestation as well.

\compile{
  \begin{code}
    let
      map = \f xs ->
        case xs of
          [] -> []
          (y::ys) -> f y :: map f xs
    in
      map f (map g ys)
  \end{code}
}{
  \begin{code}
    let
      h0 f g xs =
        case xs of
          [] -> []
          (y::ys) -> f (g y) :: h0 f g ys
    in
      h0 f g xs
  \end{code}
}

\noindent What you see here is that fusion happens on the two calls to |map| to get rid off the intermediate data structure.



% We will look at program examples to show the process of supercompilation by describing that steps the compiler is taken.
% The following examples shows these lang some examples to
% Before we go in detail, first we show some results that can be obtained.\\

% The following example program increments all elements of a list by one. On the left you see the original program and on the right the program after supercompilation.
% \compile{
%   \begin{code}
%   let
%     inc = \x -> x+1
%     map = \f xs ->
%       case xs of
%         [] -> []
%         (y::ys) -> f y :: map f ys
%   in map inc zs
%   \end{code}
% }{
%   \begin{code}
%   let h0 zs =
%     case zs of
%       [] -> []
%       (y::ys) -> (y+1) :: h0 ys
%   in h0 zs
%   \end{code}
% }
% Normal evaluation proceeds by passing the arguments |inc| and |zs| to |map|, which become the parameters |f| and |xs| respectively. Now the evaluation become stuck in the case expression on the unknown |xs|. However, supercompilation can do better at compile time. It will look at both branches in order to evaluate these. In the cons branch, we take the first element |y| from the list and increment it |f y|. Therefore the function |inc| is specialized with the argument |y| and the result |y + 1| will be inlined in this branch. Again, normal evaluation will stop here because |y + 1| isn't a value, so it will not construct the rest of the list. However, the supercompiler will split this, and try to optimize the tail. Then it will see that the expression |map f ys| is equal to the original call |map inc zs| modulo renaming, therefore it tiebacks on the compilation just performed. The result is a program where the function increment is completely inlined.\\

% The following example shows that the supercompiler performs deforestation.
% \compile{
%   \begin{code}
%     let map = \f xs ->
%       case xs of
%         [] -> []
%         (y::ys) -> f y :: map f xs
%     in map f (map g ys)
%   \end{code}
% }{
%   \begin{code}
%     let h f g xs =
%       case xs of
%         [] -> []
%         (y::ys) -> f (g y) :: h f g ys
%     in h f g xs
%   \end{code}
% }
% It get rid off the intermediate data structure. The same result can be obtained with other deforestation techniques as described in literature, but deforestation is just one of the optimizations the supercompiler will perform as part of a single compile run.\\


\subsection{Requirements}
\label{subsec:components}

In order to do this kind of program transformations, the supercompiler is built of several components that work together. First of all we need an \textbf{evaluator} that partially evaluate expressions. For example, the evaluator will optimize an expression as follow.

\compile{
  \begin{code}
  let id = \x -> x
  in id not y
  \end{code}
}{
  \begin{code}
  let id = \x -> x
  in not y
  \end{code}
}
% \caption{The evaluator partially evaluates expressions.}

\noindent We also perform \textbf{dead code removal} to get rid off the unnessecary bindings.

\compile{
  \begin{code}
    let id = \x -> x
    in not y
  \end{code}
}{
  \begin{code}
    not y
  \end{code}
}

\noindent In the case evaluation gets stuck on a free variable, the \textbf{splitter} provides a way to individually evaluate sub parts of the expression.

\compile{
  \begin{code}
    let id = \x -> x
    in case x of
      True -> id False
      False -> id id True
  \end{code}
}{
  \begin{code}
  case x of
    True -> False
    False -> True
  \end{code}
}

\noindent There is also a component \textbf{drive} which will alternate the evaluation and splitting steps in order to obtain the following result.

\compile{
  \begin{code}
  let
    id = \x -> x
    not = \x ->
      case x of
        True -> False
        False -> True
    xnor = \x y ->
      case x of
        True -> id y
        False -> id not y
  in
    xnor x y
  \end{code}
}{
  \begin{code}
    case x of
      True -> y
      False ->
        case y of
          True -> False
          False -> True
  \end{code}
}

\noindent The drive function furthermore take care of \textbf{memoization} in order to tieback on expression patterns that are already supercompiled. We already have seen this in an earlier example where we tieback on a binding |h0|. In order to determine equal expression patterns we require a \textbf{match} function that can check for equality of terms. The matcher is discussed in more detail in section \ref{subsec:matching}.

Furthermore a supercompiler requires a \textbf{termination checker}. This due the fact that we may have to deal with infinite recursive function calls and infinite data types. In the first case the evaluation will never stop specializing, while in the second case the splitting will go on forever. This checker is conservative, and make sure the compiler always terminates, but at the cost of obtaining an fully optimized program. We will look at the termination checker in section \ref{subsec:termination}.



Now we have already point out the components of the supercompiler. Each component will be discussed in detail in section \ref{sec:components}.



\subsection{Working on states rather than terms}
\label{subsec:states}
Basically what a supercompiler does is alternating evaluation and splitting steps. For evaluation we need evaluation machinery which comes with a stack and heap. Because we have to split when evaluation gets stuck, we need splitting the state of that machine, which means that we have to split 3 aspects, namely the stack, the heap and the term currently in focus. Conclusion is that the supercompiler always works on states rather then terms. We denote a state as a 3-tuple with a heap |H|, a stack |K| and a term |e| in focus.\\


\begin{minipage}[c]{\textwidth}
\centering
\noindent \\\state{|H|}{|e|}{|K|}\\
\end{minipage}

\noindent \\We use the following notation as shorthand if we have to deal with an empty stack and empty heap.\\

\begin{minipage}[c]{\textwidth}
\centering
\noindent \\\stat{|e|}\\
% \\
\end{minipage}

\noindent \\We write down evaluation by a serie of reduction steps on states. For example, to evaluate the expression |let f = \x -> x in f y| we write\\

\begin{minipage}[c]{\textwidth}
\centering
\noindent \\\stat{|let x = a in f x|} \red\\
            \state{|x|$\mapsto$|a|}{|f x|}{\none} \red\\
            \state{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}}\\
\end{minipage}

\noindent \\The evaluation rules are discussed in more detail in section \ref{subsec:evaluate}. After the reduction gets stuck on a free variable it will perform a split, which we denote as follow.\\


{\qtreeshowframes \Tree [.{\state{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}}}
                          \terminal{|a|}
                          \terminal{|f|}
                          \terminal{|x|} ]}\\

\noindent \\In the example above the input state \state{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}} will be split and should result in substates. Each such substate could be individually reduced further and may split again. The substates denoted with a |star|$^*$ are not really substates in the sense that they can't be processed further, these states can be seen as terminal terms and will be used to build the optimized output term by folding the tree using a rebuild function. So in this example there are no substates other than terminal terms, which means that supercompilation stops here and rebuild a term |let x = a in f x|, which actually means that we do not retrieve any optimization at all in this case. We discuss splitting and rebuilding in more detail in section \ref{subsec:splitter}.






%  evaluation we have to deal with an machine It tries to evaluate as much as possible until it becomes stuck at some variable whose value is unknown. Then it tries to split the current state, resulting in smaller states that will be evaluated further. Note that the compiler always works on states rather then terms, so let discuss the reason for this first.


% We are always working on states, because evalution machine maintain a heap, a term and a stack, written as a 3-tuple

% \begin{figure}[H]
% \centering
% \state{H}{e}{K}
% \caption{State represenation with a heap |H|, a stack |K| and a term |e| in focus.}
% \end{figure}
% At the moment the evaluation gets stuck because |e|, the term in focus, is a variable whose value is unknown, the compiler need to split the state. This split results in zero or more substates, each of them is again a 3-tuple. The compiler can optimize programs much better when do the splitting on states rather then terms [REF bolingbroke].

% \begin{figure}[H]
% \qtreeshowframes \Tree [.{\state{...}{|case x of True->False; False->True|}{...}}
%                             {\state{...}{|False|}{...}}
%                             \terminal{|x|}
%                             {\state{...}{|True|}{...}} ]
% \caption{Split a state of a case expression that will flip the boolean value of |x|.}
% \label{fig:split}
% \end{figure}
% \noindent Figure \ref{fig:split} visualises how a split will be performed. Right now, we leave the heap and stack unspecified, it just shows how to split this state. It results in a substate for the scrutine and a substate for each branch. Note that this shows only a simplified version of the split, because the actual splitting is more complex and depends on the heap, the stack and the term in focus. We will dicuss that in section \ref{subsec:splitter}\\

% Furthermore a supercompiler requires a termination checker. This due the fact that we may have to deal with infinite recursive function calls and infinite data types. In the first case the evaluation will never stop specializing, while in the second case the splitting will go on forever. This checker is conservative, and make sure the compiler always terminates, but at the cost of obtaining an fully optimized program. We will look at the termination checker in section \ref{subsec:termination}.\\

% In section \ref{subsec:drive} we will discuss that a supercompiler also do memoization to tieback on previously supercompiled states if it encounters equal states. States are equal if they are the same modulo alpha-renaming, meaning that they may only differ on the free variables. The memoization prevents from superfluous function specialization. (ON THE OTHER HAND IT, NOW IT LOOKS LIKE NO SPECIALISATION IS HAPPEN IN THE FIRST PALCE) .\\

% Now we have already point out some components of the supercompiler. Each component will be discussed in detail in the following sections.




\section{Components}
\label{sec:components}

Now we will look at all components in more detail and also refer to implementation decisions. It could be useful to take a look at the examples in section \ref{sec:examples} already to keep overview and see final results.
% |supercompile| function...
% From here, work will be delegated to the other components.




\subsection{Evaluate}
\label{subsec:evaluate}

We implemented the evaluator using an AG, where each production matches a rule in the operational semantics from figure \ref{fig:opsemantics}. It will reduce a state by applying a serie of reduction steps, each performing a rule that matches the input state, until no further reduction could happen anymore. The output of the reducer is always a state that contains a term in focus that is either a variable or a value. No other term can be a valid output. The only values in our language are \emph{data types} and \emph{lambda abstractions}.

\begin{figure}
\begin{tabular}{lrcl}

\evalrule{VAR}
{\qstate{$H, x\new{\theta}\mapsto (e^{t_e},\new{\theta_e})$}{$x^{t}$}{$K$}{$\new{\theta}$}}
{\qstate{$H$}{$e^{t_e}$}{\UpdateFrame{t}{$x\new{\theta}$}$,K$}{\verify{$\theta_e$}}}
\\
\evalrule{UPDATE-val}
{ \qstate{$H$}{$v^{\unrelated{t}}$}{\UpdateFrame{t_x}{$x$}$,K$}{\new{$\theta$}} }
{ \qstate{$H,x \mapsto (v^{\verify{t_x}},\new{\theta})$}{$x^{t_x}$}{$K$}{\new{$\theta$}} }
\\
\evalrule{UPDATE-var}
{ \qstate{$H[x\new{\theta}\mapsto v^{t_v}]$}{$x^{\unrelated{t}}$}{\UpdateFrame{t_y}{$y$}$,K$}{\new{$\theta$}} }
{ \qstate{$H,y \mapsto (x^{\verify{t_y}},\new{\theta})$}{$x^{t}$}{$K$}{\new{$\theta$}} }
\\
\evalrule{APP}
{ \qstate{$H$}{$(e^{t_e}\,x)^{t}$}{$K$}{\new{$\theta$}} }
{ \qstate{$H$}{$e^{t_e}$}{\AppFrame{t}{$x\new{\theta}$}$,K$}{\new{$\theta$}} }
\\
\evalrule{BETA-val}
{ \qstate{$H$}{$(\lambda y.e^{t_e})^{\unrelated{t}}$}{\AppFrame{\unrelated{t_x}}{$x$}$,K$}{\new{$\theta$}} }
{ \qstate{$H$}{$e^{t_e}$}{$K$}{$\new{\theta[y\,\mapsto\,x]}$} }
\\
\evalrule{BETA-var}
{ \qstate{$H[f\new{\theta}\mapsto (\lambda y.e^{t_e})^{\unrelated{t_\lambda}}]$}{$f^{t}$}{\AppFrame{\unrelated{t_x}}{$x$}$,K$}{\new{$\theta$}} }
{ \qstate{$H$}{$e^{t_e}$}{$K$}{$\new{\theta[y\,\mapsto\,x]}$} }
\\
\evalrule{CASE}
{ \qstate{$H$}{$(\CASE e^{t_e} \OF \{ \overline{\kw{C}\,\overline{x} \arr e_C^{t_C}} \})^{t}$}{$K$}{$\new{\theta}$} }
{ \qstate{$H$}{$e^{t_e}$}{\CaseFrame{t,\unknow{\new{\theta}}}{\kw{C}\,\overline{x} \arr e_C^{t_C}}$,K$}{$\new{\theta}$} }
\\
\evalrule{DATA-val}
{ \qstate{$H$}{$(\kw{C}\,\overline{x})^{\unrelated{t}}$}{$($\CaseFrame{\verify{t_k},\new{\theta_k}}{\kw{C}\,\overline{y} \arr e^{t_e}}$^{t_k},\theta_k),K$}{$\new{\theta}$} }
{ \qstate{$H$}{$e^{t_e}$}{$K$}{$\verify{\theta_k}\new{[\overline{y\,\mapsto\,x\theta}]}$} }
\\
\evalrule{DATA-var}
{ \qstate{$H[z\new{\theta}\mapsto (\kw{C}\,\overline{x})^{\unrelated{t_C}}]$}{$z^t$}{$($\CaseFrame{\verify{t_k},\new{\theta_k}}{\kw{C}\,\overline{y} \arr e^{t_e}}$^{t_k},\theta_k),K$}{$\new{\theta}$} }
{ \qstate{$H$}{$e^{t_e}$}{$K$}{$\verify{\theta_k}\new{[\overline{y\,\mapsto\,x\theta}]}$} }
\\
\evalrule{LETREC}
{ \qstate{$H$}{$(\LET \overline{x = e_x^{t_x}} \IN e^{t_e})^{\unknow{t}}$}{$K$}{$\new{\theta}$} }
{ $\new{\LET \overline{x' = \mbox{\texttt{fresh}}}}$ }
\\
\evalrule{}
{  }
{ \new{\IN}  \qstate{$H,\overline{\new{x'} \mapsto (e_x^{t_x}, \new{\theta[x \mapsto x']})}$}{$e^{t_e}$}{$K$}{$\new{\theta[x \mapsto x']}$} }


\end{tabular}
\caption{Operational Semantics Evaluator}
\label{fig:opsemantics}
\end{figure}

These rules are an extended specification of the rules given by Bolingbroke. We added a fourth component to our tuple that will store the rename map. These renaming is essential in this semantics but not described by Bolingbroke. In general it follows the Sestoft-style semantics \cite{sestoft}, what means we build a rename map along the way and apply it at the moment a variable is used.

Notice that the rules |beta|, \emph{data} and |update| comes in two variants, namely a rule for variables and another rule that work on values. The variable rule could have been ommitted because we could choose to perform the |var| rule first followed by the rule for \emph{beta-val}, \emph{data-val} or \emph{update-val}. This will make sense to reach our goal to minimize the implementation. Although we have chosen to still include both variants, because it minifies the steps taken by the compiler and therefore make the examples where we show the compilers process much easier.

The reason why we implement the evaluator in an AG is just because it is simple to program in this way. In many rules you have to modify the heap, stack or term, but not all at the same time. In AG you have only to specify the changes, which makes it very handy for this purpose. Further it is just working like a classic function call, where the inherit attributes corresponds with arguments of the function call and a single synthesized attribute corresponds with a single (tupled) return value. In order to drive the AG, we uses a dummy data type named Step, which is instantiated as higher-order AG with the rule (or operation) we like to perform next. Each Step instantiates another Step, what means that we chain Steps together until we reach a fully reduced term
or when the termination checker will prevent us from going on.

A termination checker is involved to make sure no infinite recursion could happen. The only rule that could be responsible for infinite recursion is the \emph{beta} rule. Therefore Bolingbroke split the evaluator in two parts, named the functions |reduce| and |normalize|, where |reduce| is a evaluator which includes the \emph{beta} rule and therefore needs a termination check, while |normalize| is the same evaluator without \emph{beta} rule and therefore guarantees to stop. The calls to |reduce| and |normalize| are interleaved to make sure the result of |reduce| is normalized.
We just integrate these two functions into a single AG, and make use of AG's driving mechanism that will continuously apply rules until we reach a normalized state that cannot be reduced further. Therefore we call this a multi-step reducer. We integrate the termination check in the beta rule. If the check fails, it continues applying only normalize rules, that means all rules except \emph{beta}, and that makes sure it finally returns a normalized state.
We haven't proven that the approaches are indeed the same, but we assume based on the arguments given above.


% ---- TODO ---- !!!!!!!!!!!!!!! XXX
% + combine normalize reduce, using a flag.\\
% + implemented in AG, using a dummy data type $Step$ to drive AG.\\
% + A single chain down, therefore uses inherit attributes only and a single synthesized attribute to pass result up. States flows from step to step everytime. We only specify the changes of the state, the rest is passed on automatically.\\
% + check which rules matches, then use the corresponding AG rule is executed.\\
% + Some rules come in 2 variants, variables and values. Due performance less termination checks, and keep our process simpler.\\
% + Multiple reduce + normalize steps. (NOT NORMALIZING IN BETWEEN, is this a problem?)\\
% - Keep track of history to do termination check. This history don't correspond with the history of driving. It was newly created with empty list at the start of evaluating the state.
% - Rebuild only used for debug purpose, in the other case we use the build functions from the brackets. (not sure if renaming is done perfectly).\\
% - show the rules and how renaming is performed.\\
% - figure showing the chaining (higher-order) steps with inherit and synthesized attributus, indicate termination check.





\subsection{Splitter}
\label{subsec:splitter}

If a state cannot be reduced, it will be split in zero or more substates where each state is optimized individually. These substates are stored in a field called |holes| from the datatype |Bracket|. Along these substates we store a rebuild function that describes how it can be build an output term which make use of the optimized terms obtained from the substates. Therefore each substate corresponds with an argument passed to the rebuild function. A bracket for the state \stat{|case xs of True -> |\stat{$...$}|; False -> |\stat{$...$}} can looks as follows, where the |case| expression contains two branches and therefore have two substates, called $s_1$ and $s_2$.

\begin{code}
Bracket {
  holes = [ s1, s2 ],
  build = \ [e1,e2] ->
    case xs of
      True -> e1
      False -> e2
}
\end{code}

We will create individual brackets for the three components of the state \state{|H|}{|e|}{|K|} that finally will be combined together into a single bracket. The heap and stack brackets even composed of smaller brackets for every entry it contains. Combining the brackets can be done with a helper function |plusBrackets| which is the same thing as the rebuild function, except that this function requires brackets as input instead of states.

How the build function will looks like depends on the thing where we create a bracket for. For example, the heap will have a rebuild function that creates a |Let| term containing all heap bindings. The stack is composed of different brackets depending on the type of stack frame. Application frames generates an |App| term, update frames a |Var| term and case frames results in a |Case| term. As last we need a bracket for terms. We only have to deal with 3 type of terms, namely variables, datatypes and lambdas, this due the fact that the splitter receives normalized input, thanks to the normalization process described in section \ref{subsec:evaluate}, which means that we only have to deal with variables or values. In the case of a lambda value, we will first optimize the body. That means that we have a single substate and a rebuild function that wraps the optimized term into a |Lam| again.

\begin{code}
Bracket {
  holes = [ s ],
  build = \ [e] ->
    \x -> e
}
\end{code}

\noindent The bracket for a variable or datatype value is even simpler, because there are no substates involved and therefore needs a rebuild function that just return a constant term and not depends on any subterm. For example, a bracket for a variable |x| look like

\begin{code}
Bracket {
  holes = [],
  build = \ [] -> x
}
\end{code}

\noindent We call this a |terminal bracket| and denote it with a star like $^*$|x| or $^*$|Cons x xs|. This notation come in handy when we draw splitting figures. Note that what we call terminal brackets is \textbf{not} the same concept as a |termBracket| in Bolingbroke's paper, because a |termBracket e| is just wrapping a term into a bracket, by first wrapping it into state \stat{e}, lets call that state |s|, and produces a bracket that immediately returns the optimized subterm without modifying it, so that bracket looks like this

\begin{code}
Bracket {
  holes = [s],
  build = \ [e] -> e
}
\end{code}

Now we recall the example from the introduction and see which splitting steps are involved. We would like to split the following state\\

\begin{minipage}[c]{\textwidth}
\centering
\noindent \state{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}}
\end{minipage}\\

\noindent Here |x|$\mapsto$|a| is a heap binding, |f| the term in focus, and \AppFrame{}{|x|} the stack frame that applies |f| to |x|. First the term |f| will result in a terminal bracket $^*$|f|. Then the stack creates a |termBracket| for the expression |x|, which results in a terminal bracket $^*$|x|. After that the heap will create a |termBracket| for every binding, meaning that we obtain a terminal bracket $^*$|a| too. The split look as follows\\

{\qtreeshowframes \Tree [.{\state{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}}}
                        {\terminal{|a|}}
                              \terminal{|f|}
                              \terminal{|x|} ]}\\

Although, most splits are not that easy, because in many cases we need to push down heap bindings to substates. If the substate refers to such binder, we need to push down that heap binding. Some analysis is required in order to know which heap bindings should be passed down. We leave out this analysis and just pass down all heap bindings to all states in order to keep the implementation simple, at the cost of additional unnecessary compilation steps. That means that our splitting process will looks as follows.


{\qtreeshowframes \Tree [.{\state{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}}}
                        [.{\state{\err{|x|$\mapsto$|a|}}{|a|}{\none}}
                              \terminal{|a|}
                              [.{\state{\err{|x|$\mapsto$|a|}}{|a|}{\none}}
                                \terminal{|a|}
                                [.{\state{\err{|x|$\mapsto$|a|}}{|a|}{\none}}
                                  {$\infty$}
                                ]
                              ]
                        ]
                        \terminal{|f|}
                        \terminal{|x|} ]}\\

\noindent Notice that this compilation will go on and on forever if we would not have a termination checker, but we would see in the following section that memoization can take care of this too in some cases, which means that we only compile {\state{|x|$\mapsto$|a|}{|a|}{\none} once. We use the red marks as indication of heap bindings that are unnecessary passed down.


% {\qtreeshowframes \Tree [.{\stat{|let x = a in f x|} \red\\
%                            \state{|x|$\mapsto$|a|}{|f x|}{\none} \red\\
%                            \nstate{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}}{h1}}
%                         [.{\nstate{\err{|x|$\mapsto$|a|}}{|a|}{\none}{h2}}
%                               \terminal{|a|}
%                               {\err{\memo{\state{|x|$\mapsto$|a|}{|a|}{\none}}{h2}}} ]
%                         \terminal{|f|} ]}

% \noindent \\{\qtreeshowframes \Tree [.{\state{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}}}
%                         [.{\state{\none}{|a|}{\none}}
%                               \terminal{|a|} ]
%                         \terminal{|f|} ]}\\

% Additionally, splits cit can produce an indentity term which we denote as |e|$^*$. Such term is not a substate because it can't and not will be further reduced. These identity terms will be part of the rebuild function in the bracket only and therefore are useful if we start folding the tree in order to obtain the final optimized output term.

% In the case of zero states it means that the state can't be splitted and it results in an terminal bracket, noted as [br]*, which is a kind of identity bracket returning a term. If the split results in one or more states we try to optimize these terms and rebuild it to a term containing the optimized rebuilding of the brackets.

% SplitHeap is implemented by pushing down the heap bindings to the brackets obtained from splitStack and splitTerm.

% SplitStack...

% SplitTerm will result in terminal* brackets if the term is a variable or constructor. In the case of a lambda we do split, in order to further optimize the body.  Other terms are not considered in the splitter because we are working on normalized terms, meaning the term is a variable or a value. Only constructurors and lambda's are values in these language, so in total we only need to consider 3 different terms types.

% PlusBrackets combine brackets ogether

% TermBrackets is an pure idenity   bracket




\subsection{Drive}
\label{subsec:drive}


This drive component contains a function |sc| which drives the whole supercompilation, taking care of memoization, termination, splitting and building the output program.

\subsubsection{Attribute Grammar}
\label{subsubsec:drive-ag}

We implemented the drive component using an Attribute Grammar as a replacement for Bolingbroke's monadic approach. What it does is nothing more than depth-first traversal over a tree that dynamically grows when a state is split, making use of the higher-order functionality of AG to dynamically add child states to the split state. Promises will be generated along the way in order to do memoization, which means that later on in the traversal we can tieback on previously compiled states.

We use the following example program in order to draw nice AG figures, although the program itself is not very useful.

\begin{minipage}[t]{0.5\textwidth}
  \begin{code}

  case x of
    None ->
      let
        f = id
        g = not
      in
        f g a
    True -> False
    False -> True
  \end{code}
\end{minipage}
\begin{minipage}[t]{0.4\textwidth}
  \begin{figure}[H]
  \centering
  \includegraphics[width=1\textwidth]{resources/AG/ag-split.eps}
  \caption{}
  \label{fig:ag-split}
  \end{figure}
\end{minipage}


\noindent \\In figure \ref{fig:ag-split} the program is visualized in an AG diagram, which contains all states that are involved in the supercompilation. These states are normally produced while performing splits and will be instantiated on the fly using higher-order AG's, but for simplicity they are now all visible from the beginning. The states are labeled by $s_1...s_n$, the order depends on the order which they will be encountered. At each state we have written an abstract term representation that corresponds to that state along with the subterms that individually are supercompiled.




An overview of the requirements of the drive component is given here. These requirements closely relates to the attributes specified in the AG, so we mention them here and visualize them, along with their flow, in figures \ref{fig:ag-split}-\ref{fig:ag-history}.
\begin{itemize}
\item Assigning unique identifiers to states using attribute \textbf{freshProm},
      and performing splits that instantiates substates using \emph{higher-order} AG's. (fig. \ref{fig:ag-split})
\item Memoization is done by \emph{chaining} \textbf{promises}. (fig. \ref{fig:ag-promises})
\item States rebuild to terms. \emph{Synthesized} in the attribute \textbf{sc}. (fig. \ref{fig:ag-sc})
\item These terms fulfill promises, leading to \emph{synthesized} \textbf{bindings}. (fig. \ref{fig:ag-bindings})
\item To perform termination checking, states are \emph{inherited} in \textbf{history}. (fig. \ref{fig:ag-history})
% \item Implement memoization by \emph{chaining} the attribute \textbf{promises}, which holds promises for the states under supercompilation.
% \item States that can't be reduced further will split. The new states become \emph{dynamic} children of the current state under compilation, making use of the \emph{higher-order} functionality of AG.
% \item When the process ends because a state can't be splitted or is stopped due a termination check, it rebuilds to a term that is \emph{synthesized} by \textbf{sc}.
% \item Synthesized terms will also fullfill promises. We keep track of the fullfilled promises by \emph{synthesize} \textbf{bindings}. A binding is of the form |h1 = e|, where |e| is the optimized term bound to the promise identifier |h1|. These bindings will be part of the output program, living in top-level scope.
% \item To perform termination checks, we also hold a tagbag representation of every state under supercompilation. These are passed as an \emph{inherit} attribute \textbf{history}.
% \item Furthermore the AG helps assigning unique identifiers \textbf{freshProm} to promises.
\end{itemize}





\begin{figure}[H]
  \centering
  \begin{minipage}[b]{0.42\textwidth}
    \includegraphics[width=\textwidth]{resources/AG/ag-promises.eps}
    \caption{Promises are chained, used for memoization. A promise $p_i$ is an abstraction over the the state $s_i$.}
    \label{fig:ag-promises}
  \end{minipage}
  \hfill
  \begin{minipage}[b]{0.42\textwidth}
    \includegraphics[width=\textwidth]{resources/AG/ag-sc.eps}
    \caption{Optimized terms (expressions) will travel up. A term $e_i$ is build from their synthesized terms.}
    \label{fig:ag-sc}
  \end{minipage}
\end{figure}

\begin{figure}[H]
  \begin{minipage}[b]{0.42\textwidth}
    \includegraphics[width=\textwidth]{resources/AG/ag-bindings.eps}
    \caption{Synthesize bindings that will be used in output program. A binding $b_i$ consist of the term $e_i$ bound to a variable called $h_i$.}
    \label{fig:ag-bindings}
  \end{minipage}
  \hfil
  \begin{minipage}[b]{0.42\textwidth}
    \includegraphics[width=\textwidth]{resources/AG/ag-history.eps}
    \caption{History is passed down as a list of previously seen states, where $t_i$ is the tagbag representation of state $s_i$.}
    \label{fig:ag-history}
  \end{minipage}
\end{figure}



The relation between these attributes are visualized in figure \ref{fig:ag-deps}. It requires some explanation why so many arrows are involved. This have to do with the fact that we uses memoization and need to stop compiling when we find a memo. Because normally an AG automatically goes in recursion, we must insert a check everywhere, in order to prevent recursion when encountering a memo. So all synthesized attributes need to implement this check in the |lhs| rule of the AG. This explains why |promises| has arrows to |sc| and |bindings|. Furthermore at every split, which is responsible for dynamically creating children, a termination check required. A termination check needs the attribute |history|. So every chained or synthesized attribute that uses the children also depends on the inherit attribute |history|.

\begin{figure}[H]
  \centering
  \includegraphics[width=0.6\textwidth]{resources/AG/ag-dependencies.eps}
  \caption{Dependencies between attributes. The dashed component should be optional.}
  \label{fig:ag-deps}
\end{figure}

We draw the |promises| component with dashed lines because we think this component should be optional in a supercompiler, at least in order to create a minimal implementation. We do not succeed in leaving out this component, because our drive AG only terminates when all leaves are |terminal brackets|, not having sub states, or when encountering a memo. In order to leave out memoization we have to deal with the infinite unfolding as mentioned in the splitter (section \ref{subsec:splitter}). Maybe the termination checker should solve this problem in that case, but right now it only prevents from doing multiple |beta| reduction steps. This gap between memoization and termination checking needs additional research.




\subsubsection{Walkthrough}
\label{subsubsec:drive-walkthrough}

The process of driving starts with a single state that needs to be supercompiled. In order to memoize, a |promise| is created for this state. We know that at some moment this promise will be fulfilled with an optimized term. This also means that from now on, we can refer to this promise if we encounter a state equal to this one. Note that we do not check syntactic equivalence of states, but rather check for equal meaning as we discuss in the section \nameref{subsec:matching}. Therefore each time encountering a state, we check if it matches a memoized state. If no such match exists then we create a new promise labeled by a unique identifier.\\

\begin{minipage}[c]{\textwidth}
\centering
\stat{x}$_{h1}$
\end{minipage}

\noindent \\Otherwise, if we do find a match, we can tieback on a previously compiled state, indicated by the diamond symbol and the identifier of the state it refers to.\\

\begin{minipage}[c]{\textwidth}
\centering
\memo{\stat{x}}{h1}
\end{minipage}

\noindent \\After creating a promises it calls the functions |reduce|, |normalize| and |split|. Reducing the state means that we partially evaluate it, what is described already in section \nameref{subsec:evaluate}. Note that we only call |reduce| in the case this state passes the |termination check|, otherwise it could start to unfold infinitely. The reduced term need to be normalized again before passed to the splitter. The splitter delivers new states that are instantiated as higher-order AG's for the current state, what means we can recursively continue supercompiling on these states.

Sometimes memoization can also play the role as termination checker, because as we have seen in section \ref{subsec:splitter} that we repeatedly try to supercompile the same state. In that case, we only need to supercompile the first occurrence and tieback on this result in following occurrences. Therefore the AG from previous section now looks like this.\\

{\qtreeshowframes \Tree [.{\state{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}}$_{h1}$}
                            [.{\state{\err{|x|$\mapsto$|a|}}{|a|}{\none}$_{h2}$}
                                \terminal{|a|}
                                \memo{\state{\err{|x|$\mapsto$|a|}}{|a|}{\none}}{h2}
                            ]
                          \terminal{|f|}
                          \terminal{|x|} ]}\\

\noindent \\Now the memoization makes sure the supercompiler terminates. This only holds for splits that results in states that we have seen before. The following examples do need a termination checker because memoization not plays a role here

\begin{code}
let count n = n :: count (n + 1)
in count 0
\end{code}

\noindent This due the fact that every call to count leads to a function specialization for every unique integer. So every state is unique and cannot be matched by the matcher. Note that our language do not have to deal with integers, but the same can happen in the following example.

\begin{code}
let listsOf x xs = xs :: listsOf (x :: xs)
in listsOf y []
\end{code}

\noindent In order to perform termination checks, the drive component will keep track of |history|. It stores that seen states and passes it to the termination when performing a termination check.

\subsubsection{Results}
\label{subsubsec:drive-walkthrough}

We have shown that the drive function can be implemented within an AG, which make use of higher-order and automatic driving capabilities to traverse the tree and implicit pass on data like |promises| and |history| by making them explicit through attributes. We still need to do further research in order to omit memoization and still satisfy the termination property.

% XXX TODO XXXX

% \verify{(verify our implementation, because memoization seems to prevent from infinite unrolling, but that should probably depend on specialization of arguments). UNTIL NOW IT LOOKS MEMOIZATION CAN BE OMITTED DUE THE USE OF AG, WHERE LHS.SC WILL INFINITLY RECURSE BECAUSE IT ONLY STARTS SYNTHESIZING ON A MEMOIZED TERM, THIS KIND OF TERMINATION SHOULD MUST HANDLED BY TERMINATION CHECKER INSTEAF OF MEMOIZATION}.

% ----

% Branches after each other (depth first)\\   !!!!!

% Although we use AG, we do nothing more than a depth-first traversal on a tree that dynamically grows when a state is splitted, making use of the higher-order functionality of AG to dynamically add the children states. After the split, the depth-first traversal proceed by supercompiling the first child. These process corresponds with Bolingbroke's monadic approach where he monadically recurse on the children states.


% While it is implemented using attribute
% In Bolingbroke's implementation a modadic stucture is used for chaining |promises| and sy, while |history| is passed by argument, and the output



%  driving function is implemented using AG in order to do memoization easily. Memoization is done by creating a promise at the moment a new state (that hasn't seen before) starts to compile. The AG contains an attribute |promises| to chain the gathered promises, so that it can be accessed when compiling the next state.

% When a state starts to supercompile, a promise is aThis means that when promises will be chained, so that they can be used when compiling a state that has been seen before. When to chain  promises for already supercompiled terms.

% -------

%  When compiling a state, it will first check if there is already a |matching| promise and uses that one, or create a promise otherwise and supercompiles this state.

% single chain AG\\
% check if there is memo (match) otherwise:\\
% Creates promise and history\\
% Reduce (only if it terminates)\\
% Split\\
%
% \\
% termination is performed because:



% In section \ref{subsec:drive} we will discuss that a supercompiler also do memoization to tieback on previously supercompiled states if it encounters equal states. States are equal if they are the same modulo alpha-renaming, meaning that they may only differ on the free variables. The memoization prevents from superfluous function specialization. (ON THE OTHER HAND IT, NOW IT LOOKS LIKE NO SPECIALISATION IS HAPPEN IN THE FIRST PALCE, -> I thought that, but integer arguments will be always specialized, but just a single variable not, so I think this is okay) .\\





\subsection{Matching}
\label{subsec:matching}

The matcher is required to let memoization work. The purpose of these module is to match two states and check if they are equal. This does not test for syntactic equality but rather test for equal meaning, this means equality modulo alpha-renaming. That means that in order to identify two states $s_1$ and $s_2$ equal, there should exists two substitutions $\theta_1$ and $\theta_2$, so that

  $$s_1\theta_1 = s_2\theta_2 = s$$

\noindent where $s$ is a state that is the most-specific generalization of this two states. Notice that $\theta$ contains variable-to-variable mappings, so only performs alpha-renaming. The matcher is responsible for finding this substitutions $\theta_1$ and $\theta_2$, and furthermore finds the common state $s$.

Although our goal is to create a minimal implementation of the supercompiler, we choose to fully implement the matcher as described in Bolingbroke's paper. Mainly because we could not figure out which parts are actually required, due the complexity that is involved to deal with work sharing. This is the reason why the matcher also delivers the common state $s$ even we are only interested in $\theta$ to make memoization work.

This time we could not make use of AG to implement this component. Because what we do here is try to match two data structures, while AG is intended to work on a single datatype. So we use a monadic structure, the same as what Bolingbroke did.

The matcher starts comparing two states, by comparing the state's heap, term and stack, using the functions |matchHeap|, |matchTerm| and |matchStack|. Furthermore |matchTerm| delegates work to |matchAlt|, |matchBind| and |matchVar| to respectively match case alternatives, let-bindings and variables.
The main concept is to substitute each free variable in both states to a single unique variable that the states then will have in common. In order to supply unique identifiers we need the monadic structure. Also lambda- and let-binders need to be renamed in order to make terms equal.

Bolingbroke also specify a function |fixup| that will run before returning $\theta$ and $s$. He suggests that the purpose of this function is to obtain work sharing. But just leave out this function can not be done. In that case compilation may not terminate because of direct recursive promises. Not sure why that is the case, but just include the fixup function solves the problem.

It could be that there are still some errors in the matcher, because in our opinion it is the most complex component of the supercompiler. Bolingbroke discussed the matcher but leave details out and refers to general knowledge about matching in lambda calculus. This makes it hard to reimplement and therefore our implementation needs verification.




\subsection{Termination}
\label{subsec:termination}

A termination checker is required to make sure the compilation process is finite. If this checker determines that compilation will loop forever, it stops further evaluation of the current state.
Termination checks are used at two places. One in the drive function and one in the evaluation.

\begin{enumerate}
\item The termination check within drive component is performed because otherwise it will possibly unfold a data structure forever when one of the substates repeatedly tiebacks on the same promise, but not triggers memoization. An example is
\begin{code}
let count n = n :: count (n + 1)
in count 0
\end{code}

\item The termination check in the evaluator is required to prevent non-termination in the case of recursive functions that diverge.
\begin{code}
let count n = count (n + 1)
in count 0
\end{code}
The compiler is also not smart enough to determine non-diverge functions as like the example below. It just stops when encounter the recursive function call.
\begin{code}
let count n =
  case n of
    0 -> 0
    _ -> count (n - 1)
in count 10
\end{code}
\end{enumerate}

The termination check fails in the case it recognizes equal states. Equality is determined by the tagbag representation of states, which have nothing to do with equality checks in the matcher. The concept of tagbags is explained by Bolingbroke, so we do not repeat it here. Although we can make some small notes, that were maybe not directly clear from his paper.
\begin{itemize}
\item The main idea is that it checks for equal states both containing the same terms, but only may differs term occurrences. This due unfolding where a term from the heap is inserted into the focus term. If it starts to occur multiple times, but for the rest stays the same state, then we stop compiling. For example this will happen when start to unfold an addition like |x|, |(x + x)| and |(x + x + x)| where all expressions have the same tagbag representation because outer terms of composed expressions |(x + x)| have just the same tag as one of its arguments. Another example is that from above |count (n - 1)| where you see a recursive call, where |(n - 1)| have the same tag each call and therefore encountered equally.
\item A tagbag can be viewed as a key-value mapping. The keys are integers, each representing a unique term. The values describe how many times they are present. For example the variable |x| tagged as |x|$^1$ can be in a state for two times like \state{|f|$\mapsto$|x|$^1$}{|x|$^1$}{\none}, which gives us a tagbag ${1\mapsto2}$.
\item The tagbag for a single term is just a singleton set containing the tag of the outer term, so the inner terms are not encountered.
\item The tagbag of the heap is just the union of the tagbag representation of all bindings, not including the binder variable itself.
\end{itemize}


% TODO XXX

% CHECK HOW THiS RELATES TO MEMOIZATION XXX

% explain only the top tags are used, we don't take the union of the whole tree. So only the tags of a state <H*, e, K*> are taken, meaning only the top-level terms of heap-bindings, a single term in focus and possible some fixed stack tags.
% We compare with all previous states, because the recusive call can somewhere deeper in the tree.
% the main idea is that it checks for a state containing the exact same terms, and differs in the occurences. This due the unfolding/app reading multiple times a variables from the heap and insert the term. If it starts to occur multiple times, but without that extra occurence we have the state already before, then we stop commpiling.
% It only mean it possible will not terminate, but we not know how to check, so stop immediatly.





\subsection{Free Variables}
\label{subsec:fvs}

Getting the free variables is trivial and Bolingbroke therefore do not explain it in his paper. Although the semantics of the supercompiler is not standard, therefore I think there is the need to explain it in more detail, especially how to deal with renaming.

\begin{figure}[H]
|fvs| \qstate{|H|}{|e|}{|K|}{$\theta_e$} |=| |(|$|fvs|_H$|(H)|$\theta_e$ $\cup$ $|fvs|_e$|(e)| $\cup$ $|fvs|_K$|(K))| $\setminus$ |(|$|bvs|_H$|(H)| $\cup$ $|bvs|_K$|(K))| \\

$|fvs|_H(\emptyset)$ |=| $\emptyset$\\
$|fvs|_H((|x|\mapsto|e|,\theta_e),|H|)$ |=| $|fvs|_e$|(e)|$\theta_e$ $\cup$ $|fvs|_H$|(H)|\\

$|fvs|_K(\emptyset)$ |=| $\emptyset$\\
$|fvs|_K($\AppFrame{}{|x|}$|,K|)$ |=| $\{x\}$ $\cup$ $|fvs|_K$|(K)|\\
$|fvs|_K(($\textbf{case}\CaseFrame{}{alt}$,\theta_c)|,K|)$ |=| $\bigcup_{\overline{alt}}|fvs|_{alt}$|(alt)|$\theta_c$ $\cup$ $|fvs|_K$|(K)|\\
$|fvs|_K(|_,K|)$ |=| $|fvs|_K$|(K)|\\

$|fvs|_{alt}(\kw{C}\,\overline{y} \arr e)$ |=| $|fvs|_e$|(e)| $\setminus$ $\overline{y}$\\

$|fvs|_e(|x|)$ |=| $\{x\}$\\
$|fvs|_e(|\x.e|)$ |=| $|fvs|_e$|(e)| $\setminus$ $\{x\}$\\
$|fvs|_e(|f x|)$ |=| $|fvs|_e$|(f)| $\cup$ $\{x\}$\\
$|fvs|_e(|C|\,\overline{|x|})$ |=| $\overline{|x|}$\\
$|fvs|_e(\LET \overline{x = e_x} \IN e)$ |=| |(|$\bigcup_{\overline{e_x}}|fvs|_{e}(e_x)$ $\cup$ $|fvs|_e$|(e))| $\setminus$ $\overline{x}$\\
\caption[fvs]{free variables}
\label{fig:fvs}
\end{figure}

The most interesting part is to how calculate free variables from a state tuple \state{H}{e}{K}. The formal rules are written in figure \ref{fig:fvs}. We first calculate the free variables of every component. Then we compute the free variables for the state as whole, which is not just the union of these components because some of the variables become bound by the binders from the heap or the update frames from the stack. Therefore we need to omit some of these variables and we do that by subtracting a set of bound variables, which rules are written down in figure \ref{fig:bvs}. The result will be the variables that are free in this state.

\begin{figure}[H]
$|bvs|_H(\emptyset)$ |=| $\emptyset$\\
$|bvs|_H(|x|\mapsto|e,H|)$ |=| $\{x\}$ $\cup$ $|bvs|_H$|(H)|\\

$|bvs|_K(\emptyset)$ |=| $\emptyset$\\
$|bvs|_K($\UpdateFrame{}{|x|}$|,K|)$ |=| $\{x\}$ $\cup$ $|bvs|_K$|(K)|\\
$|bvs|_K(|_,K|)$ |=| $|bvs|_K$|(K)|\\
\caption[bvs]{bound variables}
\label{fig:bvs}
\end{figure}

Additionally we make explicit how to deal with renaming, because within a state there are also rename maps stored. In the first place as fourth component in a state tuple \qstate{H}{e}{K}{$\theta$}, but also in all heap bindings and the stack frames for case expressions. The fourth component renaming must be applied on the term in focus, you can see that happens in the |fvs| rule. The renaming stored in the heap bindings and the stack frames for case expressions must be applied on the corresponding heap binding and case alternatives respectively, what you can see happen in the rules $|fvs|_H$ and $|fvs|_K$.








\subsection{Renaming}
\label{subsec:renaming}

The rename component provides three different functions that are all related to renaming. One is the uniqueness renaming, used to initial rename the program to make all binders uniquely named. The second one is a helper function to rename terms including binders, which is used in the matcher. This one have some overlap with the uniqueness renaming, except that the renaming is given instead of uniquely supplied. The third one is the renaming of variables that is performed while supercompiling, following the Sestoft-style semantics, which keeps track of a rename map and apply the renaming on variables at the time of their usage.

\begin{enumerate}
\item
The initial uniqueness renaming does a single traversal over a term and renames all binders of |let|, |lambda| and |case| expression patterns. The implementation requires no further explanation.
\item
The second renaming function renames a complete term, including the binders. This follows the same approach as uniqueness naming, but instead of introducing unique names, we provide a renaming map. The renaming of whole terms is used in the matcher in order to match two terms, by trying to make them equal, which also requires the same naming of binders.
\item
The renaming that is performed while supercompiling is more complex. Therefore we take a closer look at it. Given a rename map, it can easily rename some variable using the function $rename$. Although the question is, do we need transitive lookups? For example when the rename map contains the following mapping:\\

|{x -> y, y -> z}|\\

If we like to know $x$ and not do transitive lookups, we only get the result $y$. If we do transitive lookups we get $z$. But we must ask ourself if such transitive mapping can exists in the first place. The answer is no, these transitive mapping \emph{cannot} exists. For example, encounter this example that at first sight will give the transitive bindings from above:
\begin{code}
(\y. (\x . _) y) z
\end{code}
But if we look at the operational semantics and apply the \emph{beta} rule on the outer lambda, it results in a renaming from |y| to |z|. Then it encounters the inner lambda where it inserts the renaming |x| to |z|, instead of |x| to |y|. This is because we first apply the renaming |y|$\theta$ before the inner lambda is applied. The final mapping looks like this:\\

|{x -> z, y -> z}|\\

In general what is does is apply the renaming before inserting a new mapping. This will happen in the following three evaluation rules \emph{beta}, \emph{data} and \emph{let}.
\end{enumerate}

On second thoughts our initial uniqueness renaming (1) is somewhat superfluous. We just renamed all binders in order to prevent from creating transitive binding. But now we have shown in (3) that such bindings will not exists for lambda abstractions, we do not have to rename the lambda binders. But we still need to rename let bindings and case patterns, because they are stored in the heap and otherwise would be overwritten if we encounter nested bindings using the same name.

Bolingbroke not does initial renaming at all, but just does it at run time. We cannot figure out if this is a strict requirement of the semantics. Our approach will maybe fail in recursive structures, at the moment heap bindings will be instantiated, because then the same name is used while Bolingbroke generates a unique name at that moment. We still need to verify if this is indeed the case.





\subsection{Dead Code Removal}
\label{subsec:deadcode}

Dead code removal is introduced to make the output program more readable by omitting bindings that aren't used.
It's not sufficient to only look at the body to see which bindings are used, because bindings can also call each other due to use of recursive let. On the other hand we can not simply look at all bindings that are somewhere called, because it's not useful to look at the calls within dead bindings.
Therefore we uses a iterative approach to follow the bindings that are called from the body, resulting in list of bindings that are not-dead.

The function |deadCodeRemoval| is called as last step in the pipeline to clean up to output program. But right now, we need to call it within the compiling process as well. This due our goal to do not all optimizations and push down all heap bindings in the splitter, including dead bindings. These results in additional free variables. Because promises are abstracted over these free variables, they become abstracted over dead code. By performing dead code analysis before we calculate the free variables we prevent from the superfluous abstraction. In the future we need to implement this analysis in the splitter itself to prevent the work duplication from the beginning.







%-------------------------------=  --------------------------------------------
\section{Examples}
\label{sec:examples}
%-------------------------------=  --------------------------------------------

Now we show some examples to visualize the supercompilation process. Given an input program we show the evaluation-, reduction- and splitting steps that are taken. The supercompilation output is an optimized program specified in memoized bindings $h_1 \dots h_n$. This program can be feed to a normal evaluator to simplify the output. The final output is an optimized version of our input program.\\




\entry
{\textbf{}}
{\textbf{input\\program}}
{\begin{center}\textbf{supercompilation\\steps}\end{center}}
{\textbf{output\\supercompilation}}
{\textbf{output\\program}}


% \begin{figure}
\entry{VAR}
{\begin{code}
  x
\end{code}}
{\qtreeshowframes \Tree [.\nstat{|x|}{h1} \terminal{|x|} ]}
{\begin{code}
  let h1 = \x. x
  in h1 x
\end{code}}
{\begin{code}
  x
\end{code}}
{
  \begin{itemize}
  \item Start with the state \state{\none}{$x$}{\none}.
  \item Because $x$ isn't in the heap, we can't look it up and can't reduce this term.
  \item So we split \state{\none}{$x$}{\none}, which creates a memoized binding $h_1$. There is no heap or stack to be split, but we try to split $x$. Because $x$ can't be split, we will receive \terminal{|x|}, which is a terminal bracket who rebuilds to the term $x$.
  \item This term fulfill the promise $h_1$. It is abstracted over the free variables |\x.x|, resulting in a binding |h1 = \x.x| that travels up in th tree.
  \item A final program is created by wrapping all bindings in a |let| construct and make an initial call to the first (and only) binding $h1$.
  \end{itemize}
}
% \caption[Example VAR]{
%   Start with the state \state{\none}{$x$}{\none}.
%   Because $x$ isn't in the heap, we can't look it up and can't reduce this term.
%   So we split \state{\none}{$x$}{\none}, which creates a memoized binding $h_1$. There is no heap or stack to be split, but we try to split $x$. Because $x$ can't be split, we will receive $x^*$ which is the identity bracket, who rebuilds to the term $x$.
% }
% \label{ex:var}
% \end{figure}




\entry{APP}
{\begin{code}
  f x
\end{code}}
{\qtreeshowframes \Tree [.{\stat{|f x|} \red \\
                         \nstate{\none}{|f|}{\AppFrame{}{|x|}}{h1}}
                         \terminal{|f|}
                         \terminal{|x|} ]}
{\begin{code}
  let h1 = \f \x. f x
  in h1 f x
\end{code}}
{\begin{code}
  f x
\end{code}}
{
  \begin{itemize}
  \item \state{\none}{$f x$}{\none} reduces to \state{\none}{$f$}{\AppFrame{}{$x$}} when the $APP$ rule is applied.
  \item Because $f$ isn't in the heap, we can't apply the $BETA$ rule, so no further reduction is performed.
  \item This state starts splitting after creating a promise $h_1$. Because the variable $f$ can't be split it wil return a terminal bracket \terminal{|f|}. The stack frame \AppFrame{}{$x$} also returns a terminal bracket \terminal{|x|}.
  \item The promise $h_1$ is fulfilled, abstracted over the free variables, resulting in |h1 = \f\x.f x|.
  \end{itemize}
}



\entry{LAM}
{\begin{code}
  \x -> x
\end{code}}
{\qtreeshowframes \Tree [.\nstat{|\x -> x|}{h1} [.\nstat{|x|}{h2} \terminal{|x|} ]]}
{\begin{code}
  let
    h1 = \x -> x
    [h2 = \x.x]
  in h1
\end{code}}
{\begin{code}
  \x -> x
\end{code}}
{
  \begin{itemize}
  \item The state \state{\none}{|\x -> x|}{\none} can't be reduced because the lambda expression is already a value and no stack frame can be applied.
  \item Split this term results in a promise $h_1$, where the state \state{\none}{|x|}{\none} (body of the lambda expression) will be further reduced.
  \item Because \state{\none}{|x|}{\none} can't be reduced we split again, resulting in a promise $h_2$, and the terminal bracket $x^*$.
  \item \textbf{Note} that the origin of the similar looking promises $h_1$ and $h_2$ are not the same. They both results in identity functions, the only  difference is that $h_1$ is a promise for the expression |\x -> x| while $h_2$ is a promise only for the variable |x|. Beause $h_2$ needs to be abstracted over the free variables it results in |\x.x|. We use different lambda notation to distinguish between the binders that origins from the expression itself, and the ones that are created due the abstraction of the promise.
  \item The reason why the promise $h_2$ is written in brackets is because it isn't used in the output program, therefore we can safely eliminate this binding. Although $h_2$ was required in the process of supercompilation in order to rebuild $h_1$.
  \end{itemize}
}


\entry{LAM-2}
{\begin{code}
  \x -> y
\end{code}}
{\qtreeshowframes \Tree [.\nstat{|\x -> y|}{h1} [.\nstat{|y|}{h2} \terminal{|y|} ]]}
{\begin{code}
  let
    h1 = \y. \x -> y
    [h2 = \y. y]
  in h1 y
\end{code}}
{\begin{code}
  \x -> y
\end{code}}
{
  \begin{itemize}
  \item Almost the same example as LAM in the way of compilation. Only now, $h_1$ is abstracted over the free variable |y|. Notice that the binder |x| is still part of the expression |\x -> y| and has nothing to do with the abstraction over the promise.
  \end{itemize}
}



\entry{CON}
{\begin{code}
  Cons x xs
\end{code}}
{\qtreeshowframes \Tree [.\nstat{|Cons x xs|}{h1} \terminal{|Cons x xs|} ]}
{\begin{code}
  let h1 = \x \xs.
    Cons x xs
  in h1 x xs
\end{code}}
{\begin{code}
  Cons x xs
\end{code}}
{
  \begin{itemize}
  \item Datatypes are values in our language, so these term cannot reduce or split, resulting in a terminal brack \terminal{|Cons x xs|}.
  \item Note that the arguments of constructors are always variables. For that reason we do not split the arguments. If these variables points to existing heap bindings, then these bindings are optmized due the fact the splitter optimizes all heap bindings anyway.
  \end{itemize} 
}




\entry{LET}
{\begin{code}
  let x = a in x
\end{code}}
{\qtreeshowframes \Tree [.{\stat{|let x = a in x|} \red \\
                            \state{|x|$\mapsto$|a|}{|x|}{\none} \red \\
                            \nstate{\none}{|a|}{\UpdateFrame{}{|x|}}{h1}
                           } 
                            \terminal{|a|} 
                            [.\nstat{|x|}{h2} 
                              \terminal{|x|} ]]}
{\begin{code}
  let
    h1 = \a.
      let x = a in x
    [h2 = \x. x]
  in h1 a
\end{code}}
{\begin{code}
  a
\end{code}}
{
  \begin{itemize}
  \item The state \stat{|let x = a in x|} reduces by the LET and VAR rules.
  \item Then the state \state{\none}{|a|}{\UpdateFrame{}{|x|}} is split. 
  \item The focus term cannot be split and directly results in the terminal bracket \terminal{|a|}.
  \item Splitting a the stack frame \UpdateFrame{}{$x$} is now defined in terms of a call to |termBracket|. This means means that we first obtain an identity bracket and get the terminal bracket \terminal{x} afterwards. The implementation can be probably simplified in order to return the terminal bracket directly. We did that also for \AppFrame{}{$x$} in the APP example. 
  \item Furthermore the split of \UpdateFrame{}{$x$} make sure that it passes a binding |\x->a| to the function |splitHeap| in order to finally rebuild to a |let| expression that include the binding again.
  \end{itemize} 
}




\entry{LET-2}
{\begin{code}
  let x = a in f x
\end{code}}
{\qtreeshowframes \Tree [.{\stat{|let x = a in f x|} \red\\
                           \state{|x|$\mapsto$|a|}{|f x|}{\none} \red\\
                           \nstate{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}}{h1}}
                        [.{\nstate{\err{|x|$\mapsto$|a|}}{|a|}{\none}{h2}}
                              \terminal{|a|}
                              {\err{\memo{\state{|x|$\mapsto$|a|}{|a|}{\none}}{h2}}} ]
                        \terminal{|f|} ]}
{\begin{code}
  let
    h1 = \a \f.
      let x = a in f x
    [h2 = \a.a]
  in h1 a f
\end{code}}
{\begin{code}
  f a
\end{code}}
{
  \begin{itemize}
  \item It reduces \stat{|let x = a in f x|} to a state \state{|x|$\mapsto$|a|}{|f|}{\AppFrame{}{|x|}} by apply the LET and APP rules.
  \item Then it becomes stuck on the variable |f|, that is not present in the heap, so we split.
  \item The variables |f| and |x| directly results in terminal brackets \terminal{|f|} and \terminal{|x|}.
  \item Furthermore there will be a substate for every heap binding, in this case the substate \stat{|a|}.
  \item Because we leave out smart analysis to determine which heap bindings are alive, we just push down all bindings. What actually means we create a bracket for the state \state{|x|$\mapsto$|a|}{|a|}{\none}. Because this could have been omitted we mark it red.
  \item Splitting that state, results in a terminal bracket \terminal{|a|} and a substate that we already have seen before, therefore we just tieback on the promise $h_2$, indicated by the diamond symbol.
  \end{itemize} 
}




\entry{CASE}
{\begin{code}
  case x of
    T -> F
    F -> T
\end{code}}
{\qtreeshowframes \Tree [.{\stat{|case x of T->F; F->T|} \red \\
                           \nstate{\none}{|x|}{\CaseFrame{}{C\mapsto e}}{h1}}
                              [.{\state{\err{|x|$\mapsto$|T|}}{|F|}{\none}$_{h2}$}
                                    [.{\err{\state{|x|$\mapsto$|T|}{|T|}{\none}$_{h3}$}}
                                          {\err{\memo{\state{|x|$\mapsto$|T|}{|T|}{\none}}{h3}}}
                                          {\err{\terminal{|T|}}} ]
                                      {\terminal{|F|}} ]
                              \terminal{|x|}
                              {\memo{\state{\err{|x|$\mapsto$|F|}}{|T|}{\none}}{h3}} ]}
{\begin{code}
  let
    h1 = \x.
        case x of
          T -> F
          F -> h3
    [h2 = F]
    h3 = T
  in h1 x
\end{code}}
{\begin{code}
  case x of
    T -> F
    F -> T
\end{code}}
{
  \begin{itemize}
  \item The scrutine of a case expression becomes the focus term, the alternatives are stored in the stack.
  \item Splitting the focus term results in a terminal bracket \terminal{x}. Splitting the alternatives each results in a substate.
  \item Notice that the case alternative patterns contain knowledge that is useful when optimize the branches. Therefore this knowledge is included through an additional heap binding.
  \item Furtermore we pass down all heap bindings, which results in many unecessary compilation steps. This also means that when encounter the second branch, we have already compiled all heap bindings in the compilation of the first branch, so we can just tieback on the results obtained in $h_3$. This also explains why the second branch in the output program contains a reference to a promise, while the first branch directly reached a value.
  \end{itemize} 
}



\entry{S-APP}
{\begin{code}
  (\x -> x) y
\end{code}}
{\qtreeshowframes \Tree [.{\stat{|(\x -> x) y|} \red\\
                          \state{\none}{|\x -> x|}{\AppFrame{}{|y|}} \red  \\
                          \qstate{\none}{|x|}{\none}{$\theta[x\mapsto y]$}$_{h_1}$} {\terminal{|x|$\theta$}} ]}
{\begin{code}
  let h1 = \y. y
  in h1 y
\end{code}}
{\begin{code}
  y
\end{code}}
{
  \begin{itemize}
  \item First apply the APP rule, then the rule for BETA. Now we deal with the renaming introduced by the beta reduction. Therefore we use the 4-tuple notation to store the substitution (rename map) in the state explicit.
  \item Splitting this state results in a terminal bracket \terminal{|x|}. We always apply the substitution on a terminal bracket \terminal{|x|$\theta$} resulting in \terminal{|y|} before returning it. 
  \item Rename the terminal brackets should have been done in the previous examples as well. We do not mentioned it because the rename map is still empty there.
  \end{itemize} 
}



\entry{S-ID}
{\begin{code}
  let id =
    \x. -> x
  in id y
\end{code}}
{\qtreeshowframes \Tree [.{\stat{|let id = \x -> x in id y|} \red \\
                           \state{|id|$\mapsto$\dots}{|id y|}{\none} \red \\
                           \state{|id|$\mapsto$\dots}{|id|}{\AppFrame{}{|y|}} \red \\
                           \qstate{|id|$\mapsto$\dots}{|x|}{\none}{$\theta[x\mapsto y]$}$_{h_1}$}
                        \terminal{|x|$\theta$}
                        [.\qstate{\err{|id|$\mapsto$\dots}}{|\x -> x|}{\none}{$\theta$}$_{h2}$
                            [.{\qstate{\err{|id|$\mapsto$\dots}}{|x|}{\none}{$\theta$}$_{h3}$}
                                 \terminal{|x|$\theta$}
                                 \err{\memo{\qstate{|id|$\mapsto$\dots}{|\x -> x|}{\none}{$\theta$}}{h2}} ]
                            \err{\memo{\qstate{|id|$\mapsto$\dots}{|\x -> x|}{\none}{$\theta$}}{h2}} ]
                         ]}
{\begin{code}
  let h1 = \y. y
      [h2 = \x -> x]
      [h3 = \x.x]
  in h1 y
\end{code}}
{\begin{code}
  y
\end{code}}
{
  \begin{itemize}
  \item Reduction follows the steps LET, APP and BETA-VAR. The BETA-VAR rule does the lookup in the heap and then apply that expression to the variable stored in \AppFrame{}{|y|}.
  \item The beta reduction generates a substitution that must be passed down to the substates.
  \item The substitution is applied when rebuilding the term.
  \end{itemize} 
}

\noindent \\Until now we only listed the basic examples above. In the next version of this paper we like to add complex examples and also show failing cases.

\pagebreak

\section{Conclusion}
\label{sec:conclusion}

We reimplement Bolingbroke's supercompiler in a minimal form, making use of attribute grammars instead of monadic structures. By doing this we encountered several difficulties, sometimes due the lack of explanation in the existing papers. Therefore we use visuals to show the compilation process and make splitting steps explicit. We also formalized the operational semantics of the evaluator and give rules how to compute |fvs| and |bvs|. Especially we focussed on making explicit how to deal with renaming. We not really succeed in making a minimal compiler because leaving out components will quickly results in non-termination. For example, memoization is required otherwise the AG will recurse forever. Besides that fact, the AG turns out to be useful in order to create a nice implementation for evaluation and driving. We not succeed in making the compiler fully correct. There is still work to do in order to fix bugs.

%-------------------------------=  --------------------------------------------
\begin{thebibliography}{9}
%-------------------------------=  --------------------------------------------

\bibitem{bolingbroke-callbyneed-sc}
  Bolingbroke, Maximilian C. Call-by-need supercompilation. \emph{University of Cambridge, Computer Laboratory, UCAM-CL-TR-835}, May 2013.
\bibitem{bolingbroke-sc-evaluation}
  M. Bolingbroke and S. Peyton Jones. Supercompilation by evaluation. In \emph{Proceedings of the 2010 ACM SIGPLAN Haskell Symposium}, September 2010.
\bibitem{mitchell}
  Neil Mitchell. Rethinking supercompilation. In \emph{Proceedings of the ACM SIGPLAN International Conference on Functional Program- ming, ICFP 2010}. ACM, 2010.
\bibitem{sestoft}
  P. Sestoft. Deriving a lazy abstract machine. \emph{Journal of Functional Programming, 7(03):231–264}, 1997.
\bibitem{turchin}
  Valentin F. Turchin. The concept of a supercompiler. \emph{ACM Trans. Program. Lang. Syst., 8(3):292–325}, 1986.
\bibitem{uuag}
  UU Attribute Grammar Compiler (UUAGC), Software Technology, Utrecht University, http://foswiki.cs.uu.nl/foswiki/HUT/AttributeGrammarSystem

\end{thebibliography}

\end{document}
