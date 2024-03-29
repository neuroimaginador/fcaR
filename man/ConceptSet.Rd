% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concept_set.R
\name{ConceptSet}
\alias{ConceptSet}
\title{R6 class for a set of concepts}
\description{
This class implements the data structure and methods for concept sets.
}
\examples{
# Build a formal context
fc_planets <- FormalContext$new(planets)

# Find the concepts
fc_planets$find_concepts()

# Find join- and meet- irreducible elements
fc_planets$concepts$join_irreducibles()
fc_planets$concepts$meet_irreducibles()


}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ConceptSet-new}{\code{ConceptSet$new()}}
\item \href{#method-ConceptSet-size}{\code{ConceptSet$size()}}
\item \href{#method-ConceptSet-is_empty}{\code{ConceptSet$is_empty()}}
\item \href{#method-ConceptSet-extents}{\code{ConceptSet$extents()}}
\item \href{#method-ConceptSet-intents}{\code{ConceptSet$intents()}}
\item \href{#method-ConceptSet-print}{\code{ConceptSet$print()}}
\item \href{#method-ConceptSet-to_latex}{\code{ConceptSet$to_latex()}}
\item \href{#method-ConceptSet-to_list}{\code{ConceptSet$to_list()}}
\item \href{#method-ConceptSet-bracket}{\code{ConceptSet$[()}}
\item \href{#method-ConceptSet-sub}{\code{ConceptSet$sub()}}
\item \href{#method-ConceptSet-support}{\code{ConceptSet$support()}}
\item \href{#method-ConceptSet-clone}{\code{ConceptSet$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-new"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{ConceptLattice} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$new(extents, intents, objects, attributes, I = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{extents}}{(\code{dgCMatrix}) The extents of all concepts}

\item{\code{intents}}{(\code{dgCMatrix}) The intents of all concepts}

\item{\code{objects}}{(character vector) Names of the objects in the formal context}

\item{\code{attributes}}{(character vector) Names of the attributes in the formal context}

\item{\code{I}}{(\code{dgCMatrix}) The matrix of the formal context}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{ConceptLattice} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-size"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-size}{}}}
\subsection{Method \code{size()}}{
Size of the Lattice
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$size()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The number of concepts in the lattice.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-is_empty"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-is_empty}{}}}
\subsection{Method \code{is_empty()}}{
Is the lattice empty?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$is_empty()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{TRUE} if the lattice has no concepts.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-extents"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-extents}{}}}
\subsection{Method \code{extents()}}{
Concept Extents
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$extents()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The extents of all concepts, as a \code{dgCMatrix}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-intents"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-intents}{}}}
\subsection{Method \code{intents()}}{
Concept Intents
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$intents()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The intents of all concepts, as a \code{dgCMatrix}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-print"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-print}{}}}
\subsection{Method \code{print()}}{
Print the Concept Set
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Nothing, just prints the concepts
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-to_latex"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-to_latex}{}}}
\subsection{Method \code{to_latex()}}{
Write in LaTeX
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$to_latex(print = TRUE, ncols = 1, numbered = TRUE, align = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{print}}{(logical) Print to output?}

\item{\code{ncols}}{(integer) Number of columns of the output.}

\item{\code{numbered}}{(logical) Number the concepts?}

\item{\code{align}}{(logical) Align objects and attributes independently?}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The \code{LaTeX} code to list all concepts.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-to_list"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-to_list}{}}}
\subsection{Method \code{to_list()}}{
Returns a list with all the concepts
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$to_list()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of concepts.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-bracket"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-bracket}{}}}
\subsection{Method \code{[()}}{
Subsets a ConceptSet
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$[(indices)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{indices}}{(numeric or logical vector) The indices of the concepts to return as a list of Concepts. It can be a vector of logicals where \code{TRUE} elements are to be retained.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Another ConceptSet.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-sub"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-sub}{}}}
\subsection{Method \code{sub()}}{
Individual Concepts
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$sub(index)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{index}}{(numeric) The index of the concept to return.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The Concept.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-support"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-support}{}}}
\subsection{Method \code{support()}}{
Get support of each concept
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$support()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A vector with the support of each concept.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConceptSet-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ConceptSet-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConceptSet$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
