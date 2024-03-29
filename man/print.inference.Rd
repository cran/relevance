\name{print.inference}
\title{Print Tables with Inference Measures}
\alias{print.inference}
\alias{print.termtable}
\alias{print.termeffects}
\alias{print.printInference}
\description{Print methods for objects of class
  \code{"inference"}, \code{"termtable"}, \code{"termeffects"},
  or \code{"printInference"}.
}
\usage{
\method{print}{inference}(x, show = getOption("show.inference"), print=TRUE,
  digits = getOption("digits.reduced"), transpose.ok = TRUE,
  legend = NULL, na.print = getOption("na.print"), ...)

\method{print}{termtable}(x, show = getOption("show.inference"), ...)

\method{print}{termeffects}(x, show = getOption("show.inference"),
  transpose.ok = TRUE, single = FALSE, print = TRUE, warn = TRUE, ...)

\method{print}{printInference}(x, ...)
}
\arguments{
  \item{x}{object to be printed}
  \item{show}{determines items (columns) to be shown}
  \item{digits}{number of significant digits to be printed}
  \item{transpose.ok}{logical: May a single column be shown as a row?}
  \item{single}{logical: Should components with a single coefficient
    be printed?}
  \item{legend}{logical: should the legend(s) for the symbols
    characterizing p-values and relevances be printed?
    Defaults to \code{regroptions("show.symbolLegend")}.
  }
  \item{na.print}{string by which \code{NA}s are shown}
  \item{print}{logical: if \code{FALSE}, no printing will occur,
    used to edit the result before printing it.}
  \item{warn}{logical: Should the warning be issued if
    \code{termeffects} has nothing to print since there are no
    terms with more than one degree of freedom}
  \item{...}{further arguments, passed to \code{\link{print.data.frame}()}.}
}
\value{A kind of formatted version of \code{x}, with class
  \code{printInference}.
  For \code{print.inference}, it will be
  a character vector or a data.frame with attributes
  \code{head} and \code{tail} if applicable.
  For \code{print.termeffects}, it will be a list of such elements,
  with its own \code{head} and \code{tail}.
  It is invisibly returned.
}
\details{The value, if assigned to \code{rr}, say, can be printed by using
  \code{print.printInference}, writing \code{print(rr)}, which is just
  what happens internally unless \code{print=FALSE} is used.
  This allows for editing the result before printing it, see Examples.

  \code{printInference} objects can be a vector, a data.frame or a
  matrix, or a list of such items.
  Each item can have an attribute \code{head} of mode character that is
  printed by \code{cat} before the item, and analogous with a
  \code{tail} attribute. 
  }
\author{Werner A. Stahel}

\seealso{
  \code{\link{twosamples}}, \code{\link{termtable}},
  \code{\link{termeffects}}, \code{\link{inference}}.
}
\examples{
data(d.blast)
r.blast <-
  lm(log10(tremor)~location+log10(distance)+log10(charge), data=d.blast)
rt <- termtable(r.blast)
## print() : first default, then "classical" :
rt
print(rt, show="classical")

class(te <- termeffects(r.blast)) #  "termeffects"
rr <- print(te, print=FALSE)
attr(rr, "head") <- sub("lm", "Linear Regression", attr(rr, "head"))
class(rr) # "printInference"
rr # <==>  print(rr)

str(rr)
}
\keyword{regression}
\keyword{utility}
\concept{relevance}
