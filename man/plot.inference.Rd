\name{plot.inference}
\alias{plot.inference}
\alias{plot.termeffects}

\title{Plot Inference Results
}
\description{Plot confidence or relevance interval(s)
  for one or several items
}
\usage{
\method{plot}{inference}(x, pos = NULL, overlap = FALSE, 
  refline = c(0,1,-1), xlab = "relevance", ...)
\method{plot}{termeffects}(x, pos = NULL, single=FALSE,
  overlap = TRUE, termeffects.gap = 0.2, refline = c(0, 1, -1),
  xlim=NULL, ylim=NULL, xlab = "relevance", mar=NA,
  labellength=getOption("labellength"), ...)
}

\arguments{
  \item{x}{a vector or matrix of class \code{inference}.
  }
  \item{pos}{positions of the bars in vertical direction
  }
  \item{overlap}{logical: should shortened intervals be shown to show
    significance of differences?
    see Details}
  %%-   \item{col}{color to be used for the confidence intervals,
  %%-     usually a vector of colors if used.
  %%-   }
  \item{refline}{values for vertical reference lines
  }
  \item{single}{logical: should terms with a single degree of freedom
    be plotted?
  }
  \item{termeffects.gap}{gap between blocks corresponding to terms
  }
  \item{xlim, ylim}{limits of plotting area, as usual}
  \item{xlab}{label for horizontal axis}
  \item{mar}{plot margins. If \code{NULL} (default),
    the left side margin will be adjusted to accomodate the labels
    of effects of factor levels
  }
  \item{labellength}{maximum number of characters for label strings
  }
  \item{\dots}{further arguments to the call of \code{plot.inference}
    (for\code{plot.termeffects}) and \code{plot}
  }
}
\details{The overlap interval allows for a graphical assessment
  of the significance of the test for zero difference(s),
  akin the notches in the box plots:
  The difference between a pair of groups is siginificant if their
  overlap intervals do not overlap.
  For equal standard errors of the groups, the standard error of the
  difference between two of them is larger by the factor \code{sqrt(2)}.
  Therefore, the intervals should be shortened by this factor, or
  multiplied by \code{1/sqrt(2)}, which is the default for
  \code{overlapfactor}.
  If only two groups are to be shown, the factor is adjusted to unequal
  standard errors. 
  
  The graphical options are:
  \describe{
    \item{\code{lwd}: }{line widths for: [1] the interval,
      [2] middle mark, [3] end marks, [4] overlap interval marks,
      [5] vertical line marking the relevance threshold
    }
    \item{\code{markheight}: }{determines the length of the middle mark,
      the end marks and the marks for the overlap interval
      as a multiplier of the default length}
    \item{\code{extend}: }{extension of the vertical axis beyond the range}
    \item{\code{framecol}: }{color to be used for the framing lines:
      axis and vertical lines at relevances 0 and 1}
  }
}
\note{\code{plot.inference} displays \code{termtable} objects, too, 
  since they inherit from class \code{inference}.
}
\value{none
}
\seealso{\code{\link{plconfint}}
}  
\author{Werner A. Stahel
}

%- \seealso{}
\examples{
## --- regression
data(swiss)
rr <- lm(Fertility ~ . , data = swiss)
rt <- termtable(rr)
plot(rt)

## --- termeffects
data(d.blast)
rlm <- lm(log10(tremor)~location+log10(distance)+log10(charge), data=d.blast)
rte <- termeffects(rlm)
plot(rte, single=TRUE)
}
\keyword{hplot}
\concept{relevance}
