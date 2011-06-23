\name{plot.ndlClassify}
\alias{plot.ndlClassify}
\alias{plot.ndlWeights}
\alias{plot.ndlProbabilities}
\title{
  Plot function for selected results of \code{ndlClassify}.
}
\description{
  This function presents visually the estimated weights or expected
  probabilities for a model fitted with \code{ndlClassify}

}
\usage{
\method{plot}{ndlClassify}(x, values="weights", \dots)

\method{plot}{ndlWeights}(x, type="density", predictors=NULL, outcomes=NULL,
panes="single", lty=NULL, col=NULL, mfrow=NULL, main=NULL,
legend.position="topright", \dots)

\method{plot}{ndlProbabilities}(x, type="density", select="all",
panes="single", lty=NULL, col=NULL, pch=NULL, mfrow=NULL,
main=NULL, legend.position="topright", \dots)

}
\arguments{
  \item{x}{
     A object of the class \code{"ndlClassify"} produced by
     \code{ndlClassify}, consisting of a list including estimated
     weights for predictors and association strengths for
     outcome-predictor combinations.
  }
  \item{values}{
     A character string specifiying whether estimated \code{weights}
     (default) or expected \code{probabilities} should be plotted.
  }
  \item{type}{ 
     A character string spefifying the type of plot to be drawn;
     \code{density} is available for both value types as default,
     while a histogram (\code{hist}) is available only for
     \code{plot.ndlWeights} and sorted values (\code{sort}) only for
     \code{plot.ndlProbabilities}.
  }
  \item{panes}{
     A character string specifying whether a \code{single} pane
     (default) integrating all component plots, or \code{multiple}
     panes for each individual component plot are to be plotted. If
     \code{multiple} panes are selected, the number or rows and
     columns is specified automatically. Alternatively, one can invoke
     the plotting of multiple panes by explicitly specifying the
     appropriate number of rows and columns with the parameter
     \code{mfrow} (N.B. this overrides \code{panes="single"}).
  }
  \item{predictors}{ 
     A regular expression specifying which predictors and their values
     should be included in the plot(s); by default \code{=NULL} so that
     all predictors incorporated in the \code{ndlClassify} model will
     be included.
  }
  \item{outcomes}{
     A list of outcomes to be included in the plot; by default \code{=NULL} so that all
     outcomes will be considered.
  }
  \item{select}{
    For \code{plot.ndlProbabilities}, a character string specifying
    which instance-wise probability estimates should be plotted; by
    default \code{all}, other values are \code{max} for instance-wise
    maximum probabilities, \code{min} for instance-wise minimum
    probabilities, \code{maxmin, minmax} for both maximum and minimum
    instance-wise probabilities. Alternatively, a numeric vector
    \code{c(1,2,\dots)} specifying selected ranks of the instance-wise
    probability estimates can be provided, with \code{1} corresponding
    to the instance-wise maximum probability estimates.
  }
  \item{lty, col, pch, mfrow, main, legend.position}{
    Specifications of various graphical parameters (see
    \code{\link{par}}) to be used in the plots; if any of these is set
    to \code{=NULL} default settings will be used (for
    \code{legend.position}, the default value is \code{topright}). Note that
    \code{lty} is relevant only to \code{plot.ndlWeights(\dots,
    type="density", \dots)} and \code{plot.ndlProbabilities(\dots,
    type="density", \dots)}, and \code{pch} only to
    \code{plot.ndlProbabilities(\dots, type="sort", \dots)}.
  }
  \item{\dots}{ 
     Arguments to be passed to methods, such as graphical
     parameters (see \code{\link{par}}).
  }
}
\value{
  A plot of the selected type is produced on the graphics device.
}
\references{
Arppe, A. and Baayen, R. H. (in prep.)
}
\author{
Antti Arppe and R. H. Baayen
}
\seealso{
\code{\link{ndlClassify}}, \code{\link{acts2probs}}
}
\examples{
\dontrun{

data(think)
think.ndl <- ndlClassify(Lexeme ~ Agent + Patient + Section, data=think)

plot(think.ndl, values="weights")
plot(think.ndl, values="weights", type="hist", panes="multiple")
plot(think.ndl, values="weights", type="density", panes="multiple")
plot(think.ndl, values="weights", type="density", panes="multiple",
   predictors="Section*")
plot(think.ndl, values="weights", type="density", panes="multiple",
   predictors="Patient*")
plot(think.ndl, values="weights", type="hist", panes="multiple", col=1:4)
plot(think.ndl, values="weights", type="density", panes="single",
   outcomes=c("ajatella","miettia","pohtia","harkita"))

plot(think.ndl, values="probabilities")
plot(think.ndl, values="probabilities", panes="multiple")
plot(think.ndl, values="probabilities", select="max")
plot(think.ndl, values="probabilities", select=c(1:3))
plot(think.ndl, values="probabilities", panes="multiple", select=c(1:3))
plot(think.ndl, values="probabilities", type="sort", legend.position="topleft")
plot(think.ndl, values="probabilities", type="sort", pch=".",
   legend.position="topleft")
plot(think.ndl, values="probabilities", type="sort", pch=".", panes="multiple")
}
}
\keyword{ classif }
