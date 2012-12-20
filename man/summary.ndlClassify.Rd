\name{summary.ndlClassify}
\alias{summary.ndlClassify}
\alias{print.summary.ndlClassify}

\title{A summary of a Naive Discriminatory Learning Model
}

\description{
  A summarization method for an object of the class \code{"ndlClassify"}.
}

\usage{
\method{summary}{ndlClassify}(object, \dots)

\method{print}{summary.ndlClassify}(x, digits = max(3, getOption("digits") - 3), max.print=10, \dots)
}

\arguments{
\item{object}{
      An object of class \code{"ndlClassify"}, resulting from a call to
      \code{ndlClassify}.
     }
\item{x}{
     An object of class \code{"summary.ndlClassify"}, usually
     resulting from a call to \code{summary.ndlClassify}.
     }
\item{digits}{
    The number of significant digits to use when printing.
    }
\item{max.print}{
    The maximum number of rows of \code{weights} to be output when
    printing; by default equal to 10; ; if set to \code{NA} all rows
    will be output.
    }
\item{\dots}{
    Control arguments passed to or from other methods,
    e.g. \code{ndlStatistics} and \code{modelStatistics}.
    }
}

\details{
  Calculates descriptive statistics of a fitted Naive Discriminatory
  Learning model and prints a nice summary of the key results.
}

\value{
  \code{summary.ndlClassify} returns an object of the class
  \code{"summary.ndlClassify"}, a list with the following components:
\describe{
  \item{\code{call}}{The call matched to fit  the \code{"ndlClassify"} object.
  }
  \item{\code{formula}}{The formula specified for the \code{"ndlClassify"} object.
  }
  \item{\code{weights}}{The estimated weights.
  }
  \item{\code{statistics}}{A range of descriptive statistics calculated with \code{ndlStatistics}.
  }
  }
}

\references{
Arppe, A. and Baayen, R. H. (in prep.)
}

\author{
Antti Arppe
}

\seealso{
\code{\link{ndlClassify}, \link{ndlStatistics}, \link{modelStatistics}}
}

\examples{

## For examples see examples(ndlClassify).

}

\keyword{ classif }
