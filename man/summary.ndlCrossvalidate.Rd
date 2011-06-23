\name{summary.ndlCrossvalidate}
\alias{summary.ndlCrossvalidate}
\alias{print.summary.ndlCrossvalidate}

\title{A summary of a crossvalidation of a Naive Discriminatory Reader Model
}

\description{
  A summarization method for an object of the class \code{"ndlCrossvalidate"}.
}

\usage{
\method{summary}{ndlCrossvalidate}(object, \dots)

\method{print}{summary.ndlCrossvalidate}(x, digits = max(3, getOption("digits") - 3), \dots)
}

\arguments{
\item{object}{
      An object of class \code{"ndlCrossvalidate"}, resulting from a call to
      \code{ndlCrossvalidate}.
     }
\item{x}{An object of class \code{"summary.ndlCrossvalidate"}, usually resulting from a
     call to \code{summary.ndlCrossvalidate}.
     }
\item{digits}{
    the number of significant digits to use when printing.
    }
\item{\dots}{further arguments passed to or from other methods.
    }
}

\details{
  Calculates overall descriptive statistics of the crossvalidation of
  a fitted Naive Discriminatory Reader model and prints a nice summary
  of the key results.
}

\value{
  \code{summary.ndlCrossvalidate} returns an object of the class
  \code{"summary.ndlCrossvalidate"}, a list with the following components:
\describe{
  \item{\code{call}}{The call matched to fit  the \code{"ndlCrossvalidate"} object.
  }
  \item{\code{formula}}{The formula specified for the \code{"ndlCrossvalidate"} object.
  }
  \item{\code{statistics.summary}}{The means, minima and maxima of a
  range descriptive statistics for the fit and performance of
  individual folds; see \code{\link{ndlStatistics}}.
  }
  \item{\code{crosstable.summary}}{The means of the crosstabulation of
  observed and predicted outcomes for the held-out test data.
  }
  \item{\code{recall.predicted.summary}}{The means of the recall
  values for the individual outcomes predicted with the held-out test
  data.
  }
  \item{\code{precision.predicted.summary}}{The means of the precision
  values for the individual outcomes predicted with the held-out test
  data.
  }
  \item{\code{statistics.all}}{All the values for a range descriptive
   statistics for the fit and performance of individual folds on the
   held-out test data; see \code{\link{ndlStatistics}}.
  }
  \item{\code{k}}{The number of folds.
  }
  \item{\code{n.total}}{The sum frequency of all data points in
  \code{data}.
  }
  \item{\code{n.train}}{The sum frequency of data points used for
  training the individual models (excluding the individual folds).
  }
  \item{\code{n.test}}{The sum frequency of data points in the
  individual held-out folds used for testing the individual models.
  }
}}

\references{
Arppe, A. and Baayen, R. H. (in prep.)
}

\author{
Antti Arppe
}

\seealso{
\code{\link{ndlCrossvalidate}, \link{ndlClassify}, \link{ndlStatistics}}
}

\examples{

## For examples see examples(ndlCrossvalidate).

}

\keyword{ classif }
