\name{acts2probs}
\alias{acts2probs}

\title{
  Calculate probability matrix from activation matrix, as well as predicted
  values
}

\description{
  \code{acts2probs} takes the activation matrix returned by
  \code{\link{ndlClassify}} and calculates the matrix of probabilities
  for the estimated activation matrix, as well as the predicted values
  of the response variable.
}

\usage{
acts2probs(acts)
}

\arguments{
  \item{acts}{A matrix of activations (number of observations by number of levels
    of the response variable).}
}
\details{
  Probabilities in \code{p} are obtained by adding the absolute value
  of the minimum activation to the activation matrix, and
  renorming. The selection rule used to produce \code{predicted} is to
  choose for each instance in the data the outcome value that has
  received the highest probability estimate.
}
\value{
  A list with the following components:
  \describe{
    \item{\code{p}}{a matrix with the probabilities of the levels of
      the response variable for each observation.}
    \item{\code{predicted}}{a character vector with predicted values.}
  }
}
\references{
Arppe, A. and Baayen, R. H. (in prep.). Statistical classification and principles 
of human learning.
}

\author{
Harald Baayen and Antti Arppe
}

\seealso{
See also \code{\link{ndlClassify}}.
}

\examples{
data(think)
think.ndl <- ndlClassify(Lexeme ~ Person + Number + Agent + Register, data=think)
pdata <- acts2probs(think.ndl$activationMatrix)
}

\keyword{ discriminative learning }
