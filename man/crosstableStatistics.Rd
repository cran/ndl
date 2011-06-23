\name{crosstableStatistics}
\alias{crosstableStatistics}
\title{
  Calculate statistics for a contingency table 
}
\description{
  \code{crosstableStatistics} takes a contingency table of observed
  vs. predicted values for a binary or polytomous response variable as
  input, and calculates a range of statistics about prediction
  accuracy.
}
\usage{
crosstableStatistics(ctable)
}
\arguments{
  \item{ctable}{
    A contingency table cross-classifying observed and predicted values.
  }
}
\value{
  A list with the following components:
  \describe{
    \item{\code{accuracy}}{Overall prediction accuracy}
    \item{\code{recall.predicted}}{Recall of prediction for each outcome value}
    \item{\code{precision.predicted}}{Precision of prediction for each outcome value}
    \item{\code{lambda.prediction}}{lambda for prediction accuracy (improvement over baseline of always predicting mode)}
    \item{\code{tau.classification}}{tau for classification accuracy (improvement over baseline of homogeneous distribution of predicted outcomes)}
    \item{\code{d.lambda.prediction}}{d(lambda): used for calculating \code{P(lambda)}}
    \item{\code{d.tau.classification}}{d(tau): used for calculating \code{P(tau)}}
    \item{\code{p.lambda.prediction}}{P(lambda): probability of reaching \code{lambda} by chance}
    \item{\code{p.tau.classification}}{P(tau): probability of reaching \code{tau} by chance}
  }
}
\references{
  Arppe, A. 2008. Univariate, bivariate and multivariate methods in
  corpus-based lexicography -- a study of synonymy. Publications of
  the Department of General Linguistics, University of Helsinki,
  No. 44. URN: http://urn.fi/URN:ISBN:978-952-10-5175-3.

  Arppe, A. and Baayen, R. H. (in prep.). Statistical classification
  and principles of human learning.

  Menard, Scott (1995). Applied Logistic Regression Analysis. Sage
  University Paper Series on Quantitative Applications in the Social
  Sciences 07-106. Thousand Oaks: Sage Publications.
}
\author{
Antti Arppe and Harald Baayen
}
\seealso{
See also \code{\link{modelStatistics}, \link{ndlStatistics}, \link{ndlClassify}}.
}
\examples{
ctable <- matrix(c(30, 10, 5, 60), 2, 2)
crosstableStatistics(ctable)
}
\keyword{ discriminative learning }
