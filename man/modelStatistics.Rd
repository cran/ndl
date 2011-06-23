\name{modelStatistics}
\alias{modelStatistics}

\title{
  Calculate a range of goodness of fit measures for an object fitted with some multivariate statistical method that yields probability estimates for outcomes.
}

\description{
  \code{modelStatistics} calculates a range of goodness of fit
  measures.
}

\usage{
  modelStatistics(observed, predicted, frequency=NA, p.values,
     n.data, n.predictors, outcomes=levels(as.factor(observed)),
     p.normalize=TRUE, cross.tabulation=TRUE, 
     p.zero.correction=1/(nrow(p.values)*ncol(p.values))^2)
}
\arguments{
  \item{observed}{observed values of the response variable}
  \item{predicted}{predicted values of the response variable; typically the outcome estimated to have the highest probability}
  \item{frequency}{frequencies of observed and predicted values; if \code{NA}, frequencies equal to 1 for all observed and predicted values}
  \item{p.values}{matrix of probabilities for all values of the response variable (i.e outcomes)}
  \item{n.data}{sum frequency of data points in model}
  \item{n.predictors}{number of predictor levels in model}
  \item{outcomes}{a vector with the possible values of the response variable}
  \item{p.normalize}{if \code{TRUE}, probabilities are normalized so that \code{sum(P)} of all outcomes for each datapoint is equal to 1}
  \item{cross.tabulation}{if \code{TRUE}, statistics on the crosstabulation of observed and predicted response values are calculated with \code{crosstableStatistics}}
  \item{p.zero.correction}{a function to adjust slightly response/outcome-specific probability estimates which are exactly P=0; necessary for the proper calculation of pseudo-R-squared statistics; by default calculated on the basis of the dimensions of the matrix of probabilities \code{p.values}.}
}

\value{ A list with the following components: \describe{
  \item{\code{loglikelihood.null}}{Loglikelihood for null model}
  \item{\code{loglikelihood.model}}{Loglikelihood for fitted model}
  \item{\code{deviance.null}}{Null deviance}
  \item{\code{deviance.model}}{Model deviance}
  \item{\code{R2.likelihood}}{(McFadden's) R-squared}
  \item{\code{R2.nagelkerke}}{Nagelkerke's R-squared}
  \item{\code{AIC.model}}{Akaike's Information Criterion}
  \item{\code{BIC.model}}{Bayesian Information Criterion}
  \item{\code{C}}{index of concordance C (for binary response variables only)}
  \item{\code{crosstable}}{Crosstabulation of observed and predicted
   outcomes, if \code{cross.tabulation=TRUE}}
  \item{\code{crosstableStatistics(crosstable)}}{Various statistics
   calculated on \code{crosstable} with \code{crosstableStatistics}, if
  \code{cross.tabulation=TRUE}} }
}

\references{
Arppe, A. 2008. Univariate, bivariate and multivariate methods in
corpus-based lexicography -- a study of synonymy. Publications of the
Department of General Linguistics, University of Helsinki,
No. 44. URN: http://urn.fi/URN:ISBN:978-952-10-5175-3.

Arppe, A., and Baayen, R. H. (in prep.) Statistical modeling and the
principles of human learning.

Hosmer, David W., Jr., and Stanley Lemeshow 2000. Applied Regression Analysis 
(2nd edition). New York: Wiley.
}
\author{
Antti Arppe and Harald Baayen
}
\seealso{
See also \code{\link{ndlClassify}}, \code{\link{ndlStatistics}}, \code{\link{crosstableStatistics}}.
}
\examples{
data(think)
think.ndl <- ndlClassify(Lexeme ~ Agent + Patient, data=think)
probs <- acts2probs(think.ndl$activationMatrix)$p
preds <- acts2probs(think.ndl$activationMatrix)$predicted
n.data <- nrow(think)
n.predictors <- nrow(think.ndl$weightMatrix) *
   ncol(think.ndl$weightMatrix)
modelStatistics(observed=think$Lexeme, predicted=preds, p.values=probs,
   n.data=n.data, n.predictors=n.predictors)
}
\keyword{ discriminative learning }
