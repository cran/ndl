\name{ndlVarimp}
\alias{ndlVarimp}
\title{
  Permutation variable importance for classification using naive discriminative learning.
}
\description{
  \code{ndlVarimp} uses permutation variable importance for naive
  discriminative classification models, typically the output of
  \code{ndlClassify}.
}
\usage{
ndlVarimp(object, verbose=TRUE)
}
\arguments{
  \item{object}{
    An object of class \code{"ndlClassify"} (or one that can be
    coerced to that class); typically a model object as produced by
    \code{\link{ndlClassify}}.
  }
  \item{verbose}{ 
    A logical (default TRUE) specifying whether the successive predictors
    being evaluated should be echoed to stdout.  
  }
}
\details{
Variable importance is assessed using predictor permutation.
Currently, conditional permutation variable importance (as for \code{varimp}
for random forests in the \code{party} package) is not implemented.
}
\value{
  A list with two numeric vectors:
\describe{
  \item{\code{concordance}}{For binary response variables, a named vector
  specifying for each predictor the index of concordance when that predictor
  is permuted.  For polytomous response variables, NA.}
  \item{\code{accuracy}}{ A named vector specifying for each predictor
  the accuracy of the model with that predictor permuted.}
}
}
\references{
  R. Harald Baayen (2011).  Corpus linguistics and naive 
  discriminative learning.  Brazilian journal of applied 
  linguistics, in press.

  Carolin Strobl, Anne-Laure Boulesteix, Thomas Kneib, Thomas
  Augustin and Achim Zeileis (2008).  Conditional Variable
  Importance for Random Forests. BMC Bioinformatics, 9, 307.

}
\author{
R. H. Baayen and Antti Arppe
}
\seealso{
\code{\link{summary.ndlClassify}, \link{plot.ndlClassify}, \link{anova.ndlClassify}, \link{ndlCuesOutcomes}, \link{estimateWeights}, \link{cueCoding}}
}
\examples{
\dontrun{
data(dative)
dative <- dative[!is.na(dative$Speaker),-2]
dative.ndl <- ndlClassify(RealizationOfRecipient ~ ., data=dative)
dative.varimp <- ndlVarimp(dative.ndl)

library(lattice)
dotplot(sort(summary(dative.ndl)$statistics$accuracy-dative.varimp$accuracy), 
   xlab="permutation variable importance")
}
}
\keyword{ classif }
