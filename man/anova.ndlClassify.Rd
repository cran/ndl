\name{anova.ndlClassify}
\alias{anova.ndlClassify}
\alias{anova.ndlClassifylist}

\title{Analysis of Model Fit for Naive Discriminatory Reader Models
}

\description{
  Compute an analysis of individual variable contributions or model
  comparisons for one or more Naive Discriminatory Reader model
  fits.
}

\usage{
 \method{anova}{ndlClassify}(object, \dots, statistic = "deviance", test = "Chisq")
}

\arguments{
\item{object, \dots}{
      Object(s) of class \code{"ndlClassify"}, typically the result of a
      call to \code{ndlClassify}, or a list of \code{objects} for the
      \code{ndlClassifylist} method.
     }
\item{statistic}{
      A character string specifying the statistic describing the fit
      that is to be compared, by default \code{deviance}, which is
      obtained from the object(s).
     }
\item{test}{
      A character string, determining the statistical method by which
      the significance of the comparison are done, by default the
      Chi-squared test (\code{Chisq}).
     }
}

\details{
     Currently, comparison of the terms of a single model or multiple
     models is only implemented based on the \code{deviance}
     statistic.

     Specifying a single object gives a sequential analysis of deviance
     table for that fit.  That is, the reductions in the residual
     deviance as each term of the formula is added in turn are given in
     as the rows of a table, plus the residual deviances themselves.

     If more than one object is specified, the table has a row for the
     residual degrees of freedom and deviance for each model.  For all
     but the first model, the change in degrees of freedom and deviance
     is also given. (This only makes statistical sense if the models
     are nested.)  It is conventional to list the models from smallest
     to largest, but this is up to the user.

     The table will contain test statistics (and P values) comparing
     the reduction in deviance for the row to the
     residuals.  Only a comparison of models or
     contributions of their components by the chi-squared test has
     been implemented.

     The comparison between two or more models by \code{anova} or
     \code{anova.ndlClassifylist} will only be valid if they are
     fitted to the same dataset. If \code{anova.ndlClassifylist}
     detects this, it will stop and report an error.
}

\value{
     An object of class \code{"anova"} inheriting from class
     \code{"data.frame"}.
}

\references{
Arppe, A. and Baayen, R. H. (in prep.) Statistical classification and
principles of human learning.
}

\author{
Antti Arppe
}

\seealso{
\code{\link{ndlClassify}}
}

\examples{

data(think)
set.seed(314)
think <- think[sample(1:nrow(think),500),]

think.ndl1 <- ndlClassify(Lexeme ~ Agent * Person, data=think)
anova(think.ndl1)

think.ndl2 <- ndlClassify(Lexeme ~ Agent * Person + Patient, data=think)
anova(think.ndl1, think.ndl2)

}
\keyword{ classif }
