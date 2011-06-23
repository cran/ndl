\name{ndlCuesOutcomes}
\alias{ndlCuesOutcomes}
\title{Creation of dataframe for Naive Discriminative Learning from formula specification
  
}
\description{

  \code{ndlCuesOutcomes} creates a dataframe for fitting a naive
  discriminative classification model with \code{ndlClassify}, using
  the specified formula and provided data.

}
\usage{
ndlCuesOutcomes(formula, data, frequency=NA, 
  numeric2discrete=function(x) Hmisc::cut2(x,g=g.numeric), g.numeric=2,
  check.values=TRUE, ignore.absent=NULL, variable.value.separator="", \dots)
}
\arguments{
  \item{formula}{
    An object of class \code{formula} (or one that can be coerced to
    that class): a symbolic description of the model to be fitted.  
  }
  \item{data}{ 
    A data frame containing the variables in the model.  
  }
  \item{frequency}{ 
    A numeric vector (or the name of a column in the input data frame)
    with the frequencies of the exemplars. If absent, each exemplar is
    assigned a frequency equal to 1.
  }
  \item{numeric2discrete}{
    A function to transform a continuous numeric predictor into a
    number of discrete classes, by default \code{cut2} from the
    \code{Hmisc} package. If set to NULL, each value of each
    numeric predictor will be treated as a discrete class of its own.
  }
  \item{g.numeric}{
    A parameter to be passed to the numeric2discrete function
    (parameter \code{g} for \code{Hmisc::cut2(..., g=g.numeric, ...)},
    or a user-defined function), determining the desired number of
    discrete categories for each numeric predictor; by default equal
    to 2.
  }
  \item{check.values}{
   A logical specifying whether underscores \sQuote{_} in predictor
   values should substituted with periods \sQuote{.}; if \code{=FALSE},
   the predictor values will be only checked and an error message will
   result if any underscores are discovered.
  }
  \item{ignore.absent}{
   A character vector specifying one or more values for any predictor
   (e.g. \code{NIL}, \code{None} and/or \code{Other}) which may be
   considered truely \code{absent} cues in terms of the
   Rescorla-Wagner equations; by default set to \code{NULL} so that
   all values of all predictors will be treated as \code{present}
   cues.
  }
  \item{variable.value.separator}{ 
    A character string which will separate variable names from
    variable values in their combination as cue values; by default an
    empty character string (\code{=""}).
  }
  \item{\dots}{
   Control arguments to be passed along to \code{estimateWeights}.
  }
}
\details{
   Creates a dataframe to be used for fitting a Naive Discriminatory
   Learning classifier model.
}
\value{
  A dataframe with the following columns:
\describe{
  \item{\code{Frequency}}{Frequency with which the specific Cues and Outcomes co-occur.}
  \item{\code{Cues}}{A character vector of sets of Cues per instance, with Cues separated by underscore \sQuote{_}.}
  \item{\code{Outcomes}}{A character vector of Outcomes per instance.}
}
}
\references{
Arppe, A. and Baayen, R. H. (in prep.) Statistical modeling and the
principles of human learning.
}
\author{
R. H. Baayen and Antti Arppe
}
\seealso{
\code{\link{cueCoding}, \link{ndlClassify}}
}
\examples{
data(think)
set.seed(314)
think <- think[sample(1:nrow(think),500),]
think.CuesOutcomes <- ndlCuesOutcomes(Lexeme ~ (Person * Number * Agent) + Register, 
data=think)
head(think.CuesOutcomes)

\dontrun{
data(dative)
dative.cuesOutcomes <- ndlCuesOutcomes(RealizationOfRecipient ~ LengthOfRecipient +
   LengthOfTheme, data=dative, numeric2discrete=NULL)
table(dative.cuesOutcomes$Cues)

dative.cuesOutcomes1 <- ndlCuesOutcomes(RealizationOfRecipient ~ LengthOfRecipient +
   LengthOfTheme, data=dative)
table(dative.cuesOutcomes1$Cues)

dative.cuesOutcomes2 <- ndlCuesOutcomes(RealizationOfRecipient ~ LengthOfRecipient +
   LengthOfTheme, data=dative, g.numeric=3)
table(dative.cuesOutcomes2$Cues)

}
}
\keyword{ classif }
