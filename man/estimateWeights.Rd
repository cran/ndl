\name{estimateWeights}
\alias{estimateWeights}
\title{
  Estimation of the association weights using the equilibrium
  equations of Danks (2003) for the Rescorla-Wagner equations.
}
\description{
  A function to estimate the weights (associative strengths) for
  cue-outcome pairs when learning is in equilibrium, using the
  equilibrium equations for the Rescorla-Wagner model of Danks (2003).
}
\usage{
estimateWeights(cuesOutcomes, removeDuplicates=TRUE, saveCounts=FALSE,
verbose=FALSE, trueCondProb=TRUE, addBackground=FALSE, hasUnicode=FALSE, \dots)
}
\arguments{
  \item{cuesOutcomes}{
    A data frame with three variables specifying frequency, cues, and
    outcomes, that may be created with \code{\link{ndlCuesOutcomes}} or
with the accessory script in the inst/scripts directory:
    \describe{
    \item{\code{Cues}}{A character vector specifying the cues. 
           When there is more than one cue, the cues should be 
           separated by underscores.}
    \item{\code{Outcomes}}{A character vector specifying the outcomes.  
           When there is more than one outcome, the outcomes should 
           be separated by underscores.}
    \item{\code{Frequency}}{A numeric vector specifying the frequency 
           with which a combination of cues and outcomes occurs.}
    }
  }
  \item{removeDuplicates}{
    A logical specifying whether multiple occurrences of a Cue in
    conjunction with an individual instance of an Outcome shall each
    be counted as a distinct occurrence of that Cue (\code{FALSE}: default), or
    only as a single occurrence (\code{TRUE}).
 }
  \item{saveCounts}{
    A logical specifying whether the co-occurrence matrices should be
    saved.  If set equal to \code{TRUE}, the files \code{coocCues.rda} and
    \code{coocCuesOutcomes.rda} will be saved in the current
    working directory.
  }
  \item{verbose}{
    If set to \code{TRUE}, display diagnostic messages.
  }
  \item{addBackground}{
    If you would like to add a background rate for all your cues and
    outcomes, but did not include an general environment cue to all your
    events, one will be added for you to the matrices, as
    specified in Danks (2003). If changed from the default (FALSE) to
    TRUE, background cues will be added. The name used for the background rates is "Environ", and
    will be included in the output weight matrix.
    
  }
  \item{trueCondProb}{
    The conditional probability calculations used will be those
    specified in Danks (2003). If changed from the default (TRUE) to
    FALSE, the normalization specified in Baayen, et al (2011) is used.
  }

  \item{hasUnicode}{
    A logical specifying whether to apply a UTF-8 to integer conversion
    to the names of the cues. This was implemented to solve issues with
    differences Unicode cue names. 
  }

  
  \item{\dots}{Control arguments to be passed along from \code{\link{ndlClassify}}
    and/or \code{\link{ndlCrossvalidate}}.
  }
}
\details{

  Using Rcpp, a C++ based implementation processes all of the data in
  RAM. The module will check the amount of RAM you have available in
  your system and warn you if the amount of RAM is insufficient
  to build your model.
  
  For examples of how the \code{cuesOutcomes} data frame should be
  structured, see the data sets \code{\link{danks}},
  \code{\link{plurals}}, and \code{\link{serbian}}. N.B. Empty
  \code{Cues} or \code{Outcomes} (effectively having \code{length =
  0}), e.g. \code{Cues} or \code{Outcomes} strings with an initial or
  final underscore or two immediately adjacent underscores, will
  result in an error.

}
\value{
  A matrix with cue-to-outcome association strengths. Rows are cues,
  and columns are outcomes.  Rows and columns are labeled. If
  addBackground=T, a row named "Environ" will be added to the output.
}
\references{

Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix,
P. and Marelli, M. (2011), An amorphous model for morphological processing in
visual comprehension based on naive discriminative learning.
Psychological Review, 118, 438-482.

}
\author{
Cyrus Shaoul, R. H. Baayen and Petar Milin, with contributions from Antti Arppe and
Peter Hendrix.
}
\note{
Add a note here.

}
\section{Acknowledgements}{
  The assistance of Uwe Ligges in getting the C function \code{cooc}
  to work within the R framework is greatly appreciated. This C function
  was removed in version 0.2.0 and replaced with the C++ function by
  Cyrus Shaoul.
}
\seealso{
  \code{\link{estimateActivations}, \link{ndlCuesOutcomes},
   \link{danks}, \link{plurals}, \link{serbian}}
}
\examples{
  data(danks)
  estimateWeights(cuesOutcomes=danks)

  data(plurals)
  plurals$Cues <- orthoCoding(plurals$WordForm, grams=1)
  round(estimateWeights(cuesOutcomes=plurals),2)
  
  data(serbian)
  serbian$Cues <- orthoCoding(serbian$WordForm, grams=2)
  serbian$Outcomes <- serbian$LemmaCase
  sw <- estimateWeights(cuesOutcomes=serbian)
  round(sw[1:5,1:6],2)
}
\keyword{ classif }
