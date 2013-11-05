\name{estimateWeightsCompact}
\alias{estimateWeightsCompact}
\title{
  Estimation of the association weights using the equilibrium
  equations of Danks (2003) for the Rescorla-Wagner equations using
  a compact binary event file.
}
\description{
  A function to estimate the weights (associative strengths) for
  cue-outcome pairs when learning is in equilibrium, using the
  equilibrium equations for the Rescorla-Wagner model of Danks (2003) using
  a compact binary event file.
}
\usage{
estimateWeightsCompact(datasource, removeDuplicates=TRUE,
saveCounts=FALSE, verbose=FALSE, MaxEvents=100000000000000,
trueCondProb=TRUE, addBackground=FALSE, \dots)
}
\arguments{
  \item{datasource}{
    A data source that is linked with a file naming convention. If the
    datasource is the string "source", then the following resources will
    need to exist in the current working directory:
    \describe{
    \item{\code{source.events}}{A directory that contains binary event
      files in the format specified in learn.module.cpp}
    \item{\code{source.cues}}{A text file that contains the full list of
      cues in the first column, and separated by a tab, the CueID for
      each cue. Must be encoded in UTF8.}
    \item{\code{source.outcomes}}{A text file that contains the full list of
      outcomes in the first column, and separated by a tab, the OutcomeID for
      each outcome. Must be encoded in UTF8.}
    }
  }
  \item{removeDuplicates}{
    A logical specifying whether multiple occurrences of a Cue in
    conjunction with an Outcome shall each
    be counted as a distinct occurrence of that Cue (\code{FALSE}), or
    only as a single occurrence (\code{TRUE}: default).
 }
  \item{saveCounts}{
    A logical specifying whether the co-occurrence matrices should be
    saved.  If set equal to \code{TRUE}, the files \code{coocCues.rda} and
    \code{coocCuesOutcomes.rda} will be saved in the current
    workspace. Default is FALSE.
  }
  \item{verbose}{
    If set to \code{TRUE}, display diagnostic messages.
  }
  \item{MaxEvents}{
    If changed from the default value, the learning algorithm will stop
    learning after using the first N events in the training data. This
    actually number of events used may be slightly higher than the
    number specified.
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
  \item{\dots}{Control arguments to be passed along from \code{\link{ndlClassify}}
    and/or \code{\link{ndlCrossvalidate}}.
  }
}
\details{

  Using Rcpp, a C++ based implementation processes all of the data
  RAM. The module will check the amount of RAM you have available in
  your system and warn you of RAM is insufficient
  to build your model.

}
\value{
  A matrix with cue-to-outcome association strengths. Rows are cues,
  and columns are outcomes.  Rows and columns are labeled. If
  addBackground=T, a row named "Environ" will be added to the output.
}
\references{

Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix,
P. and Marelli, M., (2011) An amorphous model for morphological processing in
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
Thanks to all the beta testers of the ndl package.
}
\seealso{
  \code{\link{estimateActivations}}
}
\examples{
  message("This module requires data in a non-portable format to
demonstrate how it works.")
}
\keyword{ discriminative learning }
