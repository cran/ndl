\name{learnLegacy}
\alias{learnLegacy}
\title{
  Count cue-outcome co-occurrences needed to run the Danks equations.
}
\description{
  An internal function to count cue-outcome co-occurrences.
}
\usage{
learnLegacy(DFin,RemoveDuplicates,verbose)
}
\arguments{
  \item{DFin}{
    A dataframe, as defined in the documentation for \link{estimateWeights}.
  }
  \item{RemoveDuplicates}{
    A logical specifying whether multiple occurrences of a Cue in
    conjunction with an Outcome shall each
    be counted as a distinct occurrence of that Cue (\code{FALSE}), or
    only as a single occurrence (\code{TRUE}: default).
 }
 \item{verbose}{
   Display diagnostic messages or not.
 }
}
\details{
This function calls an Rcpp function of the same name to process the
data in the DFin data frame.
}
\value{
  A list of two matrices with cue-cue co-occurrences and cue-outcome
  co-occurrences.
}
\references{

Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix,
P. and Marelli, M., An amorphous model for morphological processing in
visual comprehension based on naive discriminative learning.
Psychological Review, 118, 438-482.

}
\author{
Cyrus Shaoul
}
\note{
  No temporary files are used.
}
\section{Acknowledgements}{
  Thanks to all the testers out there! Martijn, you know who you are.
}
\seealso{
  \code{\link{estimateActivations}, \link{ndlCuesOutcomes},
    \link{estimateWeights},
  \link{danks}, \link{plurals}, \link{serbian}}
}
\examples{
#None (internal function)
}
\keyword{ classif }
