\name{learn}
\alias{learn}
\title{
  Count cue-outcome co-occurences needed to run the Danks equations.
}
\description{
  An internal function to count cue-outcome co-occurrences.
}
\usage{
learn(data,RemoveDuplicates,verbose,MaxEvents,addBackground)
}
\arguments{
  \item{data}{
    A directory where the binary event data files are located.
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
 \item{MaxEvents}{
   The total number of events to learn from before stopping
   learning. Checked one time per compact data file.
 }     
 \item{addBackground}{
   Option to add background rates.
 }     
}
\details{
This function calls an Rcpp function of the same name to process the
data in the compact data format.
}
\value{
  A list of two matrices with cue-cue coocurrences and cue-outcome
  cooccurrences and a vector with background rates.
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
Thanks to all the testers!
}
\seealso{
  \code{\link{estimateActivations}, \link{ndlCuesOutcomes},
    \link{estimateWeightsCompact},
  \link{danks}, \link{plurals}, \link{serbian}}
}
\examples{
#None (internal function)
}
\keyword{ classif }
