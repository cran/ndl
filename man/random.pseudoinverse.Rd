\name{random.pseudoinverse}
\alias{random.pseudoinverse}
\title{
  Calculate an approximation of the pseudoinverse of a matrix.
}
\description{
  An internal function that uses an approximation of the SVD using the
  first k singular values of A to calculate the pseudo-inverse. Only
  used when the cue-cue matrix contains more than 20,000 cues.
}
\usage{
random.pseudoinverse(m, verbose=F, k = 0)
}
\arguments{
  \item{m}{
    A matrix.
  }
  \item{k}{
    If k = 0, the default, k will be set to the size of 3/4 of the
    singular values. If not, the k-rank approximation will be calculated.
 }
 \item{verbose}{
   Display diagnostic messages or not.
 }     
}
\details{
This idea was proposed by Gunnar Martinsson
Associate Professor and Director of Graduate Studies
Department of Applied Mathematics, University of Colorado at Boulder
http://amath.colorado.edu/faculty/martinss/
And with ideas from:
Yoel Shkolnisky and his Out-of-Core SVD code:
https://sites.google.com/site/yoelshkolnisky/software
}
\value{
 The approximate pseudoinverse of the input matrix
}
\references{

"Finding structure with randomness: Probabilistic algorithms for
constructing approximate matrix decompositions"
Nathan Halko, Per-Gunnar Martinsson, Joel A. Tropp
http://arxiv.org/abs/0909.4061
}
\author{
Cyrus Shaoul
}
\note{
  No temporary files are used.
}
\section{Acknowledgements}{
  Thanks to Gunnar for his help with this!
}
\seealso{
  \link{estimateWeights},  \link{estimateWeightsCompact},
}
\examples{
#None (internal function)
}
\keyword{ classif }
