\name{cooccurrenceCues}
\alias{cooccurrenceCues}
\title{
  compute co-occurrence matrix 
}
\description{
  calculation of the co-occurrence matrix of cues, see (37) in 
  Baayen et al. 2011 for an example
}
\usage{
cooccurrenceCues(cuesOutcomes, duplicates=FALSE, method="C")
}
\arguments{
  \item{cuesOutcomes}{
    A data frame with three character vector variables specifying
    frequency, cues, and outcomes; see \code{\link{estimateWeights}}.
  }
 \item{duplicates}{
    A logical specifying whether multiple occurrences of a Cue in
    conjunction with an individual instance of an Outcome shall each
    be counted as a distinct occurrence of that Cue (\code{TRUE}), or
    only as a single occurrence (\code{FALSE}: default).
 }
  \item{method}{
    A character string indicating whether cooccurrence matrices should
    be calculated with C using \code{cooc} (\code{"C"}: default), with
    an AWK script (\code{"awk"}), or within R (\code{"R"}).
  }
}

\details{

  The real work is done by C code (\code{cooc}), when
  \code{method="C"} (default), or an AWK script outside this function,
  when \code{method="awk"}.  Otherwise, a slow R implementation can be
  used (\code{method="R"}), in which case the model may not scale up.

  With the fastest C code version, the maximum length of individual
  cue combinations is 20,000 characters (including underscores) and
  the maximum number of unique Cues or unique Outcomes is 20,000. The
  AWK script does not have these restrictions concerning its input,
  but will only work in UNIX-like operating systems, i.e. not in
  Windows.

  With the C and AWK versions, several temporary text files are
  written to and erased from the current working directory (see
  \code{\link{estimateWeights}} for details).

}
\value{
  A square symmetric matrix with the co-occurrence frequencies of the
  cues.
}
\references{
  Danks, D. (2003).  Equilibria of the Rescorla-Wagner model, Journal of
  Mathematical Psychology, 47, 109-121.

  Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and
  Hendrix, P. and Marelli, M., An amorphous model for morphological
  processing in visual comprehension based on naive discriminative
  learning.  Psychological Review, in press.
}
\author{
R. H. Baayen and Petar Milin, with contributions from Antti Arppe and
Peter Hendrix.
}
\seealso{
\code{\link{estimateWeights}, \link{cooccurrenceCuesOutcomes}}
}
%\examples{
%}
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
