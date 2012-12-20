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
cooccurrenceCues(cuesOutcomes, duplicates=FALSE, method="C",
   max.cues=20000, max.characters=20000, max.lines=500000)
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
  \item{max.cues}{
    An integer specifying the overall number of distinct cues (and
    outcomes); by default set to \code{=20000}.
  }
  \item{max.characters}{
    An integer specifying the maximum character length of cues per
    each occurrence, i.e. line in \code{cuesOutcomes}, by default set to
    \code{=20000}.
  }
  \item{max.lines}{An integer specifying the maximum number of
    lines in \code{cuesOutcomes}; by default set to \code{=500000}.
  }
}

\details{

  The real work is done by C code (\code{cooc}), when
  \code{method="C"} (default), or an AWK script outside this function,
  when \code{method="awk"}.  Otherwise, a slow R implementation can be
  used (\code{method="R"}), in which case the model may not scale up.

  With the fastest C code version, the default maximum length of
  individual cue combinations is 20,000 characters (including
  underscores), the default maximum number of unique Cues or unique
  Outcomes is 20,000, and the default number of input lines in
  \code{cuesOutcomes} is 500,000. These limits can be adjusted with
  the appropriate arguments; however, that should only be done with
  the utmost care. The AWK script does not have these restrictions
  concerning its input, but will only work in UNIX-like operating
  systems, i.e. not in Windows.

  With the C and AWK versions, several temporary text files are
  written to and erased from the current working directory (see
  \code{\link{estimateWeights}} for details).

  A warning is given if the Cues contain any space characters, as
  these may be difficult to identify visually in the resultant matrix.

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
  learning.  Psychological Review, 118, 438-482.
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
