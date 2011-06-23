\name{cueCoding}
\alias{cueCoding}
\title{
  code a vector of cues as n-grams
}
\description{
  \code{cueCoding} codes a vector of cues into unigrams, bigrams, 
  \dots, n-grams, with unigrams as default. 
}
\usage{
cueCoding(cues = c("hello", "world"), maxn=1, adjacent=FALSE)
}
\arguments{
  \item{cues}{
    A  vector of cues (represented by strings) to be recoded as unigrams,
    bigrams, \dots, ngrams.
  }
  \item{maxn}{
    The longest n-gram to be encoded, by default \code{maxn=1}. 
  }
  \item{adjacent}{
    A logical indicating whether only adjacent bigrams should be
    included when \code{maxn=2}.  If \code{adjacent=TRUE} and
    \code{maxn!=2}, \code{maxn} is forced to 2.
  }
}
\value{
  A vector of cue n-grams, one for each word in the input
  vector \code{cues}. Each n-gram vector lists the constituent unigrams, 
  bigrams, etc., separated by underscores.
}
\references{
  Arppe, A. and Baayen, R. H. (in prep.). Statistical classification
  and principles of human learning.
}
\author{
Antti Arppe and Harald Baayen
}
\seealso{
  See also \code{\link{ndlClassify}, \link{ndlCuesOutcomes},
  \link{ndlVarimp}, \link{ndlCrossvalidate}}.
}
\examples{
# Cues from the \code{think} data: Person, Number, Register
cues <- c("First", "Plural", "hs95")
cueCoding(cues, maxn=1)
cueCoding(cues, maxn=2)
}
\keyword{ discriminative learning }
