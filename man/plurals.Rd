\name{plurals}
\alias{plurals}
\docType{data}
\title{
  Artificial data set used to illustrate the Rescorla-Wagner equations 
  and naive discriminative learning.
}
\description{
  Data set with 10 English words of different (ad hoc) frequencies, each
  with a lexical meaning and a grammatical meaning.
}
\usage{data(plurals)}
\format{
  A data frame with 10 observations on the following 3 variables:
  \describe{
    \item{\code{WordForm}}{A character vector of word forms (cues).}
    \item{\code{Frequency}}{A numeric vector of frequencies.}
    \item{\code{Outcomes}}{A character vector of meanings (outcomes).
      Meanings are separated by underscores.  The \code{NIL} meaning is ignored.
    }
  }
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
\source{

Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix,
P. and Marelli, M., An amorphous model for morphological processing in
visual comprehension based on naive discriminative learning.
Psychological Review, 118, 438-482.

}
\references{

Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix,
P. and Marelli, M., An amorphous model for morphological processing in
visual comprehension based on naive discriminative learning.
Psychological Review, 118, 438-482.

}
\examples{
data(plurals)
plurals$Cues <- orthoCoding(plurals$WordForm, grams=1)
estimateWeights(cuesOutcomes=plurals)
}
\keyword{datasets}
