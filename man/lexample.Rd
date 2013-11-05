\name{lexample}
\alias{lexample}
\docType{data}
\title{
  Lexical example data illustrating the Rescorla-Wagner equations  
}
\description{
  Ten monomorphemic and inflected English words with fictive frequencies,
  and meanings.
}
\usage{data(lexample)}
\format{
  A data frame with 10 observations on the following 3 variables:
  \describe{
    \item{\code{Word}}{A character vector specifying word forms}
    \item{\code{Frequency}}{A numeric vector with the -- fictive --
         frequencies of occurrence of the words}
    \item{\code{Outcomes}}{A character vector specifying the meaning 
         components of the words, separated by underscores}
  }
}
\details{
This example lexicon is used in Baayen et al. (2011) (table 8, figure 4)
to illustrate the Rescorla-Wagner equations.
}
\references{

  Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and
  Marelli, M.  (2011), An amorphous model for morphological processing
  in visual comprehension based on naive discriminative
  learning. Psychological Review, 118, 438-482.

}
\seealso{
  \code{\link{RescorlaWagner}, \link{orthoCoding}}
}
\examples{
data(lexample)
lexample$Cues <- orthoCoding(lexample$Word, grams=1)
par(mfrow=c(2,2))
lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="h",traceOutcome="hand")
plot(lexample.rw)
mtext("h - hand", 3, 1)

lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="s",traceOutcome="plural")
plot(lexample.rw)
mtext("s - plural", 3, 1)

lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="a",traceOutcome="as")
plot(lexample.rw)
mtext("a - as", 3, 1)

lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="s",traceOutcome="as")
plot(lexample.rw)
mtext("s - as", 3, 1)
par(mfrow=c(1,1))
}
\keyword{datasets}
