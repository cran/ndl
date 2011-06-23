\name{numbers}
\alias{numbers}
\docType{data}
\title{
  Example data illustrating the Rescorla-Wagner equations as applied 
  to numerical cognition by Ramscar et al. (2011).
}
\description{
  The data used in simulation 3 of Ramscar et al. (2011) on numerical
  cognition.  
}
\usage{data(lexample)}
\format{
  A data frame with 10 observations on the following 3 variables.
  \describe{
    \item{\code{Cues}}{A character vector specifying cues for
      quantities, separated by underscores.}
    \item{\code{Frequency}}{The frequencies with which the numbers
      appear in the COCA corpus.}
    \item{\code{Outcomes}}{A character vector specifying numerical
      outcomes associated with the input quantities.}
  }
}
\details{
  The cues represent learning trials with objects of the same size,
  shape and color. The numeric cues represent the presence of at
  least one subset of the specified size.  The cues \code{exactlyn} 
  represent the presence of exactly \code{n} objects. We are indebted 
  to Michael Ramscar to making this data set available for inclusion 
  in the package.
}
\references{
Michael Ramscar, Melody Dye, Hanna Muenke Popick & Fiona O'Donnell-McCarthy
(2011), The Right Words or Les Mots Justes?  Why Changing the Way We Speak to
Children Can Help Them Learn Numbers Faster.  Manuscript,  Department of
Psychology, Stanford University.
}
\examples{
data(numbers)

traceCues=c( "exactly1", "exactly2", "exactly3", "exactly4",
"exactly5", "exactly6", "exactly7", "exactly10", "exactly15")
traceOutcomes=c("1", "2", "3", "4", "5", "6", "7", "10", "15")

ylimit=c(0,1)
par(mfrow=c(3,3),mar=c(4,4,1,1))

for (i in 1:length(traceCues)){
  numbers.rw = RescorlaWagner(numbers, nruns=1,
    traceCue=traceCues[i],traceOutcome=traceOutcomes[i])
  plot(numbers.rw, ylimit=ylimit)
  mtext(paste(traceCues[i], " - ", traceOutcomes[i], sep=""), 
    side=3, line=-1, cex=0.7)
}
par(mfrow=c(1,1))

}
\keyword{datasets}
