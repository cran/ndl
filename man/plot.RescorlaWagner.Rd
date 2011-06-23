\name{plot.RescorlaWagner}
\alias{plot.RescorlaWagner}
\title{
  Plot function for the output of \code{RescorlaWagner}.
}
\description{
  This function graphs the Rescorla-Wagner weights for a cue-outcome pair
  against learning time. 
}
\usage{
\method{plot}{RescorlaWagner}(x, asymptote=TRUE, xlab="t", ylab="weight", ylimit=NA, \dots)
}
\arguments{
  \item{x}{
     A object of the class \code{"RescorlaWagner"} produced by
     \code{RescorlaWagner}, consisting of a list including estimated
     weights for the incremental and equilibrium stages.
  }
  \item{asymptote}{ 
     A logical specifying whether the equilibrium asymptotic 
     weight should be added to the plot.
  }
  \item{xlab}{ 
     Label for x-axis, by default \code{"t"}.
  }
  \item{ylab}{ 
     Label for y-axis, by default \code{"weight"}.
  }
  \item{ylimit}{ 
     The range of values to be displayed on the Y axis. By default,
     this will be determined from the data itself.
  }
  \item{\dots}{ 
     Arguments to be passed to methods, such as graphical
     parameters (see \code{link{par}}).
  }
}
\value{
  A plot is produced on the graphics device.
}
\references{
Danks, D. (2003). Equilibria of the Rescorla-Wagner model. Journal of
Mathematical Psychology, 47 (2), 109-121.

Rescorla, R. A., & Wagner, A. R. (1972). A theory of Pavlovian
conditioning: Variations in the effectiveness of reinforcement and
nonreinforcement. In Black, A. H., & Prokasy, W. F. (Eds.),
Classical conditioning II: Current research and theory (pp. 64-99).
New York: Appleton-Century-Crofts.
}
\author{
R. H. Baayen and Antti Arppe
}
\seealso{
\code{\link{RescorlaWagner}}, \code{\link{orthoCoding}}
}
\examples{

data(lexample)
lexample$Cues <- orthoCoding(lexample$Word, maxn=1)
lexample.rw <- RescorlaWagner(lexample, nruns=25, 
   traceCue="h", traceOutcome="hand")
plot(lexample.rw)
mtext("h - hand", 3, 1)

# Full example

\dontrun{
par(mfrow=c(2,2))
lexample.rw <- RescorlaWagner(lexample, nruns=25, 
   traceCue="h", traceOutcome="hand")
plot(lexample.rw)
mtext("h - hand", 3, 1)

lexample.rw <- RescorlaWagner(lexample, nruns=25, 
   traceCue="s", traceOutcome="plural")
plot(lexample.rw)
mtext("s - plural", 3, 1)

lexample.rw <- RescorlaWagner(lexample, nruns=25, 
   traceCue="a", traceOutcome="as")
plot(lexample.rw)
mtext("a - as", 3, 1)

lexample.rw <- RescorlaWagner(lexample, nruns=25, 
   traceCue="s", traceOutcome="as")
plot(lexample.rw)
mtext("s - as", 3, 1)
par(mfrow=c(1,1))
}
}
\keyword{ classif }
