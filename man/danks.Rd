\name{danks}
\alias{danks}
\docType{data}
\title{
Example data from Danks (2003), after Spellman (1996).
}
\description{
Data of Spellman (1996) used by Danks (2003) to illustrate the equilibrium 
equations for the Rescorla-Wagner model.  There are two liquids (red
and blue) that are potentially fertilizers, and the
experimental participant is given the rates at which
flowers bloom for the four possible conditions (no
liquid, red liquid, blue liquid, and both liquids). 
}
\usage{data(danks)}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{\code{Cues}}{A character vector specifying the cues. The pots in which the flowers are grown, and the color of the fertilizer.  Individual cues are separated by underscores.}
    \item{\code{Outcomes}}{A character vector specifying whether plants flowered (y or n).}
    \item{\code{Frequency}}{A numeric vector specifying the frequency of flowering.}
  }
}
\details{
For details, see Danks (2003: 112).
}
\source{
B. A. Spellman, (1996). Conditionalizing causality. In Shanks, D. R., Holyoak,
K. J., & Medin, D. L. (Eds.), Causal learning: the psychology of learning and
motivation, Vol. 34 (pp. 167-206). San Diego, CA: Academic Press.
}
\references{
D. Danks (2003), Equilibria of the Rescorla-Wagner model.  Journal of
Mathematical Psychology 47, 109-121.

B. A. Spellman, (1996). Conditionalizing causality. In Shanks, D. R., Holyoak,
K. J., & Medin, D. L. (Eds.), Causal learning: the psychology of learning and
motivation, Vol. 34 (pp. 167-206). San Diego, CA: Academic Press.
}
\examples{
data(danks)
estimateWeights(cuesOutcomes=danks)
}
\keyword{datasets}
