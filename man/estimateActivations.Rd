\name{estimateActivations}
\alias{estimateActivations}
\title{
  Estimation of the activations of outcomes (meanings) 
}
\description{
  \code{estimateActivations} is used to estimate the activations for
  outcomes (meanings) using the equilibrium association strengths
  (weights) for the Rescorla-Wagner model.
}
\usage{
estimateActivations(cuesOutcomes, weightMatrix, unique=FALSE, \dots)
}
\arguments{
  \item{cuesOutcomes}{
    A data frame with three variables specifying frequency, cues, and outcomes:
    \describe{
    \item{\code{Cues}}{A character vector specifying the cues. When there is more than one cue, the cues should be separated by underscores.}
    \item{\code{Outcomes}}{A character vector specifying the outcomes.  When there is more than one outcome, the outcomes should be separated by underscores.}
    \item{\code{Frequency}}{A numeric vector specifying the frequency with which a combination of cues and outcomes occurs.}
    }
  }
  \item{weightMatrix}{
    A numeric matrix with as dimensions the number of cues
    (horizontal) and number of outcomes (vertical).  Rows and columns
    should be labeled with cues and outcomes.
  }
  \item{unique}{
    A logical that, if \code{=TRUE}, removes duplicate rows from the activation matrix.
  }
  \item{\dots}{
    Control arguments to be passed along from
    \code{\link{ndlClassify}} and/or \code{\link{ndlCrossvalidate}}.
  }
}
\details{

The activation of an outcome is defined as the sum of the weights on
the incoming links from active cues.  When the input (the \code{Cues}
in \code{cuesOutcomes}) contain elements that are not present in the
rownames of the \code{weightMatrix}, such new cues are added to the
\code{weightMatrix} with zero entries. The set of exemplars in
\code{cuesOutcomes} may contain rows with identical cue sets but
different outcome sets. Consequently, for such rows, identical vectors
of activations of outcomes are generated.  In the activation matrix
returned by \code{estimateActivations}, such duplicate entries are
removed.

For examples of how the \code{cuesOutcomes} data frame should be
structured, see the data sets \code{\link{danks}},
\code{\link{plurals}}, and \code{\link{serbian}}.  For examples of how
the \code{weightMatrix} should be structured, see the corresponding
output of \code{\link{estimateWeights}}.

}
\value{
A list with the following components:
\describe{
  \item{\code{activationMatrix}}{A matrix with as dimensions, for rows, the number of exemplars
  (by-row cue sets, typically word forms), and for columns, the number
  of unique outcomes (meanings), specifying the activation of a
  meaning given the cues in the input for a given exemplar.}
  \item{\code{newCues}}{A vector of cues encountered in \code{cuesOutcomes} which were not
  present in \code{weightMatrix}.}
  \item{\dots}{
    Control arguments to be passed along from \code{\link{ndlClassify}},
    and/or \code{\link{ndlCrossvalidate}}.}
}
}
\references{

Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix,
P. and Marelli, M., An amorphous model for morphological processing in
visual comprehension based on naive discriminative learning.
Psychological Review, 118, 438-482.

}
\author{
R. H. Baayen & Antti Arppe
}
%%\note{
%%  ~~further notes~~
%%}
\seealso{
\code{\link{estimateWeights}, \link{danks}, \link{plurals}, \link{serbian}}
}
\examples{
  data(serbian)
  serbian$Cues <- orthoCoding(serbian$WordForm, grams=2)
  serbian$Outcomes <- serbian$LemmaCase
  sw <- estimateWeights(cuesOutcomes=serbian)
  sw[1:5,1:6]
  activations <- estimateActivations(unique(serbian["Cues"]), sw)$activationMatrix
  rownames(activations) <- unique(serbian[["WordForm"]])
  activations[1:5,1:6]

  syntax <- c("acc", "dat", "gen", "ins", "loc", "nom", "Pl", "Sg") 
  activations2 <- activations[,!is.element(colnames(activations),syntax)]
  head(rownames(activations2), 50)
  head(colnames(activations2), 8)
  image(activations2, xlab="word forms", ylab="meanings", xaxt="n", yaxt="n")
  mtext(c("yena", "...", "zvuke"), side=1, line=1, at=c(0, 0.5, 1), adj=c(0,0,1))
  mtext(c("yena", "...", "zvuk"), side=2, line=1, at=c(0, 0.5, 1), adj=c(0,0,1))
}
\keyword{ classif }
