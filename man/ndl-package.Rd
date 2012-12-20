\name{ndl-package}
\alias{ndl-package}
\alias{ndl}
\docType{package}
\title{
  Naive discriminative learning: an implementation in R
}
\description{

  Naive discriminative learning implements classification models based
  on the Rescorla-Wagner equations and the equilibrium equations of
  the Rescorla-Wagner equations.  This package provides three kinds of
  functionality: (1) discriminative learning based directly on the
  Rescorla-Wagner equations, (2) a function implementing the naive
  discriminative reader, and a model for silent (single-word) reading,
  and (3) a classifier based on the equilibrium equations.  The
  functions and datasets for the naive discriminative reader model
  make it possible to replicate the simulation results for Experiment
  1 of Baayen et al. (2011).  The classifier is provided to allow for
  comparisons between machine learning (svm, TiMBL, glm, random
  forests, etc.) and human learning.  Compared to standard
  classification algorithms, naive discriminative learning may overfit
  the data, albeit gracefully.

}
\details{
\tabular{ll}{
Package: \tab ndl\cr
Type: \tab Package\cr
Version: \tab 0.1.6\cr
Date: \tab 2012-12-20\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

For using the Rescorla-Wagner equations, see the functions
\code{\link{RescorlaWagner}} and \code{\link{plot.RescorlaWagner}}, as
well as the data sets \code{\link{danks}}, \code{\link{numbers}} (data
courtesy of Michael Ramscar), and \code{\link{lexample}} (an example
discussed in Baayen et al. 2011).

The functions for the naive discriminative reader (at the user level)
are \code{\link{estimateWeights}} and
\code{\link{estimateActivations}}. The relevant data sets are
\code{\link{serbian}} and \code{\link{serbianLex}}.  The examples for
\code{\link{serbianLex}} present the full simulation for Experiment 1
of Baayen et al. (2011).

Key functionality for the user is provided by the functions
\code{\link{orthoCoding}}, \code{\link{estimateWeights}}, and
\code{estimateActivations}.  \code{orthoCoding} calculates n-grams for
character strings, to be used as cues.  It is assumed that meanings
(possibly separated by underscores) are available as outcomes.  The
frequency with which each (unique) combination of cues and outcomes
occurs should also be available.  See for example input data sets,
\code{\link{danks}}, \code{\link{plurals}}, and \code{\link{serbian}}.
The function \code{estimateWeights} estimates the association
strengths of cues to outcomes, using the equilibrium equations of
Danks (2003).  The function \code{estimateActivations} estimates the
activations of outcomes (meanings) given cues (n-grams).  The
functions \code{\link{cooccurrenceCues}} and
\code{\link{cooccurrenceCuesOutcomes}} are low-level functions that
call on a C function \code{cooc} that computes the conditional
co-occurrence matrices required for the equilibrium equations.

The key function for naive discriminative classification is
\code{\link{ndlClassify}}; see data sets \code{\link{think}} and
\code{\link{dative}} for examples.

} 
\author{
  Antti Arppe, Petar Milin and R. Harald Baayen, with contributions of
  Peter Hendrix.

Maintainer: Antti Arppe <antti.arppe@iki.fi>
}
\references{
  Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and
  Hendrix, P. and Marelli, M., An amorphous model for morphological
  processing in visual comprehension based on naive discriminative
  learning.  Psychological Review, 118, 438-482.

  Baayen, R. H. (2011) Corpus linguistics and naive discriminative
  learning.  Brazilian Journal of Applied Linguistics, 11, 295-328.

  Arppe, A. and Baayen, R. H. (in prep.) Statistical classification
  and principles of human learning.
}
\keyword{ naive discriminative learning }
\examples{
\dontrun{
# Rescorla-Wagner 
data(lexample)

lexample$Cues <- orthoCoding(lexample$Word, maxn=1)
par(mfrow=c(2,2))
lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="h",
   traceOutcome="hand")
plot(lexample.rw)
mtext("h - hand", 3, 1)

data(numbers)
     
traceCues <- c( "exactly1", "exactly2", "exactly3", "exactly4", "exactly5",
   "exactly6", "exactly7", "exactly10", "exactly15")
traceOutcomes <- c("1", "2", "3", "4", "5", "6", "7", "10", "15")

ylimit <- c(0,1)
par(mfrow=c(3,3), mar=c(4,4,1,1))

for (i in 1:length(traceCues)) {
  numbers.rw <- RescorlaWagner(numbers, nruns=1, traceCue=traceCues[i],
     traceOutcome=traceOutcomes[i])
  plot(numbers.rw, ylimit=ylimit)
  mtext(paste(traceCues[i], " - ", traceOutcomes[i], sep=""), side=3, line=-1,
    cex=0.7)
}
par(mfrow=c(1,1))


# naive discriminative learning (for complete example, see serbianLex)
data(serbian)
serbian$Cues <- orthoCoding(serbian$WordForm, maxn=2)
serbian$Outcomes <- serbian$LemmaCase
sw <- estimateWeights(cuesOutcomes=serbian)
activations <- estimateActivations(unique(serbian["Cues"]), sw)$activationMatrix
rownames(activations) <- unique(serbian[["WordForm"]])

syntax <- c("acc", "dat", "gen", "ins", "loc", "nom", "Pl",  "Sg") 
activations2 <- activations[,!is.element(colnames(activations), syntax)]
head(rownames(activations2),50)
head(colnames(activations2),8)

image(activations2, xlab="word forms", ylab="meanings", xaxt="n", yaxt="n")
mtext(c("yena", "...", "zvuke"), side=1, line=1, at=c(0, 0.5, 1), 
  adj=c(0,0,1))
mtext(c("yena", "...", "zvuk"), side=2, line=1, at=c(0, 0.5, 1), 
  adj=c(0,0,1))

# naive discriminative classification
data(think)
think.ndl <- ndlClassify(Lexeme ~ Person + Number + Agent + Patient + Register,
   data=think)
summary(think.ndl)
plot(think.ndl, values="weights", type="hist", panes="multiple")
plot(think.ndl, values="probabilities", type="density")
}
}
