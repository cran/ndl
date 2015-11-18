\name{serbianUniCyr}
\alias{serbianUniCyr}
\docType{data}
\title{
  Serbian case inflected nouns (in Cyrillic Unicode).
}
\description{
  3240 case-inflected Serbian nouns and their frequencies, for 270 different
  masculine, feminine and neuter noun lemmas, written using the Cyrillic
  alphabet and encoded in UTF-8.
}
\usage{data(serbianUniCyr)}
\format{
  A data frame with 3240 observations on the following 3 variables:
  \describe{
    \item{\code{WordForm}}{A character vector specifying the inflected
      word forms encoded in UTF-8.}
    \item{\code{LemmaCase}}{A character vector specifying lemma (meaning), case, and number.}
    \item{\code{Frequency}}{A numeric vector specifying the frequency of each word form.}
  }
}

\details{
  Frequencies were taken from the Frequency Dictionary of Contemporary
  Serbian Language (Kostic, 1999).  The 270 lemmas comprise the set of
  nouns for which each different case form appears at least once in this
  resource.
}

\source{
Kostic, D. (1999). Frekvencijski recnik savremenog srpskog jezika (Frequency
Dictionary of Contemporary Serbian Language). Institute for Experimental
Phonetics and Speech Pathology & Laboratory of Experimental Psychology,
University of Belgrade, Serbia.
}
\references{
Kostic, D. (1999). Frekvencijski recnik savremenog srpskog jezika (Frequency
Dictionary of Contemporary Serbian Language). Institute for Experimental
Phonetics and Speech Pathology & Laboratory of Experimental Psychology,
University of Belgrade, Serbia.

Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli, M.
(2011), An amorphous model for morphological processing in visual comprehension
based on naive discriminative learning. Psychological Review, 118, 438-482. 

}
\seealso{
See also \code{\link{serbian}, \link{serbianLex}, \link{estimateActivations}}.
}
\examples{
\dontrun{
data(serbianUniCyr)
serbianUniCyr$Cues <- orthoCoding(serbianUniCyr$WordForm, grams=2)
serbianUniCyr$Outcomes <- serbianUniCyr$LemmaCase
sw <- estimateWeights(cuesOutcomes=serbianUniCyr)
sw[1:5,1:5]
desiredItems <- unique(serbianUniCyr["Cues"])
desiredItems$Outcomes=""
activations <- estimateActivations(desiredItems, sw)$activationMatrix
rownames(activations) <- unique(serbianUniCyr[["WordForm"]])
activations[1:5,1:6]
}
}
\keyword{datasets}
