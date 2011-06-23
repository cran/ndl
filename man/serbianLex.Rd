\name{serbianLex}
\alias{serbianLex}
\docType{data}
\title{
  Serbian lexicon with 1187 prime-target pairs.
}
\description{
  The 1187 prime-target pairs and their lexical properties used in the
  simulation study of Experiment 1 of Baayen et al. (2011).
}
\usage{data(serbianLex)}
\format{
  A data frame with 1187 observations on the following 14 variables:
  \describe{
    \item{\code{Target}}{A factor specifying the target noun form}
    \item{\code{Prime}}{A factor specifying the prime noun form}
    \item{\code{PrimeLemma}}{A factor specifying the lemma of the prime}
    \item{\code{TargetLemma}}{A factor specifying the target lemma}
    \item{\code{Length}}{A numeric vector with the length in letters of the target}
    \item{\code{WeightedRE}}{A numeric vector with the weighted relative entropy of the prime and target inflectional paradigms}
    \item{\code{NormLevenshteinDist}}{A numeric vector with the normalized 
       Levenshtein distance of prime and target forms}
    \item{\code{TargetLemmaFreq}}{A numeric vector with log frequency of 
       the target lemma}
    \item{\code{PrimeSurfFreq}}{A numeric vector with log frequency of the prime form}
    \item{\code{PrimeCondition}}{A factor with prime conditions, levels: \code{DD}, \code{DSSD}, \code{SS}}
    \item{\code{CosineSim}}{A numeric vector with the cosine similarity of prime and target vector space semantics}
    \item{\code{IsMasc}}{A vector of logicals, \code{TRUE} if the noun is masculine.}
    \item{\code{TargetGender}}{A factor with the gender of the target, levels: \code{f}, \code{m}, and \code{n}}
    \item{\code{TargetCase}}{A factor specifying the case of the target noun, levels: \code{acc}, \code{dat}, \code{nom}}
    \item{\code{MeanLogObsRT}}{Mean log-transformed observed reaction time}
  }
}
\references{
Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli, M. (2011), An amorphous model for morphological processing in visual comprehension based on naive discriminative learning. Psychological Review, in press.
}
\examples{
# calculate the weight matrix for the full set of Serbian nouns
data(serbian)
serbian$Cues <- orthoCoding(serbian$WordForm, maxn=2)
serbian$Outcomes <- serbian$LemmaCase
sw <- estimateWeights(cuesOutcomes=serbian)

# calculate the meaning activations for all unique word forms
activations <- estimateActivations(unique(serbian[,c("WordForm", "Cues")]),sw)
activations <- activations + abs(min(activations))

# calculate simulated latencies for the experimental materials

data(serbianLex)
syntax <- c("acc", "dat", "gen", "ins", "loc", "nom", "Pl", "Sg")
we <- 0.4 # compound cue weight
strengths <- rep(0, nrow(serbianLex))
for(i in 1:nrow(serbianLex)) {
   target <- serbianLex$Target[i]
   prime <- serbianLex$Prime[i]
   targetLemma <- as.character(serbianLex$TargetLemma[i])
   primeLemma <- as.character(serbianLex$PrimeLemma[i])
   targetOutcomes <- c(targetLemma, primeLemma, syntax)
   primeOutcomes <- c(targetLemma, primeLemma, syntax)
   p <- activations[target, targetOutcomes]
   q <- activations[prime, primeOutcomes]
   strengths[i] <- sum((q^we)*(p^(1-we)))
}
serbianLex$SimRT <- -strengths
lengthPenalty <- 0.3
serbianLex$SimRT2 <- serbianLex$SimRT + 
  (lengthPenalty * (serbianLex$Length>5))

cor.test(serbianLex$SimRT, serbianLex$MeanLogObsRT)
cor.test(serbianLex$SimRT2, serbianLex$MeanLogObsRT)

serbianLex.lm <- lm(SimRT2 ~ Length +  WeightedRE*IsMasc + 
      NormLevenshteinDist + TargetLemmaFreq + 
      PrimeSurfFreq + PrimeCondition, data=serbianLex)
summary(serbianLex.lm)
}
\keyword{datasets}
