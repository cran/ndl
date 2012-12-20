\name{dative}
\alias{dative}
\docType{data}
\title{Dative Alternation}
\description{
Data describing the realization of the dative as NP or PP in the Switchboard
corpus and the Treebank Wall Street Journal collection.
}
\usage{data(dative)}
\format{
  A data frame with 3263 observations on the following 15 variables.
  \describe{
    \item{\code{Speaker}}{a factor coding speaker; available only for the 
       subset of spoken English.}
    \item{\code{Modality}}{a factor with levels \code{spoken}, \code{written}.}
    \item{\code{Verb}}{a factor with the verbs as levels.} 
    \item{\code{SemanticClass}}{a factor with levels 
      \code{a} (abstract: 'give it some thought'), 
      \code{c} (communication: 'tell, give me your name'), 
      \code{f} (future transfer of possession: 'owe, promise'), 
      \code{p} (prevention of possession: 'cost, deny'), and
      \code{t} (transfer of possession: 'give an armband, send').}
    \item{\code{LengthOfRecipient}}{a numeric vector coding the 
      number of words comprising the recipient.}
    \item{\code{AnimacyOfRec}}{a factor with levels \code{animate} and
     \code{inanimate} for the animacy of the recipient.}
    \item{\code{DefinOfRec}}{a factor with levels \code{definite} and
     \code{indefinite} coding the definiteness of the recipient.}
    \item{\code{PronomOfRec}}{a factor with levels \code{nonpronominal} and
      \code{pronominal} coding the pronominality of the recipient.}
    \item{\code{LengthOfTheme}}{a numeric vector coding the number of words
      comprising the theme.}
    \item{\code{AnimacyOfTheme}}{a factor with levels \code{animate} and
      \code{inanimate} coding the animacy of the theme.}
    \item{\code{DefinOfTheme}}{a factor with levels \code{definite} and
      \code{indefinite} coding the definiteness of the theme.}
    \item{\code{PronomOfTheme}}{a factor with levels \code{nonpronominal} and
      \code{pronominal} coding the pronominality of the theme.}
    \item{\code{RealizationOfRecipient}}{a factor with levels \code{NP} and
      \code{PP} coding the realization of the dative.}
    \item{\code{AccessOfRec}}{a factor with levels \code{accessible}, 
      \code{given}, and \code{new} coding the accessibility of the recipient.}
    \item{\code{AccessOfTheme}}{a factor with levels \code{accessible}, 
      \code{given}, and \code{new} coding the accessibility of the theme.}
  }
}
\references{

Bresnan, J., Cueni, A., Nikitina, T. and Baayen, R. H. (2007)
Predicting the dative alternation, in Bouma, G. and Kraemer, I. and
Zwarts, J.  (eds.), \emph{Cognitive Foundations of Interpretation},
Royal Netherlands Academy of Sciences, 69-94.

}
\examples{
\dontrun{
data(dative)
out <- which(is.element(colnames(dative), c("Speaker","Verb")))
dative <- dative[,-out]
dative.ndl <- ndlClassify(RealizationOfRecipient ~ ., data=dative)
ndlStatistics(dative.ndl)

} }
\keyword{datasets}
