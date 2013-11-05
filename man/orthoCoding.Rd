\name{orthoCoding}
\alias{orthoCoding}
\title{
  Code a character string (written word form) as letter n-grams
}
\description{
  \code{orthoCoding} codes a character string into unigrams, bigrams, 
  ..., n-grams, with as default bigrams as the substring size. If
  tokenization is not at the letter/character level, a token separator
  can be provided.
}
\usage{
orthoCoding(strings=c("hel.lo","wor.ld"), grams = c(2), tokenized = F, sepToken = '.') 
}
\arguments{
  \item{strings}{
    A character vector of strings (usually words) to be recoded as
n-grams.
}
  \item{grams}{
    A vector of numbers, each one a size of ngram to be produced. For
example a vector like grams=c(1,3) will create the unigram and trigram
cues from the input. 
}
\item{tokenized}{
    If tokenzied is FALSE (the default), the input strings are split
into letters/characters. If it is set to TRUE, the strings will be split
up based on the value of sepToken.
}
\item{sepToken}{
A string that defines which character will be used to separate tokens
when tokenized is TRUE. Defaults to the "." character.
}
}
%\details{
%}
\value{
  A vector of grams (joined by underscores), one for each word in the input
  vector \cite{words}.
}
\references{

Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix,
P. and Marelli, M., An amorphous model for morphological processing in
visual comprehension based on naive discriminative learning.
Psychological Review, 118, 438-482.

}
\author{
Cyrus Shaoul, Peter Hendrix and Harald Baayen
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{estimateWeights}}.
}
\examples{

#Default
orthoCoding(tokenize=FALSE)
#With tokenizing on a specific character
orthoCoding(tokenize=TRUE)

#Comparing different n-gram sizes
data(serbian) 
serbian$Cues=orthoCoding(serbian$WordForm, grams=2)
head(serbian$Cues)
serbian$Cues=orthoCoding(serbian$WordForm, grams=c(2,4))
head(serbian$Cues)
}
\keyword{ discriminative learning }
