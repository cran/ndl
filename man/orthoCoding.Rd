\name{orthoCoding}
\alias{orthoCoding}
\title{
  code a character string (written word form) as n-grams
}
\description{
  \code{orthoCoding} codes a character string into unigrams, bigrams, 
  ..., n-grams, with as default bigrams as longest substring. 
}
\usage{
orthoCoding(words = c("hello", "world"), maxn = 2)
}
\arguments{
  \item{words}{
    A character vector of strings (usually words) to be recoded as unigrams,
    bigrams, \dots, ngrams.
}
  \item{maxn}{
    The longest substring to be encoded.  By default, bigrams (\code{maxn=2}) is
    the longest substring considered. 
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
Peter Hendrix and Harald Baayen
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{estimateWeights}}.
}
\examples{
data(serbian) 
serbian$Cues=orthoCoding(serbian$WordForm, maxn=2)
}
\keyword{ discriminative learning }
