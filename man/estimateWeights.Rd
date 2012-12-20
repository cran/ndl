\name{estimateWeights}
\alias{estimateWeights}
\title{
  Estimation of the association weights using the equilibrium
  equations of Danks (2003) for the Rescorla-Wagner equations.
}
\description{
  A function to estimate the weights (associative strengths) for
  cue-outcome pairs when learning is in equilibrium, using the
  equilibrium equations for the Rescorla-Wagner model of Danks (2003).
}
\usage{
estimateWeights(cuesOutcomes, duplicates=FALSE, saveCounts=FALSE,
   method="C", max.cues=20000, max.characters=20000, max.lines=500000, \dots)
}
\arguments{
  \item{cuesOutcomes}{
    A data frame with three variables specifying frequency, cues, and
    outcomes, that may be created with \code{\link{ndlCuesOutcomes}}:
    \describe{
    \item{\code{Cues}}{A character vector specifying the cues. 
           When there is more than one cue, the cues should be 
           separated by underscores.}
    \item{\code{Outcomes}}{A character vector specifying the outcomes.  
           When there is more than one outcome, the outcomes should 
           be separated by underscores.}
    \item{\code{Frequency}}{A numeric vector specifying the frequency 
           with which a combination of cues and outcomes occurs.}
    }
  }
  \item{duplicates}{
    A logical specifying whether multiple occurrences of a Cue in
    conjunction with an individual instance of an Outcome shall each
    be counted as a distinct occurrence of that Cue (\code{TRUE}), or
    only as a single occurrence (\code{FALSE}: default).
 }
  \item{saveCounts}{
    A logical specifying whether the co-occurrence matrices should be
    saved.  If set equal to \code{TRUE}, the files \code{coocCues.rda} and
    \code{coocCuesOutcomes.rda} will be saved in the current
    workspace.
  }
  \item{method}{
    A character string indicating whether cooccurrence matrices should
    be calculated with C using \code{cooc} (\code{"C"}: default), with two AWK
    scripts (\code{"awk"}), or within R (\code{"R"}).
  }
  \item{max.cues}{
    An integer specifying the overall number of distinct cues (and
    outcomes); by default set to \code{=20000}.
  }
  \item{max.characters}{
    An integer specifying the maximum character length of cues per
    each occurrence, i.e. line in \code{cuesOutcomes}, by default set to
    \code{=20000}.
  }
  \item{max.lines}{An integer specifying the maximum number of
    lines in \code{cuesOutcomes}; by default set to \code{=500000}.
  }
  \item{\dots}{Control arguments to be passed along from \code{\link{ndlClassify}}
    and/or \code{\link{ndlCrossvalidate}}.
  }
}
\details{

  The real work is done by C code (\code{cooc}), when
  \code{method="C"} (default), or two AWK scripts, when
  \code{method="awk"}.  Otherwise, a slow R implementation can be used
  (\code{method="R"}), in which case the model may not scale up.

  With the fastest C code version, the default maximum length of
  individual cue combinations is 20,000 characters (including
  underscores), the default maximum number of unique Cues or unique
  Outcomes is 20,000, and the default number of input lines in
  \code{cuesOutcomes} is 500,000. These limits can be adjusted with
  the appropriate arguments; however, that should only be done with
  the utmost care.  The AWK scripts do not have these restrictions
  concerning their input, but will only work in UNIX-like operating
  systems, i.e. not in Windows.

  For examples of how the \code{cuesOutcomes} data frame should be
  structured, see the data sets \code{\link{danks}},
  \code{\link{plurals}}, and \code{\link{serbian}}. N.B. Empty
  \code{Cues} or \code{Outcomes} (effectively having \code{length =
  0}), e.g. \code{Cues} or \code{Outcomes} strings with an initial or
  final underscore or two immediately adjacent underscores, will
  result in an error.

}
\value{
  A matrix with cue-to-outcome association strengths. Rows are cues,
  and columns are outcomes.  Rows and columns are labeled.
}
\references{

Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix,
P. and Marelli, M., An amorphous model for morphological processing in
visual comprehension based on naive discriminative learning.
Psychological Review, 118, 438-482.

}
\author{
R. H. Baayen and Petar Milin, with contributions from Antti Arppe and
Peter Hendrix.
}
\note{
  With the current C and AWK versions, several temporary text files
  are written to and erased from the current working directory in
  order to undertake the cooccurrence calculations.

  For the C version, these temporary files are named:
  \code{ndl_410025912.txt}, \code{ndl_par_410025912.txt},
  \code{rows_410025912.txt}, \code{columns_410025912.txt},
  \code{coocCues_410025912.txt}, and \code{coocCuesOutcomes_410025912.txt}.

  For the AWK version, these temporary files are named:
  \code{ndl_410025912.txt},
  \code{featuresByOutcomes_410025912.mat.txt},
  \code{featuresByOutcomes_410025912.row.txt},
  \code{featuresByOutcomes_410025912.col.txt},
  \code{cooc_410025912.mat.txt}, and \code{cooc_410025912.row.txt}.

}
\section{Acknowledgements}{
  The assistance of Uwe Ligges in getting the C function \code{cooc}
  to work within the R framework is greatly appreciated.
}
\seealso{
  \code{\link{estimateActivations}, \link{ndlCuesOutcomes},
  \link{cooccurrenceCues}, \link{cooccurrenceCuesOutcomes},
  \link{danks}, \link{plurals}, \link{serbian}}
}
\examples{
  data(danks)
  estimateWeights(cuesOutcomes=danks)

  data(plurals)
  plurals$Cues <- orthoCoding(plurals$WordForm, maxn=1)
  estimateWeights(cuesOutcomes=plurals)
  
  data(serbian)
  serbian$Cues <- orthoCoding(serbian$WordForm, maxn=2)
  serbian$Outcomes <- serbian$LemmaCase
  sw <- estimateWeights(cuesOutcomes=serbian)
  sw[1:5,1:6]
}
\keyword{ classif }
