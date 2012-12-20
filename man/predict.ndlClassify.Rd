\name{predict.ndlClassify}
\alias{predict.ndlClassify}
\title{
Predict method for ndlClassify objects
}
\description{

   Obtains predictions on the basis of a fitted \code{"ndlClassify"}
   object on data already incorporated in the object or on new
   data with the same predictors as the originally fitted model object.

}
\usage{
\method{predict}{ndlClassify}(object, newdata=NULL, frequency=NA,
   type="choice", \dots)

}

\arguments{
  \item{object}{

      objects of class \code{"ndlClassify"}, typically the result of a
      call to \code{ndlClassify}.

  }
  \item{newdata}{

     optionally, a data frame in which to look for variables with
     which to predict.  If omitted (i.e. set to \code{NULL}), the
     original data used to fit the \code{object} are used.
  }
  \item{frequency}{ 

    A numeric vector (or the name of a column in the (new) data frame
    \code{newdata}) with the frequencies of the exemplars. If absent,
    each exemplar is assigned a frequency equal to 1.

  }
  \item{type}{

    the type of prediction requested.  The default option
    \code{type="choice"} produces the predicted individual discrete
    choices (i.e. Outcomes), given the predictor Cues selected for
    fitting the original \code{object}. The option \code{type="acts"}
    provides the sum activations for each Outcome given the Cue
    combinations in \code{newdata} (or in the original data in
    \code{object}, while the alternative \code{type="probs"} yields
    the distributions of predicted probabilities (based on the
    activations) over the Outcome responses.

  }
  \item{\dots}{

    further arguments passed to and from other functions.

  }
}

\details{

    If \code{newdata} is omitted the predictions are based on the data
    used for the fit.

}
\value{

   a vector \code{predicted}, or matrix of activations
   \code{activations}, or a matrix of predictions
   \code{probabilities}.

}
\references{
Arppe, A. and Baayen, R. H. (in prep.) Statistical classification and
principles of human learning.
}
\author{
Antti Arppe
}

\seealso{
\code{\link{ndlClassify}}, \code{\link{estimateActivations}}, \code{\link{acts2probs}}
}
\examples{

data(think)
think.ndl <- ndlClassify(Lexeme ~ Agent + Patient, data=think[1:300,])
head(predict(think.ndl, type="choice"))
predict(think.ndl, newdata=think[301:320,], type="probs")
predict(think.ndl, newdata=think[301:320,], type="acts")

}

\keyword{ classif }
