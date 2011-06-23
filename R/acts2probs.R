acts2probs <- function(acts) {

  acts = acts + abs(min(acts))

  rowsums = apply(acts,1,sum)

  m = matrix(rep(rowsums, rep(ncol(acts),nrow(acts))), nrow(acts), ncol(acts), byrow=TRUE)

  p = acts/m
  colnames(p)=colnames(acts)

  maxima = apply(p, 1, FUN=function(v) which.max(v))

  predictions = colnames(p)[maxima]

  return(list(p = p, predicted = predictions))

}

