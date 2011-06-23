estimateActivations <- function(cuesOutcomes, weightMatrix, unique=FALSE, reportNew=TRUE) {

  cues = rownames(weightMatrix)

  obsCues = strsplit(cuesOutcomes$Cues, "_")
  uniqueObsCues = unique(unlist(obsCues))
  newCues = uniqueObsCues[!is.element(uniqueObsCues, cues)]


  if (length(newCues) > 0) {
    wnew = matrix(0, length(newCues), ncol(weightMatrix))
    rownames(wnew)=newCues
    colnames(wnew)=colnames(weightMatrix)
    w = rbind(weightMatrix, wnew)
    if (reportNew) {
      cat("\nThere are previously unseen cues:\n")
      for (i in 1:length(newCues)) {
        cat(newCues[i], " ")
      }
      cat("\n")
    }
    cues = c(cues, newCues)
  } else {
    w = weightMatrix
  }


  m = matrix(0, length(cues), nrow(cuesOutcomes))
  rownames(m) = cues
  colnames(m) = cuesOutcomes$WordForm

  v = rep(0, length(cues))
  names(v) = cues

  for (i in 1:nrow(cuesOutcomes)) {
    v[obsCues[[i]]]=1
    m[,i] = v
    v[obsCues[[i]]]=0
  }

  a=t(w)%*%m

  if (unique==TRUE) {
    return(unique(t(a)))
  } else {
    return(t(a))
  }

}
