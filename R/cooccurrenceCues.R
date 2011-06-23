cooccurrenceCues <-
function (cuesOutcomes, duplicates=FALSE, method="C") 
{
  if(!(method %in% c("R","awk","C")))
    stop(paste("method: ",method, " unknown => select 'C', 'R' or 'awk'.",sep=""))

  if(method!="R" & !("ndl_410025912.txt" %in% list.files()))
    { standAlone=TRUE
      write.table(cuesOutcomes[, c("Frequency", "Cues", "Outcomes")], 
          file = "ndl_410025912.txt", quote = FALSE, row.names = FALSE)
      if(method=="C")
        { write(paste("duplicates=",duplicates,sep=""), file="ndl_par_410025912.txt")
    	  .C("cooc")
        }
    }
  else
    standAlone=FALSE

  if (method=="C") {

  res = scan("coocCues_410025912.txt", quiet = TRUE)
  rown = scan("rows_410025912.txt", what = "character", quiet = TRUE, quote="")
  m = matrix(res, length(rown), length(rown), byrow = TRUE)
  rownames(m) = rown
  colnames(m) = rownames(m)

  if(standAlone)
    system("rm ndl_410025912.txt ndl_par_410025912.txt rows_410025912.txt columns_410025912.txt coocCues_410025912.txt coocCuesOutcomes_410025912.txt", wait=TRUE)

  return(m)
  }

  if(method=="awk") {

  awk = paste('BEGIN { duplicates=',as.numeric(duplicates),'

    ufeatures=0
    infile = "ndl_410025912.txt"
    getline < infile
    
    while (getline < infile > 0) {

      nfeatures=split($2, FEAT, "_")
      for(i=1;i<=nfeatures;i++) {
         f = FEAT[i]
         FEATURES[f]=1
         if(!(f in ALLFEATURES))
           { ALLFEATURES[f]=1
             LISTOFFEATURES[++ufeatures]=f
           }
      }
      if(duplicates==1)
        for(i=1; i<=nfeatures; i++)
           for(j=1; j<=nfeatures; j++)
              COUNT[FEAT[i],FEAT[j]]+=$1
      if(duplicates==0)
         for(f1 in FEATURES)
            for(f2 in FEATURES)
               COUNT[f1,f2]+=$1

      delete FEATURES
      delete FEAT
    }
    close(infile)
   
    outfile1 = "cooc_410025912.mat.txt"
    outfile2 = "cooc_410025912.row.txt"
    printf "" > outfile1
    printf "" > outfile2
    nfeatures = 0
    for(i=1; i <= ufeatures; i++) {
       printf "%s\\n", LISTOFFEATURES[i] >> outfile2
       nfeatures++
       V1[nfeatures] = LISTOFFEATURES[i]
    }
    for(i=1; i <= nfeatures; i++) {
       for(j=1; j <= nfeatures; j++) {
          f1=V1[i]
          f2=V1[j]
          printf "%d ", COUNT[f1,f2] >> outfile1
       }
       printf "\\n" >> outfile1
    }
    close(outfile)

  }',sep="")

  write(awk, file="coocCues.awk")

  system("awk -f coocCues.awk", wait = TRUE)
  res = scan("cooc_410025912.mat.txt", quiet = TRUE)
  rown = scan("cooc_410025912.row.txt", what = "character", quiet = TRUE, quote="")
  m = matrix(res, length(rown), length(rown), byrow = TRUE)
  rownames(m) = rown
  colnames(m) = rownames(m)
  system("rm coocCues.awk", wait=TRUE)

  if(standAlone)
    system("rm ndl_410025912.txt cooc_410025912.mat.txt cooc_410025912.row.txt", wait=TRUE)

  return(m)

  }
  
  if(method=="R") {

  features = unique(unlist(strsplit(cuesOutcomes$Cues, "_")))
  featuresets = strsplit(cuesOutcomes$Cues, "_")
  if(!duplicates)
    featuresets = lapply(featuresets,unique)

  binmat = matrix(0, nrow(cuesOutcomes), length(features))
  colnames(binmat) = features
  for (f in features) {
    binmat[,f] = unlist(lapply(featuresets, function(v) return(sum(v==f))))
  }

  resmat = matrix(0, length(features), length(features))
  rownames(resmat) = colnames(resmat) = features

  for (i in 1:length(features)) {
    f1 = features[i]
    for (j in i:length(features)) {
       f2 = features[j]
       resmat[f1,f2] = sum(binmat[,f1]*binmat[,f2]*cuesOutcomes$Frequency)
       if (f1 != f2) resmat[f2,f1] = resmat[f1,f2]
    }
  }

  return(resmat)

  }

}
