cooccurrenceCuesOutcomes <-
function(cuesOutcomes, duplicates=FALSE, method="C") 
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

  cues = unique(unlist(strsplit(cuesOutcomes$Cues, "_")))
  outcomes = unique(unlist(strsplit(cuesOutcomes$Outcomes,"_")))
  outcomes = outcomes[outcomes!="NIL"]

  if(method=="awk") {

  awk = paste('BEGIN { duplicates=',as.numeric(duplicates),'

     infile = "ndl_410025912.txt"
     getline < infile

     while (getline < infile > 0) {
  
       nfeatures=split($2, FEAT, "_")
       for (i=1;i<=nfeatures;i++) {
         f = FEAT[i]
         FEATURES[f]=1
         LISTOFFEATURES[f]=1
       }

       noutcomes=split($3, OUTC, "_")
       for(i=1;i<=noutcomes;i++) {
          o = OUTC[i]
          if(o != "NIL") {
            OUTCOMES[o] = 1
            LISTOFOUTCOMES[o]=1
          }
       }

      if(duplicates==1)
        for(i=1; i<=nfeatures; i++)
           for(j=1; j<=noutcomes; j++)
              if(OUTCOMES[j]!="NIL")
                COUNT[FEAT[i],OUTC[j]]+=$1
      if(duplicates==0)
        for(f in FEATURES)
           for(o in OUTCOMES)
              COUNT[f,o]+=$1

       delete OUTCOMES
       delete FEATURES
       delete FEAT
     }
     close(infile)
  
     outfile1 = "featuresByOutcomes_410025912.mat.txt"
     outfile2 = "featuresByOutcomes_410025912.row.txt"
     outfile3 = "featuresByOutcomes_410025912.col.txt"
     printf "" > outfile1
     printf "" > outfile2
     printf "" > outfile3
     nfeatures = 0
     for(f in LISTOFFEATURES) {
        printf "%s\\n", f >> outfile2
        nfeatures++
        V1[nfeatures] = f
     }
     noutcomes = 0
     for(o in LISTOFOUTCOMES) {
        printf "%s\\n", o >> outfile3
        noutcomes++
        V2[noutcomes] = o
     }
     for(i=1; i <= nfeatures; i++) {
        for(j=1; j <= noutcomes; j++) {
           f=V1[i]
           o=V2[j]
           printf "%d ", COUNT[f,o] >> outfile1
        }
        printf "\\n" >> outfile1
     }
     close(outfile)

   }',sep="")

   write(awk, file="coocCuesOutcomes.awk")

   system("awk -f coocCuesOutcomes.awk", wait=TRUE)

   res = scan("featuresByOutcomes_410025912.mat.txt", quiet = TRUE)
   rown = scan("featuresByOutcomes_410025912.row.txt", what = "character", 
       quiet = TRUE, quote="")
   coln = scan("featuresByOutcomes_410025912.col.txt", what = "character", 
      quiet = TRUE, quote="")
   m = matrix(res, length(rown), length(coln), byrow = TRUE)
   rownames(m) = rown
   colnames(m) = coln
   m = m[cues, outcomes]
   system("rm coocCuesOutcomes.awk", wait=TRUE)

   if(standAlone)
     system("rm ndl_410025912.txt featuresByOutcomes_410025912.*.txt", wait=TRUE)

   return(m)

   }

  if(method=="C")
  {
    res = scan("coocCuesOutcomes_410025912.txt", quiet = TRUE)
    rown = scan("rows_410025912.txt", what = "character", quiet = TRUE, quote="")
    coln = scan("columns_410025912.txt", what = "character", quiet = TRUE, quote="")
    m = matrix(res, length(rown), length(coln), byrow = TRUE)
    rownames(m) = rown
    colnames(m) = coln
    m = m[cues, outcomes]

    if(standAlone)
      system("rm ndl_410025912.txt ndl_par_410025912.txt rows_410025912.txt columns_410025912.txt coocCues_410025912.txt coocCuesOutcomes_410025912.txt", wait=TRUE)

    return(m)
  }

  if(method=="R")
  {
    m = matrix(0, length(cues), length(outcomes))
    rownames(m)=cues
    colnames(m)=outcomes

    for(i in 1:nrow(cuesOutcomes)) {
      cs = unlist(strsplit(cuesOutcomes$Cues[i], "_"))
      ot = unlist(strsplit(cuesOutcomes$Outcomes[i], "_"))
      ot = ot[ot!="NIL"]
      if(!duplicates)
        { cs = unique(cs)
          ot = unique(ot)
        }
      f = cuesOutcomes$Frequency[i]
      for(cue in cs)
         for(out in ot)
            m[cue,out] = m[cue,out]+f
  }
  return(m)
  }
}
