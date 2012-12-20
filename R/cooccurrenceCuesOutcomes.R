cooccurrenceCuesOutcomes <-
function(cuesOutcomes, duplicates=FALSE, method="C", max.cues=20000, max.characters=20000, max.lines=500000) 
{
  if(!(method %in% c("R","awk","C")))
    stop(paste("method: ",method, " unknown => select 'C', 'R' or 'awk'.",sep=""))

  if(method!="R" & is.null(getOption("ndl.estimateWeights")))
    standAlone = TRUE
  else
    standAlone = FALSE

  if(method=="C" & .Platform$OS.type=="unix" & standAlone)
    { cues = unique(unlist(strsplit(as.character(cuesOutcomes$Cues), "_")))
      outcomes = unique(unlist(strsplit(as.character(cuesOutcomes$Outcomes), "_")))
      outcomes = outcomes[outcomes!="NIL"]
      characters.cues = max(unlist(lapply(as.character(cuesOutcomes$Cues), nchar)))
      n.cues = length(cues)
      n.outcomes = length(outcomes)
      n.rows = NROW(cuesOutcomes)
      if(characters.cues>=max.characters)
        stop(paste("Overall maximum character length of cues ",characters.cues, " + 1 > ",max.characters," => switch method to 'awk' (or 'R')."))
      if(n.cues>max.cues)
        stop(paste("Overall number of unique cues: ",n.cues," > ",max.cues," => switch method to 'awk' (or 'R')."))
      if(n.outcomes>max.cues)
        stop(paste("Overall number of unique outcomes: ",n.outcomes," > ",max.cues," => switch method to 'awk' (or 'R')."))
      if(n.rows>max.lines)
        stop(paste("Overall number of 'cuesOutcomes' lines: ",n.rows," > ",max.lines," => switch method to 'awk' (or 'R')."))
     }

  if(standAlone)
    { write.table(cuesOutcomes[, c("Frequency", "Cues", "Outcomes")], 
          file = "ndl_410025912.txt", sep="\t", quote = FALSE, row.names = FALSE)
      if(method=="C")
        { write(paste("duplicates=",duplicates,sep=""), file="ndl_par_410025912.txt")
          write(format(max.lines, scientific=FALSE), file="ndl_par_410025912.txt", append=TRUE);
          write(format(max.characters, scientific=FALSE), file="ndl_par_410025912.txt", append=TRUE);
          write(format(max.cues, scientific=FALSE), file="ndl_par_410025912.txt", append=TRUE);
    	  .C("cooc")
        }
    }

  cues = unique(unlist(strsplit(as.character(cuesOutcomes$Cues), "_")))
  outcomes = unique(unlist(strsplit(as.character(cuesOutcomes$Outcomes),"_")))
  outcomes = outcomes[outcomes!="NIL"]
  if(length(grep(" ",cues))!=0)
    warning("One or more space characters ' ' among cues.")
  if(length(grep(" ",outcomes))!=0)
    warning("One or more space characters ' ' among outcomes.")

  if(method=="awk") {

  awk = paste('BEGIN { FS="\t"
     duplicates=',as.numeric(duplicates),'

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
       sep="\n", quiet = TRUE, quote="")
   coln = scan("featuresByOutcomes_410025912.col.txt", what = "character", 
      sep="\n", quiet = TRUE, quote="")
   m = matrix(res, length(rown), length(coln), byrow = TRUE)
   rownames(m) = rown
   colnames(m) = coln
   m = m[cues, outcomes, drop=FALSE]
   system("rm coocCuesOutcomes.awk", wait=TRUE)

   if(standAlone)
     system("rm ndl_410025912.txt featuresByOutcomes_410025912.*.txt", wait=TRUE)

   return(m)

   }

  if(method=="C")
  {
    res = scan("coocCuesOutcomes_410025912.txt", quiet = TRUE)
    rown = scan("rows_410025912.txt", what = "character", sep="\n", quiet = TRUE, quote="")
    coln = scan("columns_410025912.txt", what = "character", sep="\n", quiet = TRUE, quote="")
    m = matrix(res, length(rown), length(coln), byrow = TRUE)
    rownames(m) = rown
    colnames(m) = coln
    m = m[cues, outcomes, drop=FALSE]

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
      cs = unlist(strsplit(as.character(cuesOutcomes$Cues[i]), "_"))
      ot = unlist(strsplit(as.character(cuesOutcomes$Outcomes[i]), "_"))
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
