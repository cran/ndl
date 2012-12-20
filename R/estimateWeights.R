estimateWeights <-
function(cuesOutcomes, duplicates=FALSE, saveCounts=FALSE, method="C", max.cues=20000, max.characters=20000, max.lines=500000, ...) 
{
	require(MASS, quietly = TRUE)

        if(!(method %in% c("R","awk","C")))
          stop(paste("method: ",method, " unknown => select 'C', 'R' or 'awk'.", sep=""))

        empty.cues <- grep("(^_)|(__)|(_$)", cuesOutcomes$Cues)
        if(length(empty.cues)>0)
          stop("Incorrectly placed unscore(s) in 'Cues'.\n")
        empty.outcomes <- grep("(^_)|(__)|(_$)", cuesOutcomes$Outcomes)
        if(length(empty.outcomes)>0)
          stop("Incorrectly placed unscore(s) in 'Outcomes'.\n")

        NA.cue_strings <- grep("(^NA_)|(_NA_)|(_NA$)",cuesOutcomes$Cues)
        NA.outcome_strings <- grep("(^NA_)|(_NA_)|(_NA$)",cuesOutcomes$Outcomes)
        if(length(NA.cue_strings)>0)
          warning(paste("Potential NA's in ",length(NA.cue_strings)," 'Cues'.",sep=""))
        if(length(NA.outcome_strings)>0)
          warning(paste("Potential NA's in ",length(NA.outcome_strings)," 'Outcomes'.",sep=""))

        NA.cues <- which(is.na(cuesOutcomes$Cues))
        NA.outcomes <- which(is.na(cuesOutcomes$Outcomes))
        if(length(NA.cues)>0)
          stop(paste("NA's in 'Cues': ",length(NA.cues)," cases.",sep=""))
        if(length(NA.outcomes)>0)
          stop(paste("NA's in 'Outcomes': ",length(NA.outcomes)," cases.",sep=""))

	cues = unique(unlist(strsplit(as.character(cuesOutcomes$Cues), "_")))
	outcomes = unique(unlist(strsplit(as.character(cuesOutcomes$Outcomes), "_")))
	outcomes = outcomes[outcomes!="NIL"]
        characters.cues = max(unlist(lapply(as.character(cuesOutcomes$Cues), nchar)))
        n.cues = length(cues)
        n.outcomes = length(outcomes)
    	n.rows = NROW(cuesOutcomes)

        if(method=="awk" & .Platform$OS.type!="unix")
          stop("\nmethod 'awk' only available in 'unix' type OS.")
        if(method=="C" & .Platform$OS.type!="unix")
          { if(characters.cues>=max.characters)
              stop(paste("Overall maximum character length of cues ",characters.cues," + 1 > ",max.characters," => switch method to 'R'."))
            if(n.cues>max.cues)
              stop(paste("Overall number of unique cues: ",n.cues," > ",max.cues," => switch method to 'R'."))
            if(n.outcomes>max.cues)
              stop(paste("Overall number of unique outcomes: ",n.outcomes," > ",max.cues," => switch method to 'R'."))
	    if(n.rows>max.lines)
	      stop(paste("Overall number of 'cuesOutcomes' lines: ",n.rows," > ",max.lines," => switch method to 'R'."))
          }
        if(method=="C" & .Platform$OS.type=="unix")
          { if(characters.cues>=max.characters)
              stop(paste("Overall maximum character length of cues ",characters.cues, " + 1 > ",max.characters," => switch method to 'awk' (or 'R')."))
            if(n.cues>max.cues)
              stop(paste("Overall number of unique cues: ",n.cues," > ",max.cues," => switch method to 'awk' (or 'R')."))
            if(n.outcomes>max.cues)
              stop(paste("Overall number of unique outcomes: ",n.outcomes," > ",max.cues," => swith method to 'awk' (or 'R')."))
	    if(n.rows>max.lines)
	      stop(paste("Overall number of 'cuesOutcomes' lines: ",n.rows," > ",max.lines," => switch method to 'awk' (or 'R')."))
          }

#### *.txt needs to be written and C code loaded and run only if method!="R"
	if(method!="R") 
          { write.table(cuesOutcomes[, c("Frequency", "Cues", "Outcomes")], 
              file = "ndl_410025912.txt", sep="\t", quote = FALSE, row.names = FALSE)
	    options(ndl.estimateWeights=TRUE)
	  }
        if(method=="C")
            { write(paste("duplicates=",duplicates,sep=""), file="ndl_par_410025912.txt");
              write(format(max.lines, scientific=FALSE), file="ndl_par_410025912.txt", append=TRUE);
              write(format(max.characters, scientific=FALSE), file="ndl_par_410025912.txt", append=TRUE);
              write(format(max.cues, scientific=FALSE), file="ndl_par_410025912.txt", append=TRUE);
            }

        if(method=="C")
    	  .C("cooc")

	coocCues = cooccurrenceCues(cuesOutcomes, duplicates=duplicates, method=method)
	coocCuesOutcomes = cooccurrenceCuesOutcomes(cuesOutcomes, duplicates=duplicates, method=method)

	coocCues = coocCues[cues, cues, drop=FALSE]
	coocCuesOutcomes = coocCuesOutcomes[cues, outcomes, drop=FALSE]

	if (saveCounts) {
	  save(coocCues, file="coocCues.rda")
	  save(coocCuesOutcomes, file="coocCuesOutcomes.rda")
	}

	rowsums = rowSums(coocCuesOutcomes)
	rowsums[rowsums == 0] = 1
	condProbsCues = coocCues/rowsums
	probsOutcomesGivenCues = coocCuesOutcomes/rowsums
	pseudoinverse = ginv(condProbsCues)
	weightMatrix = pseudoinverse %*% probsOutcomesGivenCues
	rownames(weightMatrix) = cues

#### These removals need to be done only when 'method!="R"'
        if(method!="R")
	  options(ndl.estimateWeights=NULL)
	if(method=="C")
	  system("rm ndl_410025912.txt ndl_par_410025912.txt rows_410025912.txt columns_410025912.txt coocCues_410025912.txt coocCuesOutcomes_410025912.txt", wait=TRUE)
        if(method=="awk")
          system("rm ndl_410025912.txt featuresByOutcomes_410025912.*.txt cooc_410025912.mat.txt cooc_410025912.row.txt", wait=TRUE)

	return(weightMatrix)
}
