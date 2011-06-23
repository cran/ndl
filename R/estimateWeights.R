estimateWeights <-
function(cuesOutcomes, duplicates=FALSE, saveCounts=FALSE, method="C") 
{
	require(MASS, quietly = TRUE)

        if(!(method %in% c("R","awk","C")))
          stop(paste("method: ",method, " unknown => select 'C', 'R' or 'awk'.", sep=""))

        empty.cues <- grep("(^_)|(__)|(_$)", cuesOutcomes$Cues)
        if(length(empty.cues)>0)
          stop("Incorrectly placed unscore(s) in Cues.\n")

	cues = unique(unlist(strsplit(cuesOutcomes$Cues, "_")))
	outcomes = unique(unlist(strsplit(cuesOutcomes$Outcomes, "_")))
	outcomes = outcomes[outcomes!="NIL"]
        characters.cues = max(unlist(lapply(cuesOutcomes$Cues, nchar)))
        n.cues = length(cues)
        n.outcomes = length(outcomes)

        if(method=="awk" & .Platform$OS.type!="unix")
          stop("\nmethod 'awk' only available in 'unix' type OS.")
        if(method=="C" & .Platform$OS.type!="unix")
          { if(characters.cues>=20000)
              stop(paste("Overall maximum character length of cues ",characters.cues, " > 20000 => switch method to 'R'."))
            if(n.cues>=20000)
              stop(paste("Overall number of unique cues: ",n.cues," > 20000 => switch method to 'R'."))
            if(n.outcomes>=20000)
              stop(paste("Overall number of unique outcomes: ",n.outcomes," > 20000 => switch method to 'R'."))
          }
        if(method=="C" & .Platform$OS.type=="unix")
          { if(characters.cues>=20000)
              stop(paste("Overall maximum character length of cues ",characters.cues, " > 20000 => switch method to 'awk' (or 'R')."))
            if(n.cues>=20000)
              stop(paste("Overall number of unique cues: ",n.cues," > 20000 => switch method to 'awk' (or 'R')."))
            if(n.outcomes>=20000)
              stop(paste("Overall number of unique outcomes: ",n.outcomes," > 20000 => swith method to 'awk' (or 'R')."))
          }
          
#### *.txt needs to be written and C code loaded and run only if method!="R"
	if(method!="R") 
          write.table(cuesOutcomes[, c("Frequency", "Cues", "Outcomes")], 
            file = "ndl_410025912.txt", quote = FALSE, row.names = FALSE)
        if(method=="C")
            write(paste("duplicates=",duplicates,sep=""), file="ndl_par_410025912.txt")
        if(method=="C")
    	  .C("cooc")

	coocCues = cooccurrenceCues(cuesOutcomes, duplicates=duplicates, method=method)
	coocCuesOutcomes = cooccurrenceCuesOutcomes(cuesOutcomes, duplicates=duplicates, method=method)

	coocCues = coocCues[cues, cues]
	coocCuesOutcomes = coocCuesOutcomes[cues, outcomes]
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
	if (method=="C")
	  system("rm ndl_410025912.txt ndl_par_410025912.txt rows_410025912.txt columns_410025912.txt coocCues_410025912.txt coocCuesOutcomes_410025912.txt", wait=TRUE)
        if(method=="awk")
          system("rm ndl_410025912.txt featuresByOutcomes_410025912.*.txt cooc_410025912.mat.txt cooc_410025912.row.txt", wait=TRUE)

	return(weightMatrix)
}
