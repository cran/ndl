### estimateWeights uses an Rcpp module for faster computation of co-occurrence counts.
### See R documentation for more info.
estimateWeights <- function(cuesOutcomes, removeDuplicates=TRUE, saveCounts=FALSE, verbose=FALSE, trueCondProb=TRUE, addBackground=FALSE, hasUnicode=FALSE, ...) {
    
    ## Internal functions.
    toInt = function(char) {
        if (char != '_') { 
            return(paste(utf8ToInt(char),"*",sep='')) 
        } else { 
            return(char)
        }
    }
    
    toUTF8 = function(token) {
        token = sub('_','',token)
        tok = type.convert(token,as.is=T)
            if (is.numeric(tok)) {
                return(intToUtf8(tok)) 
            } else { 
                return(token)
            }
    }
    
    convertCues = function(cuelist) { 
        return(paste(unlist(lapply(unlist(strsplit(cuelist,'')), FUN=toInt)),collapse=''))
    }
    
    convertBack = function(cuelist) { 
        return(paste(unlist(lapply(unlist(strsplit(cuelist,'*',fixed=T)), FUN=toUTF8)),collapse=''))
    } 
        
####    Sys.setlocale("LC_COLLATE", "C")
    basename <- NULL
    basename = paste(substitute(cuesOutcomes))
    loaded = FALSE
    coocFile = paste(basename,".coocCues.rds",sep='')
    coocOutFile = paste(basename,".coocCuesOutcomes.rds",sep='')
    if (file.exists(coocFile) && file.exists(coocOutFile) ) {
        if (verbose) message("NOTE: Loading pre-computed coocurrence matrices. Ignoring DataFrame Provided.")
        flush.console()
        coocCues = readRDS(coocFile)
        coocCuesOutcomes = readRDS(coocOutFile)
        loaded = TRUE
    } else {
        empty.cues <- grep("(^_)|(__)|(_$)", cuesOutcomes$Cues)
        if(length(empty.cues)>0)
            stop("Incorrectly placed underscore(s) in 'Cues'.\n")
        
        empty.outcomes <- grep("(^_)|(__)|(_$)", cuesOutcomes$Outcomes)
        if(length(empty.outcomes)>0)
            stop("Incorrectly placed underscore(s) in 'Outcomes'.\n")
        
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
        
                                        # Fixing unicode sorting errors.
        if (hasUnicode) {
            cuesOutcomes$Cues = unlist(lapply(cuesOutcomes$Cues,FUN=convertCues))
        }
        
        ## Call Rcpp function to process all events.
        CuAndCo = learnLegacy(DFin=cuesOutcomes, RemoveDuplicates=removeDuplicates, verbose=verbose)
        coocCues = CuAndCo[[1]]
        coocCuesOutcomes = CuAndCo[[2]]
        rm(CuAndCo)
    }
    if (verbose) message("Starting to process matrices.")
    ## Check sanity of arguments
    if ((addBackground) & (!trueCondProb)) {
        if (verbose) {
            message("*WARNING: Can't add background rates without true conditional probabilities. \n*ACTION: Proceeding without background rates.")
        }
        addBackground = FALSE
    }
    
    if (addBackground & trueCondProb) {
      ## Add background for Cue-Cue cooc
      cueTotals = diag(coocCues)
      grandTotal = sum(cueTotals)
      coocCues["Environ",] = cueTotals
      coocCues[,"Environ"] = cueTotals
      coocCues["Environ","Environ"] = grandTotal
    }
    else {
      ## remove rows and columns reserved for background rates
      coocCues=coocCues[!rownames(coocCues) %in% "Environ", !colnames(coocCues) %in% "Environ" ]
      coocCuesOutcomes=coocCuesOutcomes[!rownames(coocCuesOutcomes) %in% "Environ",]
    }
    if (trueCondProb) {
      #Convert Cue-Outcome counts to Cue-Outcome Probabilities using diagonal
      cueTotals = diag(coocCues) 
      cueTotals[cueTotals == 0] = 1
      condProbsCues = coocCues/cueTotals
      probsOutcomesGivenCues = coocCuesOutcomes/cueTotals
    } else {
      ## use the original algorithm for normalization
      rowsums = rowSums(coocCuesOutcomes)
      rowsums[rowsums == 0] = 1
      condProbsCues = coocCues/rowsums
      probsOutcomesGivenCues = coocCuesOutcomes/rowsums
  }
### Sort the matrices in alphabetical order.
    coocCues = coocCues[order(rownames(coocCues)),order(colnames(coocCues))]
    coocCuesOutcomes = coocCuesOutcomes[order(rownames(coocCuesOutcomes)),order(colnames(coocCuesOutcomes))]
    ## Save the cooc matrices for later reuse (after doing Background rates and normalization.
    if (saveCounts & !loaded) {
        if (verbose) message("Completed Event Counts. Saving Cooc Data for future calculations.")
        flush.console()
        saveRDS(coocCues, file=coocFile)
        if (verbose) message(paste("Saved ",coocFile))
        flush.console()
        saveRDS(coocCuesOutcomes, file=coocOutFile)
        if (verbose) message(paste("Saved ",coocOutFile))
        flush.console()
    }
    if (verbose) message("Starting to calculate pseudoinverse.")
    flush.console()
    n = dim(condProbsCues)[1]
    if (n < 20000) {
        pseudoinverse = ginv(condProbsCues)
    } else {
        ## Use an approximation of the pseudoinverse here to make this feasible
        ## average hardware.
        if (verbose) message("Number of cues was too large for standard pseudoinverse. Switching to lower-rank approximation.")
        pseudoinverse = random.pseudoinverse(condProbsCues,verbose=verbose)
    }
    ## Calculate the weights by multiplying the pseudoinver of the c-c
    ## counts by the probabilites of the outcomes given the cues.
    weightMatrix = pseudoinverse %*% probsOutcomesGivenCues
    ## Deal with Unicode issue.
    if (hasUnicode) {
	rownames(weightMatrix) = unlist(lapply(rownames(coocCues),FUN=convertBack))
    } else {
        rownames(weightMatrix) = rownames(coocCues)
    }
    colnames(weightMatrix) = colnames(coocCuesOutcomes)
    if (verbose) message("Completed calculations. Returning weight matrix.")
    flush.console()
    return(weightMatrix)
  }



