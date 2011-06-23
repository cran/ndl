orthoCoding <-
function (words = c("hello", "world"), maxn = 2) 
{
    ngram.fnc = function(s, n) {
        len = nchar(s)
        ng = NULL
        for (i in 1:(len - n + 1)) {
            ng = c(ng, substr(s, i, i + n - 1))
        }
        return(paste(ng, collapse = "_"))
    }
    letters = strsplit(words, "")
    grams = unlist(lapply(letters, FUN = paste, collapse = "_"))
    letters = sapply(words, FUN = function(s) paste("#", s, "#", 
        sep = ""))
    if (maxn == 1) {
        return(grams)
    }
    else {
        for (i in 2:maxn) {
            gramsi = unlist(lapply(letters, FUN = ngram.fnc, 
                i))
            grams = paste(grams, gramsi, sep = "_")
        }
    }
    return(grams)
}
