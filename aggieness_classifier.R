# simple classifier for agriculture-related tweet
get_aggieness <- function(message) {
    aggie.exp <- as.numeric()
    aggie.exp[1] <- length(grep("aggie", message))*3
    aggie.exp[2] <- length(grep("agr(i|o)", message))*3
    aggie.exp[3] <- length(grep("envir", message))*2
    aggie.exp[4] <- length(grep("agbio", message))*2
    aggie.exp[5] <- length(grep("farm(er|ers|ing|s|ings)", message))*2
    aggie.exp[6] <- length(grep("grow(er|ing)", message))*2
    aggie.exp[7] <- length(grep("crop(s)\\b", message))
    aggie.exp[8] <- length(grep("food", message))
    aggie.exp[9] <- length(grep("water\\b", message))
    aggie.exp[10] <- length(grep("land\\b", message))
    aggie.exp[11] <- length(grep("horti", message))
    aggie.exp[12] <- length(grep("pest\\b", message))
    aggie.exp[13] <- length(grep("cultiv", message))
    aggie.exp[14] <- length(grep("soil\\b", message))
    aggieness <- sum(aggie.exp)/sapply(gregexpr("[[:alpha:]]+", gsub("[[:punct:]]", "", message)), function(x) sum(x > 0))
    aggieness[is.nan(aggieness)] <- 0
    return(aggieness)
}