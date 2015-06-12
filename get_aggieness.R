# simple classifier for agriculture-related tweet
get_aggieness <- function(message) {
    aggie.exp <- as.numeric()
    aggie.exp[1] <- length(grep("aggie", message))*3
    aggie.exp[2] <- length(grep("agr(i|o)", message))*3
    aggie.exp[3] <- length(grep("envir", message))*2
    aggie.exp[4] <- length(grep("agbio", message))*2
    aggie.exp[5] <- length(grep("farm(er|ers|ing|s|ings)", message))*2
    aggie.exp[6] <- length(grep("husbandry", message))*2 
    aggie.exp[7] <- length(grep("crop(s)\\b", message))
    aggie.exp[8] <- length(grep("food", message))
    aggie.exp[9] <- length(grep("water\\b", message))
    aggie.exp[10] <- length(grep("land\\b", message))
    aggie.exp[11] <- length(grep("horti", message))
    aggie.exp[12] <- length(grep("pest(\\b|icide|icides)", message))*2
    aggie.exp[13] <- length(grep("cultiv", message))
    aggie.exp[14] <- length(grep("soil\\b", message))
    aggie.exp[15] <- length(grep("grow(er|ing)", message))*2
    aggie.exp[16] <- length(grep("groundwater", message))*2
    aggie.exp[17] <- length(grep("livestock", message))*2
    aggie.exp[18] <- length(grep("rainfall", message))*2
    aggie.exp[19] <- length(grep("rearing", message))
    aggie.exp[20] <- length(grep("drought", message))*3    
    aggie.exp[21] <- length(grep("conservation", message))
    aggie.exp[22] <- length(grep("sustain(able|ability)", message))*2
    aggie.exp[23] <- length(grep("agrarian", message))*2
    aggie.exp[24] <- length(grep("fertilizer", message))
    aggie.exp[25] <- length(grep("rural", message))*2
    aggie.exp[26] <- length(grep("gmo", message))*2
    aggie.exp[27] <- length(grep("organic\\b", message))*2
    aggie.exp[28] <- length(grep("usda\\", message))*2
    aggie.exp[29] <- length(grep("manure", message))
    aggieness <- sum(aggie.exp)/sapply(gregexpr("[[:alpha:]]+", gsub("[[:punct:]]", "", message)), function(x) sum(x > 0))
    aggieness[is.nan(aggieness)] <- 0
    return(aggieness)
}

# # test classifier
# temp <- tolower(sample(profiles.df$description, 1000))
# get_agginess(temp[924])
# get_agginess(temp[901])
