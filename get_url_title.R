###
###
###

get_url_title <- function(urls, return.df=TRUE) {
    library(httr)
    library(XML)
    complete_text <- function(x) try(sapply(xpathSApply(content(GET(x)), "//head/title"), xmlValue))
    complete_text2 <- function(x) {
        #Sys.sleep(0.1)
        cat(".")
        complete_text(x) }
    response <- sapply(urls, complete_text2)
    if(return.df==TRUE) { results <- data.frame(url.link=names(response), url.title=as.character(response)) }
    return(results) 
}
