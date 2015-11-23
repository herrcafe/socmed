get_url_title_buzz <- function(urls, sleep.time=0, return.df=TRUE) {
    library(RCurl)
    library(RJSONIO)
    get_url_title_buzz1 <- function(x) try(fromJSON(getURL(paste0("http://tools.buzzstream.com/metaDataService?url=",x))))
    get_url_title_buzz2 <- function(x) {
        Sys.sleep(sleep.time)
        cat(".")
        get_url_title_buzz1(x) }
    response <- sapply(urls, get_url_title_buzz2)
    if(return.df==TRUE) {
        if(is.matrix(response)) { 
            results <- as.data.frame(t(response))
            rownames(results) <- NULL }
        else {
            results <- data.frame(url=as.character(names(sapply(response, row.names))),
                                  title=as.character(sapply(response, "[", 2, USE.NAMES=F)),
                                  description=as.character(sapply(response, "[", 3, USE.NAMES=F)),
                                  keywords=as.character(sapply(response, "[", 4, USE.NAMES=F))) }
        return(results) } else {
            return(response) }
}
