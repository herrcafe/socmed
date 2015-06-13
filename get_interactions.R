# get retweets and @-mentions
get_interactions <- function(tweets, screenName, edge.attribute=NULL, ascii=TRUE, return.network="all.interactions", return.graph=TRUE, directed=TRUE) {
    library(stringr)
    if(return.graph==TRUE) { library(igraph) }
    
    # process text
    if(ascii==T) { tweets <- iconv(tweets, to="ASCII", sub="") }
    tweets <- gsub(" via @", " rt @", tolower(as.character(tweets)))
    
    if(!return.network=="mentions") {
        # get retweets
        rt.receiver <- as.character(screenName[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)])
        rt.sender <- gsub("rt @", "", str_extract(tweets, "rt @[a-zA-Z0-9_]+"))
        rt.sender <- rt.sender[!is.na(rt.sender)]
        rt.sender[rt.sender==""] <- "<NA>"
        rt.receiver[rt.receiver==""] <- "<NA>"

        # create rt edge list
        if(!is.null(edge.attribute)) {
            edge.variable <- edge.attribute[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)]
            rts.df <- data.frame(sender=rt.sender, receiver=rt.receiver, edge.attribute=edge.variable, type="retweet", stringsAsFactors=FALSE) }
        if(is.null(edge.attribute)) { rts.df <- data.frame(sender=rt.sender, receiver=rt.receiver, type="retweet", stringsAsFactors=FALSE) }
        rts.df <- rts.df[!is.na(rts.df$sender),]
        rts.df <- rts.df[!rts.df$sender==rts.df$receiver,]
        rownames(rts.df) <- NULL 
    }
    
    if(!return.network=="retweets") {
        # remove retweets from text corpora
        tweets <- gsub("rt @[a-z0-9_]{1,15}", "", tweets, perl=T)
        
        # get @-mentions edgelist
        mention.count.0 <- str_count(tweets, "@[a-zA-Z0-9_]+")
        mentioned <- str_extract_all(tweets, "@[a-zA-Z0-9_]+")
        at.receiver <- gsub("@", "", unlist(mentioned))
        at.sender <- rep(screenName, mention.count.0)
        
        # create edge list
        if(!is.null(edge.attribute)) { 
            edge.variable <- rep(edge.attribute, mention.count.0)
            ats.df <- data.frame(sender=at.sender, receiver=at.receiver, edge.attribute=edge.variable, type="mention", stringsAsFactors=FALSE) }
        if(is.null(edge.attribute)) { ats.df <- data.frame(sender=at.sender, receiver=at.receiver, type="mention", stringsAsFactors=FALSE) }
        ats.df <- ats.df[!is.na(ats.df$sender),]
        ats.df <- ats.df[!ats.df$sender==ats.df$receiver,]
        rownames(ats.df) <- NULL
    }
    
    if(return.graph==FALSE){
        if(return.network=="retweets") { return(rts.df) }
        if(return.network=="mentions") { return(ats.df) }
        if(return.network=="all.interactions") {
            # merge retweet and @-mention data frames
            info.df <- rbind(ats.df, rts.df)
            return(info.df) } }
    
    if(return.graph==TRUE){
        if(return.network=="retweets") {
            # edgelist to graph
            if(!is.null(rts.df$edge.attribute)) { rts.df$edge.attribute <- as.character(rts.df$edge.attribute) }
            rt.graph <- graph.data.frame(rts.df, directed=TRUE)
            E(rt.graph)[edge.attributes(rt.graph)$type=="retweet"]$color <- "darkred"
            return(rt.graph) }
        if(return.network=="mentions") { 
            # edgelist to graph
            if(!is.null(ats.df$edge.attribute)) { ats.df$edge.attribute <- as.character(ats.df$edge.attribute) }
            at.graph <- graph.data.frame(ats.df, directed=TRUE)
            E(at.graph)[edge.attributes(at.graph)$type=="mention"]$color <- "darkblue"
            return(at.graph) }
        if(return.network=="all.interactions") {
            # merge retweet and @-mention data frames
            info.df <- rbind(ats.df, rts.df)
            if(!is.null(info.df$edge.attribute)) { info.df$edge.attribute <- as.character(info.df$edge.attribute) }
            info.graph <- graph.data.frame(info.df, directed=TRUE)
            E(info.graph)[edge.attributes(info.graph)$type=="retweet"]$color <- "darkred"
            E(info.graph)[edge.attributes(info.graph)$type=="mention"]$color <- "darkblue"
            #plot.igraph(info.graph,vertex.label=NA,layout=layout.fruchterman.reingold,edge.width=E(info.graph)$weight/1000,edge.arrow.size=0.1,vertex.size=1,main="")
            return(info.graph) } }
}
