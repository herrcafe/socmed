get_retweet_mentions <- function(screenName, tweets, id, date, time, time.num) {
  
  # load required packages
  library(stringr)
  
  # extract retweets
  rt.receiver <- tolower(as.character(screenName[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)]))
  rt.sender <- gsub("rt @", "", str_extract(tweets, "rt @[a-zA-Z0-9_]+"))
  rt.sender <- rt.sender[!is.na(rt.sender)]
  rt.tweet.id <- id[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)]
  rt.time.char <- as.character(time[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)])
  rt.time.num <- as.integer(time[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)])
  rt.date.char <- as.character(date[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)])
  
  # check for errors
  rt.sender[rt.sender==""] <- "NA"
  rt.receiver[rt.receiver==""] <- "NA"
  
  # create edge list
  rts.df <- data.frame(sender=rt.sender, receiver=rt.receiver, type="retweet", tweetID=rt.tweet.id, 
                       dateChar=rt.date.char, timeChar=rt.time.char, timeNum=rt.time.num, stringsAsFactors=FALSE)
  
  # remove invalid entries
  rts.df <- rts.df[!is.na(rts.df$sender),]
  rts.df <- rts.df[!rts.df$sender==rts.df$receiver,]
  
  # get @-mentions edgelist
  mention.count.0 <- str_count(tweets, "@[a-zA-Z0-9_]+")
  mention.count.0[is.na(mention.count.0)] <- 0
  mentioned <- stringr::str_extract_all(tweets, "@[a-zA-Z0-9_]+")
  mentioned.1 <- unlist(mentioned)[!is.na(unlist(mentioned))]
  at.receiver <- gsub("@", "", mentioned.1)
  at.sender <- rep(screenName, mention.count.0)
  at.tweet.id <- rep(id, mention.count.0)
  at.time.char <- rep(as.character(time), mention.count.0)
  at.time.num <- rep(as.integer(time), mention.count.0)
  at.date.char <- as.character(rep(as.integer(date), mention.count.0))
  
  # create edge list
  ats.df <- data.frame(sender=at.sender, receiver=at.receiver, type="mention", tweetID=at.tweet.id, 
                       dateChar=at.date.char, timeChar=at.time.char, timeNum=at.time.num, stringsAsFactors=FALSE)
  
  # remove invalid @-mentions
  ats.df <- ats.df[!is.na(ats.df$sender),]
  ats.df <- ats.df[!ats.df$sender==ats.df$receiver,]
  
  # bind df
  edgelist <- rbind(rts.df, ats.df)
  return(edgelist)

}
