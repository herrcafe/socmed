###
### geo-networks over time
###

# install igraph 0.7.1
# install.packages("C:/Users/bastos/OneDrive - City, University of London/Geo-networks over time/igraph_0.7.1.zip", repos=NULL)
# install.packages("C:/Users/bastos/OneDrive - City, University of London/Geo-networks over time/igraph_0.7.1.tar.gz", repos=NULL, type="source")

# set path
path.proj <- "C:/Users/bastos/OneDrive - City, University of London/Geo-networks over time"

# set required packages
req.packages <- c("reshape", "stringr", "plyr", "RCurl", "httr", "streamR", "igraph", "rgexf", "maps", "ggmap", "mapdata", "ggplot2", "visNetwork")

# prepare wd
project="geonetworks"
setwd(path.proj)
lapply(req.packages, require, character.only=T)
source("get_interactions.R")

# load files
file.to.load <- dir(paste0(path.proj, "/JSON"), full.names = T)
geo.df <- streamR::parseTweets(file.to.load[1])
save(geo.df, file="geo.df.rda")
# geo.df <- geo.df.temp <- data.frame()
# for(u in 1:length(file.to.load)) {
#   geo.df.temp <- streamR::parseTweets(file.to.load[u])
#   geo.df <- rbind(geo.df, geo.df.temp)
#   save(geo.df, file="geo.df.rda")
# }

# beat data into shape
geo.df$time <- as.POSIXct(geo.df$created_at, format = "%a %b %d %H:%M:%S %z %Y")
hist(geo.df$time, breaks="hours", main="timeline", xlab="time", ylab="tweets")
lines(density(as.integer(geo.df$time), adjust = 1, na.rm=TRUE), col="red")

# graph network
geo.g <- get_interactions(tweets=geo.df$text, screenName=geo.df$screen_name, edge.attribute=geo.df$time, 
                          ascii=TRUE, return.network="retweets", return.graph=TRUE, directed=TRUE)

# check network
geo.g
igraph::list.edge.attributes(geo.g)
E(geo.g)$time <- as.POSIXct(E(geo.g)$edge.attribute, format = "%Y-%m-%d %H:%M:%S")
unique(E(geo.g)$color)
table(E(geo.g)$type)
summary(E(geo.g)$time)
summary(as.POSIXct(E(geo.g)$time, tz = "GMT", origin="1970-01-01"))

# inspect graph (if igraph>0.7.1)
visIgraph(geo.g)

# plot graph
plot.igraph(geo.g,vertex.label=NA,layout=layout_nicely,edge.width=E(geo.g)$weight/10,edge.arrow.size=0.1,vertex.size=1,main="")

###
### dynamic plots with gephi
###

# get retweet network
tweets <- iconv(geo.df$text, to="ASCII", sub="")
tweets <- gsub(" via @", " rt @", tolower(as.character(tweets)))
rt.receiver <- tolower(as.character(geo.df$screen_name[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)]))
rt.sender <- gsub("rt @", "", str_extract(tweets, "rt @[a-zA-Z0-9_]+"))
rt.sender <- rt.sender[!is.na(rt.sender)]
rt.sender[rt.sender==""] <- "<NA>"
rt.receiver[rt.receiver==""] <- "<NA>"

# create edge table
edge.variable.time <- geo.df$time[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)]
edge.variable.hashtag <- as.character(str_extract(tweets, "#[a-z0-9_]+")[grep("rt @[a-zA-Z0-9_]+", tolower(tweets), perl=T)])
rts.df <- data.frame(sender=rt.sender, receiver=rt.receiver, edge.attribute.time.start=edge.variable.time, edge.attribute.time.end=edge.variable.time+2, edge.attribute.hashtag=edge.variable.hashtag, type="retweet", stringsAsFactors=FALSE)
rts.df <- rts.df[!is.na(rts.df$sender),]
rts.df <- rts.df[!rts.df$sender==rts.df$receiver,]
rownames(rts.df) <- NULL

# create node table
nodes.df <- data.frame(userID=as.numeric(as.factor(unique(c(rts.df$sender, rts.df$receiver)))), 
                       username=unique(c(rts.df$sender, rts.df$receiver)), stringsAsFactors = F)
nodes.df$tweets <- geo.df$statuses_count[match(nodes.df$username, tolower(geo.df$screen_name))]
nodes.df$followers <- geo.df$followers_count[match(nodes.df$username, tolower(geo.df$screen_name))]
nodes.df$followees <- geo.df$friends_count[match(nodes.df$username, tolower(geo.df$screen_name))]
nodes.df$listed_count <- geo.df$listed_count[match(nodes.df$username, tolower(geo.df$screen_name))]
nodes.df$latitude <- geo.df$lat[match(nodes.df$username, tolower(geo.df$screen_name))]
nodes.df$longitude <- geo.df$lon[match(nodes.df$username, tolower(geo.df$screen_name))]
nodes.df$latitude[is.na(nodes.df$latitude)] <- geo.df$place_lat[match(nodes.df$username[is.na(nodes.df$latitude)], tolower(geo.df$screen_name))]
nodes.df$longitude[is.na(nodes.df$longitude)] <- geo.df$place_lon[match(nodes.df$username[is.na(nodes.df$longitude)], tolower(geo.df$screen_name))]
rts.df$edge.attribute.time.start[match(nodes.df$username[is.na(nodes.df$start.time)], rts.df$receiver)]

# finish edge table
rts.df$senderID <- nodes.df$userID[match(rts.df$sender, nodes.df$username)]
rts.df$receiverID <- nodes.df$userID[match(rts.df$receiver, nodes.df$username)]

# finish node table
nodes.df$start.time <- rts.df$edge.attribute.time.start[match(nodes.df$username, rts.df$sender)]
nodes.df$start.time[is.na(nodes.df$start.time)] <- rts.df$edge.attribute.time.start[match(nodes.df$username[is.na(nodes.df$start.time)], rts.df$receiver)]
nodes.df$end.time <- max(nodes.df$start.time)+4

# save backup
nodes.df.bck <- nodes.df
rts.df.bck <- rts.df

# convert to integer for Gephi
nodes.df$start.time <- as.integer(nodes.df$start.time)
nodes.df$end.time <- as.integer(nodes.df$end.time)
rts.df$edge.attribute.time.start <- as.integer(rts.df$edge.attribute.time.start) 
rts.df$edge.attribute.time.end <- as.integer(rts.df$edge.attribute.time.end)

# write gexf
write.gexf(nodes=nodes.df[,c("userID", "username")], edges=rts.df[c("senderID", "receiverID")],
           nodesAtt=nodes.df[c("tweets", "followers", "followees", "listed_count", "latitude", "longitude")],
           edgesAtt=rts.df[c("sender", "receiver", "edge.attribute.hashtag", "type")],
           edgeDynamic=rts.df[,c("edge.attribute.time.start", "edge.attribute.time.end")], 
           nodeDynamic=nodes.df[,c("start.time", "end.time")],
           output="tweets_timeline.gexf", tFormat="integer", keepFactors=F, defaultedgetype = "directed", 
           meta = list(creator="M.T.Bastos", description="twitter dynamic network", keywords="gexf graph, R, rgexf") )

###
### open gexf graph in gephi >> install plugins "map of countries" and "geo layout"
###

# save work thus far
save.image(paste(project,Sys.Date(), ".rdata", sep="_"))

# resume work
path.proj <- "C:/Users/bastos/OneDrive - City, University of London/Geo-networks over time"
setwd(path.proj)
load(dir()[grep("rdata", dir())][which.max(as.numeric(gsub("[^0-9,.]", "", grep("rdata", dir(), value=T))))])
lapply(req.packages, require, character.only=T)

###
### dynamic plots within R
###

# backup older graph
geo.g.bck <- geo.g

# recreate igraph object
geo.g.full <- graph.data.frame(rts.df, directed=TRUE, vertices=nodes.df[,c(2,1,3:10)])

# remove nodes withou geo location
geo.g <- induced.subgraph(geo.g.full, which(!is.na(V(geo.g.full)$latitude)))

# get degree distribution
V(geo.g)$degree <- degree(geo.g)

# inspect graph (if igraph>0.7.1)
visIgraph(geo.g)

# load topic description
topics.all <- unique(E(geo.g)$edge.attribute.hashtag)[!is.na(unique(E(geo.g)$edge.attribute.hashtag))]

# select top hashtags (max 10)
if(length(topics.all)>=10) { 
  topic.description <- data.frame(group=paste("Hashtag", rep(1:10)), hashtag=names(sort(table(gsub("#", "", E(geo.g)$edge.attribute.hashtag)), decreasing=T)[1:10]), col.spectrum=c("gold","mediumorchid4","darkgreen","darkblue","darkred","orange","dimgray","deeppink4","seagreen1","steelblue"), stringsAsFactors=F) }
if(length(topics.all)<10) { 
  topic.description <- data.frame(group=paste("Hashtag", rep(1:length(topics.all))), hashtag=names(sort(table(gsub("#", "", E(geo.g)$edge.attribute.hashtag)), decreasing=T)[1:(length(topics.all))]), col.spectrum=c("gold","mediumorchid4","darkgreen","darkblue","darkred","orange","dimgray","deeppink4","seagreen1","steelblue")[1:length(topics.all)], stringsAsFactors=F) }

# color edges by hashtags
E(geo.g)$colors <- as.character(topic.description$col.spectrum[match(gsub("#", "", E(geo.g)$edge.attribute.hashtag), topic.description$hashtag)])
E(geo.g)$colors[is.na(E(geo.g)$colors)] <- "lightgrey"
if(nrow(topic.description)=10) { ncol.plot <- 5 } else  {ncol.plot <- 3 }

# prepare labels
nodeLabels <- V(geo.g)$name
nodeLabels[!nodeLabels %in% names(sort(degree(geo.g),decreasing=T)[1:50])] <- ""
latex <- matrix(c(V(geo.g)$longitude, V(geo.g)$latitude), ncol=2)

# plot on a geographic grid
png(file="geo_plot_network_small.png", type='cairo', width=20,height=20, units='in', res=300)
map(regions = "UK", boundary = T)
plot.igraph(geo.g,layout=layout.norm(as.matrix(data.frame(lon=V(geo.g)$longitude,lat=V(geo.g)$latitude))), rescale = FALSE, 
            xlim = c(min(V(geo.g)$longitude), max(V(geo.g)$longitude)), ylim = c(min(V(geo.g)$latitude), max(V(geo.g)$latitude)), edge.curved = TRUE, vertex.label=nodeLabels,
            edge.width=5, edge.color=E(geo.g)$colors, edge.arrow.size=1,vertex.size=log(V(geo.g)$degree+2)*4, vertex.label.cex=0.4, vertex.label.color="black", add=T)
legend(x="top", tolower(topic.description$hashtag), ncol=ncol.plot, title="hashtags", lwd=20, col=topic.description$col.spectrum, cex=2, bg="transparent")
# legend(x="bottom", c("retweets","mentions"), lwd=30, col=c("darkblue","darkred"), title="edges", cex=5)
# map("county", resolution=300, add=T)
# map('worldHires', 'UK', boundary=T, add=T)
dev.off()

###
### network modeling
###

# model targetted removal of nodes
source("IterateNetwork.R")
iterations <- iterateNetwork(geo.g)

