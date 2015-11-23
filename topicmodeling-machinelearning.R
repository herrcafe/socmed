library("tm")
library("topicmodels")
library("readODS")
library("RTextTools")

# Read Guardian corpus and create document term matrix (DTM)
stopwords <- scan("stopwords.EN", what = "char", sep = "\n")									# load stopwords
guardian.corpus <- VCorpus(DirSource("guardian", pattern = ".txt"))								# read in articles and prepare 
guardian.corpus <- tm_map(guardian.corpus, content_transformer(tolower))
guardian.corpus <- tm_map(guardian.corpus, removePunctuation, preserve_intra_word_dashes = T)
guardian.corpus <- tm_map(guardian.corpus, removeWords, stopwords)
guardian.corpus <- tm_map(guardian.corpus, stripWhitespace)
guardian.dtm <- DocumentTermMatrix(guardian.corpus)												# create dtm and remove very sparse terms
guardian.dtm <- removeSparseTerms(guardian.dtm, 0.75)											# decrease = fewer terms

# Apply topic modeling with latent dericlet allocation (LDA)
guardian.tm <- LDA(guardian.dtm, k = 6)															# k = number of topics
terms(guardian.tm, 20)																			# get terms
terms(guardian.tm, 10, 0.005)																	# terms with likelihood threshold
topics(guardian.tm, 10, 0.005)																	# topics with a likelihood threshold
str(guardian.tm)																				# what is gamma? what is LL?

# Read NYTimes corpus
nyt.data <- read.ods("nytimes/nyt_ftpg_1996_2006_short.ods", sheet = 1)
colnames(nyt.data) <- c(nyt.data[1, 1:4], "Topic.Code")
nyt.data <- nyt.data[-1,]
nyt.data <- nyt.data[-grep("99", nyt.data$Topic.Code, fixed = T),]								# remove data coded with '99'
rownames(nyt.data) <- 1:nrow(nyt.data)

# Create DTM and container, and apply models
nyt.matrix <- create_matrix(cbind(nyt.data["Title"], nyt.data["Summary"]), language = "english", removeNumbers = TRUE, stemWords = FALSE, weighting = tm::weightTfIdf)
nyt.container <- create_container(nyt.matrix, nyt.data$Topic.Code, trainSize = 1:4000, testSize = 4001:5000, virgin = FALSE)
nyt.models <- train_models(nyt.container, algorithms = c("SVM","MAXENT"))
nyt.results <- classify_models(nyt.container, nyt.models)
nyt.analytics <- create_analytics(nyt.container, nyt.results)
summary(nyt.analytics)
str(nyt.analytics)

nyt.tbl <- as.data.frame(nyt.analytics@algorithm_summary)
nyt.tbl <- nyt.tbl[complete.cases(nyt.tbl),]
nyt.tbl <- nyt.tbl[order(as.numeric(row.names(nyt.tbl))),]
barplot(t(nyt.tbl), beside = T, col = c("steelblue1", "steelblue2", "steelblue3", "tan1", "tan2", "tan3"))




# nytimes_appid <- "5f2d4e9768b4634bd8527a3a86cf51a6:16:47845372"