library(tm); library(dplyr); library(magrittr)
source("TextPredictionMethods.R")

MIN_NGRAM <- 5
MAX_NGRAM <- 5
SAMPLE_SIZE <- 50000
TEST_SIZE <- 100

if(!file.exists("Data/Coursera-SwiftKey.zip")) download.file(
  "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
  "Data/Coursera-SwiftKey.zip")

if(length(list.files("Data/TrainData/", pattern="^en_US.+txt$"))==0 || 
   exists("reload_docs")) {
  corp <- PCorpus(
    ZipSource("Data/Coursera-SwiftKey.zip", pattern="en_US", recursive = TRUE), 
    dbControl = list(dbName = "Data/enCorpus.db", dbType="DB1")
  )
  if(!get0("get_all", ifnotfound = FALSE)) {
    set.seed(123)
    corp %<>% tm_map(content_transformer(function(x)sample(x, size=SAMPLE_SIZE)))
    
    intest <- sample(SAMPLE_SIZE, TEST_SIZE*2)
    holdout <- intest[seq(TEST_SIZE+1, TEST_SIZE*2)]
    intest <- intest[seq_len(TEST_SIZE)]
    if(!all(dir.exists(paths=c("Data/TestData/", "Data/TrainData/", 
                               "Data/ValidateData")))) {
      dir.create("Data/TestData/", showWarnings = FALSE)
      dir.create("Data/TrainData/", showWarnings = FALSE)
      dir.create("Data/ValidateData/", showWarnings = FALSE)
    }
    #extract the test set. has to be done indirectly to avoid altering the PCorpus
    extractSubset <- function(sourceCorp, i, destFolder) {
      sub <- VCorpus(DataframeSource(data.frame(t(sapply(sourceCorp, function(x) {
        as.character(x)[i] })), stringsAsFactors = FALSE)), 
        readerControl = list(reader=readPlain))
      names(sub) <- names(sourceCorp)
      writeCorpus(sub, path=destFolder)
    }
    mapply(extractSubset, i=c(intest, holdout), 
           destFolder = c("Data/TestData/",  "Data/ValidateData"),
           MoreArgs = list(sourceCorp = corp))
    
    writeCorpus(tm_map(corp, content_transformer(
      function(x)x[-c(intest, holdout)])), path="Data/TrainData/")
    rm(corp, intest, holdout)
  }
}

if(!exists("corp"))corp <- VCorpus(DirSource("Data/TrainData/", encoding = "UTF-8", pattern="^en_US.+txt$"))
# Split each line into its own document to facilitate clustering analysis
#corp <- VCorpus(VectorSource(sapply(corp, as.character)))

#TODO validate UTF-8 encoding, validUTF8()

# ShutterStock's list of profane words
profanity <- readLines(
  "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")
profanity <- c (profanity, "fuckin", "damn", "fucking")

corp %<>% prepText(profanity)

## Word Clusters

#create series of term document matrices for each ngram length
library(RWeka)


# terms <- lapply(1:5, function(i){
#   tok <- function(x)NGramTokenizer(
#     x, Weka_control(max=i, min=i, delimiters=" \r\n\t\\/_"))
#   TermDocumentMatrix(corp, control = list(tokenize=tok, tolower=FALSE, 
#                                           wordLengths=c(1, Inf)))
# })
terms <- lapply(c(1,seq(ifelse(MIN_NGRAM>1,MIN_NGRAM, 2), 
                        MAX_NGRAM)), myTokenizer, x=corp)

print(sapply(terms, nrow))
# terms <- lapply(terms, removeSparseTerms, sparse=.9996); 
# print(sapply(terms, nrow))


#rwng <- function(x)NGramTokenizer(x, 
#                                  Weka_control(max=3, min=2, delimiters=" \r\n\t\\/_"))
#tdmn <- termFreq(corp[[1]], control = list(tokenize=rwng))

#d <- PlainTextDocument(do.call("c", lapply(corp, as.character)))

freqs <- lapply(terms, function(x)sort(slam::row_sums(x), decreasing = TRUE))

words <- createDictionary(freqs[[1]])
if(MIN_NGRAM > 1) freqs[[1]] <- NULL
mark <- markovMatrix(freqs, words, singleton_limit = 5)
save(mark, words, file = "fullmark.Rdata")
#TODO fat trimming
# * Remove ngrams with only 1 occurence
# * Remove rare words


#library(wordcloud); library(RColorBrewer)
#cloud_terms <- freqs[[1]][1:300]
#wordcloud(names(cloud_terms), sqrt(cloud_terms), colors = brewer.pal(8, "Dark2"), random.order = F)

# tmat <- do.call(rbind, mapply(function(t, i, n){
#   as.matrix(t[names(freqs[[i]])[seq_len(n)], ])
# }, t=terms, i=seq_along(terms), n=c(25, 20, 15, 10, 5)))
# 
# plot(hclust(dist(tmat)))

#tabs <- probTables(freqs)

testCorp <- VCorpus(DirSource(
  "Data/TestData/", encoding = "UTF-8", pattern="^en_US.+txt$"))

testCorp %<>% prepText(profanity)

testToks <- lapply(seq(2, MAX_NGRAM), myTokenizer, x = testCorp)
testFreqs <- lapply(testToks, function(x)sort(slam::row_sums(x), decreasing = TRUE))

