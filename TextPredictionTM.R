library(tm); library(dplyr); library(magrittr); library(RWeka); 
source("TextPredictionMethods.R")

MIN_NGRAM <- 1
MAX_NGRAM <- 8
SAMPLE_SIZE <- 52000
TEST_SIZE <- 1000

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
    corp %<>% tm_map(content_transformer(function(x){
      if(length(x) >  SAMPLE_SIZE) sample(x, size=SAMPLE_SIZE) else
        x}))
    
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
#save.image()
#TODO validate UTF-8 encoding, validUTF8()

# ShutterStock's list of profane words
profanity <- readLines(
  "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")
profanity <- c (profanity, "fuckin", "damn", "fucking")

corp %<>% prepTextClearPunct(profanity)

## Word Clusters

#create series of term document matrices for each ngram length

terms <- lapply(c(1,seq(ifelse(MIN_NGRAM>1,MIN_NGRAM, 2), 
                        MAX_NGRAM)), myTokenizer, x=corp)

freqs <- lapply(terms, function(x)sort(slam::row_sums(x), decreasing = TRUE))
rm(terms)


words <- createDictionary(freqs[[1]])
if(MIN_NGRAM > 1) freqs[[1]] <- NULL

mark <- markovMatrix(
   freqs[setdiff(seq_along(freqs), ifelse(MIN_NGRAM > 1, 1, 0))], 
   words, singleton_limit = 5, probCalc = "ngram")
marks <- splitMarkovArrays(mark)
rm(mark)
contProbs <- continuationProbabilityMatrix(marks)

#Clean up ngram probabilities by resolving ties via continuation probabiulities
# and dropping predictions that aren't the top rated for their stem
# also drop unigram probabilities because their information is encoded
# in the n-gram porbabilities and their ranking is the dictionary coding
marks <- lapply(marks[seq(2, length(marks))], 
                removeUselessProbs, contProbs = contProbs)
#re-insert blank first entry in list of arrays so indexing still works
marks <- c(alist(NULL), marks)
marks[seq(ifelse(MAX_NGRAM-2 < MIN_NGRAM, MIN_NGRAM, MAX_NGRAM-2), MAX_NGRAM)] <- 
   lapply(marks[seq(ifelse(MAX_NGRAM-2 < MIN_NGRAM, MIN_NGRAM, MAX_NGRAM-2), MAX_NGRAM)],
          trimMarkovArray, quant = .3)

#pred <- fuzzyModel(mark, words, profanity, myTokenizer, prepTextClearPunct)
#multi <- multiModel(marks, contProbs, words, profanity, myTokenizer, prepTextClearPunct)
multiCon <- multiModelFactory(marks, contProbs, freqs[[1]], 
                              profanity, myTokenizer, prepTextClearPunct,
                              multiModel, 
                              myExtract, createDictionary)
saveRDS(multiCon, "multiModelConstructor.RDS")

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

testCorp %<>% prepTextClearPunct(profanity)

testToks <- lapply(seq(2, MAX_NGRAM), myTokenizer, x = testCorp)
testFreqs <- lapply(testToks, function(x)sort(slam::row_sums(x), decreasing = TRUE))

#rateModel(testFreqs, multi, samp = 10, seed = 897)
rateModel(testFreqs, multi$predict, samp = 20, seed = 654564, noisy = F, fuzzSmooth = .003, perpSmooth = 1)

# optimise(optModel, c(-10, 2), smoother = "fuzz", tests=testFreqs, 
#          model=multi, samp=20, seed=897, tol = .1, maximum = TRUE)
# 
optimise(optModel, c(-2, 10), smoother = "perp", tests=testFreqs,
model=multi$predict, samp=25, seed=5555, tol = .1, maximum = TRUE)
# 
#save.image()

frequenciesUnigram <- freqs[[1]]
markovArray3d <- marks_large[[3]]
markovArray4d <- marks_large[[4]]
save(frequenciesUnigram, markovArray3d, markovArray4d, createDictionary, myExtract,
     file = "PresendationObjects.RData")
rm(frequenciesUnigram, markovArray3d, markovArray4d)