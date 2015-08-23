library(tm); library(dplyr); library(magrittr); library(RWeka); library(pryr)
source("TextPredictionMethods.R")

MIN_NGRAM <- 1
MAX_NGRAM <- 5
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

optims <- data.frame(
  Optimization = "None", 
  Description=sprintf("Full %d-d array for %1.1e unique words",
                      MAX_NGRAM, length(freqs[[1]])),
  Memory = sprintf("Approx. %1.1e TB", length(freqs[[1]])^5 * object_size(-1.5)/1024/1024/1024/1024),
  `Prediction Time` = "Not Available", stringsAsFactors = FALSE)

words <- createDictionary(freqs[[1]])

optims %<>% rbind(c("Dictionary Limit", 
                  "Retain only enough unique words to cover 90% of the corpus",
                  sprintf("Approx. %1.1e TB", words$n^5 * object_size(-1.5)/1024/1024/1024/1024),
                  "Not Available"))

if(MIN_NGRAM > 1) freqs[[1]] <- NULL

mark <- markovMatrix(
   freqs[setdiff(seq_along(freqs), ifelse(MIN_NGRAM > 1, 1, 0))], 
   words, singleton_limit = 5, probCalc = "ngram")
marks <- splitMarkovArrays(mark)
#rm(mark)
contProbs <- continuationProbabilityMatrix(marks)

slowmodel <- slowMulti(mark, contProbs, words, profanity, myTokenizer, prepTextClearPunct)



mc_large <- multiModelFactory(marks_large, contProbs, freqs[[1]], profanity, myTokenizer, prepTextClearPunct, multiModel, myExtract, createDictionary)
m_large <- mc_large$constructModel()
rm(mc_large)

optims %<>% rbind(c("Sparse Array", 
                    "Use simple_sparse_array from package slam so that the vast number of 0 probability values do not need to be stored",
                    sprintf("%3.0f MB", object_size(slowmodel)/1024/1024),
                    sprintf("%1.2f seconds",getTime(slowmodel, testFreqs[[4]], 3))))

optims %<>% rbind(c("Custom Access Method", 
                    "Replace [.simple_sparse_array with a custom optimised method for accessing array values",
                    sprintf("%3.0f MB", object_size(slowmodel)/1024/1024),
                    sprintf("%1.2f seconds",getTime(m_large, testFreqs[[4]]))))
rm(m_large, slowmodel, mark)

#Clean up ngram probabilities by resolving ties via continuation probabiulities
# and dropping predictions that aren't the top rated for their stem
# also drop unigram probabilities because their information is encoded
# in the n-gram porbabilities and their ranking is the dictionary coding
marks <- lapply(marks[seq(2, length(marks))], 
                removeUselessProbs, contProbs = contProbs)
#re-insert blank first entry in list of arrays so indexing still works
marks <- c(alist(NULL), marks)

#trim 30% least likely predictions for 3+-grams
marks[seq(ifelse(MAX_NGRAM-2 < MIN_NGRAM, MIN_NGRAM, MAX_NGRAM-2), MAX_NGRAM)] <- 
   lapply(marks[seq(ifelse(MAX_NGRAM-2 < MIN_NGRAM, MIN_NGRAM, MAX_NGRAM-2), MAX_NGRAM)],
          trimMarkovArray, quant = .3)


multiCon <- multiModelFactory(marks, contProbs, freqs[[1]], 
                              profanity, myTokenizer, prepTextClearPunct,
                              multiModel, 
                              myExtract, createDictionary)
saveRDS(multiCon, "multiModelConstructor.RDS")
multi <- multiCon$constructModel()

optims %<>% rbind(c("Array Trimming",
                    "Drop probabilities for predictions beyond the 1st for each n-gram root and trim bottom 30% of high order n-gram probabilities",
                    sprintf("%3.0f MB", object_size(multi)/1024/1024),
                    sprintf("%1.2f seconds",getTime(multi, testFreqs[[4]]))))

testCorp <- VCorpus(DirSource(
  "Data/TestData/", encoding = "UTF-8", pattern="^en_US.+txt$"))

testCorp %<>% prepTextClearPunct(profanity)

testToks <- lapply(seq(2, MAX_NGRAM), myTokenizer, x = testCorp)
testFreqs <- lapply(testToks, function(x)sort(slam::row_sums(x), decreasing = TRUE))

a<- rateModel(testFreqs[4], multi$predict, samp = 25, seed = 5555, noisy = F,
          fuzzSmooth = 1, perpSmooth = 1)
tunes <- as.data.frame(rbind(a[[1]]))
names(tunes) <- c("Total Match Rate", "Matches missed due to backoff tuning", "Matches made through fuzzy smoothing")

optimise(optModel, c(-5, 2), smoother = "fuzz", tests=testFreqs[4], 
         model=multi$predict, samp=25, seed=5555, tol = .5, maximum = TRUE)

 
optimise(optModel, c(-1, 3), smoother = "perp", tests=testFreqs[4],
model=multi$predict, samp=25, seed=5555, tol = .01, maximum = TRUE)



validCorp <- VCorpus(DirSource(
  "Data/ValidateData/", encoding = "UTF-8", pattern="^en_US.+txt$"))

validCorp %<>% prepTextClearPunct(profanity)

validToks <- lapply(seq(2, MAX_NGRAM), myTokenizer, x = testCorp)
validFreqs <- lapply(testToks, function(x)sort(slam::row_sums(x), decreasing = TRUE))







frequenciesUnigram <- freqs[[1]]
markovArray3d <- marks_large[[3]]
#markovArray4d <- marks_large[[4]]
validationData <- validFreqs[[4]]
save(frequenciesUnigram, markovArray3d, createDictionary, myExtract,
     optims, validationData, rateModel, multiCon,
     file = "PresendationObjects.RData")
rm(frequenciesUnigram, markovArray3d, markovArray4d)



