probTables <- function(freqs) {
  c(
    # 1-gram probabilities are the frequency of the word
    list(data.frame(src="",
                    pred=names(freqs[[1]]),
                    prob=freqs[[1]]/sum(freqs[[1]]), row.names = NULL) %>% arrange(desc(prob))),
    # >1-gram probabilities are the frequency of the final word given the rest
    lapply(freqs[2:length(freqs)], function(f) {
      data.frame(src = tolower(sub(" [[:graph:]]+$", "", names(f))),
                 pred = sub("^.* ", "", names(f)), predCount = f) %>% 
        #group any pred-src combos that are identifcal after the tolower
        group_by(pred, src) %>% summarise(predCount = sum(predCount)) %>%
        #calculate proportion of each src that is followed by each pred in the set
        group_by(src) %>% mutate(srcTotal = sum(predCount)) %>% 
        mutate(prob = predCount/srcTotal) %>% ungroup %>%
        set_rownames(NULL) %>% select(src, pred, prob) %>% 
        arrange(src, desc(prob))
    })
  )
}

predictNextWord <- function(text, probTables) {
  prepText(VCorpus(VectorSource(text)), profanity) %>% 
    myTokenizer(1, tokensOnly = TRUE) %>% tolower ->
    text
  l <- testLength <- length(text)
  if(testLength > MAX_NGRAM - 1) testLength <- MAX_NGRAM - 1
  while(testLength > 0) {
    t <- do.call(paste, lapply(text[seq_len(testLength)+(l-testLength)], eval))
    i <- t == probTables[[testLength + 1]]$src
    if(any(i)) return(probTables[[testLength + 1]][i, ])
    else testLength <- testLength - 1
  }
  # if no matches, just guess the most popular words
  probTables[[1]][1:3, ]
}

ngramProb <- function(ngram, markovArray, words, backoff = .4) {
  prepText(VCorpus(VectorSource(ngram)), profanity) %>% 
    myTokenizer(1, tokensOnly = TRUE) ->
    toks
  #discard words beyond the current ngram limit
  if(length(toks) > MAX_NGRAM) toks <- 
      toks[seq(length(toks) - (MAX_NGRAM - 1), length(toks))]
  toks <- sapply(seq(2,length(toks)), function(x)toks[seq(1, x)])
  codes <- lapply(toks, fmatch, table=words)
  probs <- sapply(codes, function(x) {
    #browser()
    getProb(ngram = x, markovArray = markovArray) })
  
  probs <- ifelse(is.na(probs), c(0, probs)*backoff, probs)
  probs <- c(probs, sum(probs))
  names(probs) <- c(sapply(toks, paste, collapse = " "), "total")
  probs
}

testall <- function(root, answers) {
  lapply(answers, function(x) {
    ngramProb(paste(root, x, collapse=" "), mark, words)
  })
}



predictNextWordMarkov <- function(text, markovArray, words, smooth = .4, skipProb = FALSE) {
  prepText(VCorpus(VectorSource(text)), profanity) %>% 
    myTokenizer(1, tokensOnly = TRUE) ->
    toks
  #discard words beyond the current ngram limit
  if(length(toks) > MAX_NGRAM - 1) toks <- 
      toks[seq(length(toks) - (MAX_NGRAM - 2), length(toks))]
  toks <- fmatch(toks, words)
  fuzzPred <- function(ngram) {
    pred <- getProb(ngram, markovArray, predict = TRUE, top = 1)
    cat("retrieved ", pred, " for ", ngram, "\n")
    attempt <- 1
    while(pred && attempt < length(ngram)) {
      # try again with the least likely word fuzzed
      ngram[stemRanks[attempt]] <- NA
      pred <- getProb(ngram, markovArray, predict = TRUE, top = 1)
      cat("fuzzing attempt no.", attempt, "yielded", pred, "from", ngram, "\n")
    }
    if(is.na(pred)) stop("no matches found")
    r <- sum(c(stem, pred))
    names(r) <- names(pred)
    r
    
  }
  #nextWords(toks)
  #getProb(toks)
  stems <- sapply(seq(2, length(ngram)), function(x){getProb(ngram[seq_len(x)])})
  
  stemRanks <- order(stems)
  sapply(seq(1, length(toks)), function(x) {
    #browser()
    ngram <- toks[seq(x, length(toks))]
    p <- rankPrediction(ngram)
    names(p) <- c(ngram, as.numeric(names(p))) %>% extract(words, .) %>% paste(collapse=" ")
    p * -1/length(ngram)
  }) %>% exp
  
}