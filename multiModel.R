library(slam); library(tm); library(dplyr); library(magrittr)

multiModel <- function(markovArrays, continuationProbs, dict,
                       profanity, myTokenizer, prepText, myExtract) {
  
  #looks up the ngram probability in a sparse array
  #if words in the ngram are missing, find ngram
  #probabilities for ngrams with any word in those spots
  #then collapse any with identical words in the remaining spots by
  #adding their probabilities
  getProb <- function(ngram, predict = FALSE, top = FALSE) {
    if(predict) {
      ngram <- c(ngram, NA)
      pdim <- length(ngram)
    } else pdim <- NA
    pred <- myExtract(markovArrays[[length(ngram)]], ngram)
    
    if(!length(pred$v)) return(NA)
    
    pred %<>% rollup(MARGIN = setdiff(seq_along(dim(pred)), pdim), 
                     FUN = function(x) log(sum(exp(x)))) %>%
      drop_simple_sparse_array
    
    if(!predict) return(pred)
    r <- data.frame(word=pred$i[,1], prob=pred$v)
    r <- arrange(r, desc(prob))
    
    if(top) {
      if(top > nrow(r)) top <- nrow(r)
      r <- r[seq_len(top), ]
    }
    
    r
  }
  
  codeNgram <- function(text) {
    prepText(VCorpus(VectorSource(text)), profanity) %>% 
      myTokenizer(1, tokensOnly = TRUE) ->
      toks
    maxLength <- length(dim(markovArrays[[length(markovArrays)]]))
    #discard words beyond the current ngram limit
    if(length(toks) > maxLength - 1) toks <- 
      toks[seq(length(toks) - (maxLength - 2), length(toks))]
    dict$wt(toks)
  }
  
  predictFuzzy <- function(ngram, fuzzSmooth = .003, perpSmooth = 1.29) {
    
    pred <- getProb(ngram, predict = TRUE, top = 1)
    pCont <- pContinuation(ngram)
    while(is.na(pred)[1]) {
      #sequentially remove words starting with the rarest
      del <- which.min(pCont)
      ngram[del] <- NA
      pCont[del] <- 1
      pred <- getProb(ngram, predict = TRUE, top = 1)
    }
    pred %<>% transmute(root = paste(dict$wti(ngram), collapse = " "), 
                        pred = dict$wti(word), 
                        prob = (prob + log(fuzzSmooth) * sum(is.na(ngram))) *
                          -1/((length(ngram)+1) ^ perpSmooth))
    pred
  }
  
  pContinuation <- function(ngram, level = 2) {
    continuationProbs[ngram, level]
    #levels <- seq_along(ngram)
    #levels[1] <- .9999 #unigram continuation probabilities are meaningless
    #mapply(function(w,l)continuationProbs[w, l], w = ngram, l = levels)
    #ngram * -1 
  }
  
  predictMulti <- function(text, fuzzSmooth = .001, perpSmooth = 2) {
    ngram <- codeNgram(text)
    preds <- data.frame()
    while(length(ngram) > 0) {
      preds <- rbind(preds, predictFuzzy(ngram, fuzzSmooth, perpSmooth))
      ngram <- ngram[-1]
    }
    if(nrow(preds)) preds <- arrange(preds, prob)
    preds
  }
  return(list(predict = predictMulti, 
              markovArrays = markovArrays, 
              continuationProbs = continuationProbs, 
              dict = dict,
              profanity = profanity, 
              myTokenizer = myTokenizer, 
              prepText = prepText,
              myExtract))
}

multiModelFactory <- function(markovArrays, continuationProbs, freqs,
                              profanity, myTokenizer, prepText, multiModel, 
                              myExtract, createDictionary) {

  construct <- function() {
    multiModel(markovArrays, continuationProbs, createDictionary(freqs), 
               profanity, myTokenizer, prepText, myExtract)
  }
  list(constructModel = construct, markovArrays, continuationProbs, freqs,
       profanity, myTokenizer, prepText, multiModel, myExtract, createDictionary)
}