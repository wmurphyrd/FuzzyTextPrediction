fuzzyModel <- function(markovArray, dict, profanity, myTokenizer, prepText) {
  
  getProb <- function(ngram, predict = FALSE, top = FALSE) {
    #search <- seq(1,dim(markovArray)[1])
    ngram <- as.list(ngram)
    if(predict) {
      #ngram <- c(ngram, list(search))
      ngram <- c(ngram, NA)
      pdim <- length(ngram)
    } else pdim <- NA
    #if any words are unknown, pull all possible words in their place
    #ngram <- lapply(ngram, function(x) if(any(is.na(x)))alist(,)[[1]] else x)
    fill <- length(dim(markovArray)) - length(ngram)
    if(fill > 0) ngram <- c(ngram, as.list(rep(1, fill)))
    #pred <- do.call(`[`, c(alist(markovArray),ngram))
    pred <- myExtract(markovArray, ngram)
    #collapse multi-dimensional arrays resulting from unknown word searches
    #by taking geometric mean (formely added their log proababilities,
    # but this seemd to over penalize)
    if(!length(pred$v)) return(NA)
    pred %<>% rollup(MARGIN = setdiff(seq_along(dim(pred)), pdim), FUN = 
                       function(x) log(sum(exp(x)))) %>%
      drop_simple_sparse_array
    
    if(!predict) return(pred)
    if(length(pred) == 1) return(NA) #if no full-order matches were found
    r <- data.frame(word=pred$i[,1], prob=pred$v)
    #index one is a placeholder and the retrieved value is actually 
    #probability fo the root ngram due to the matrix structure
    #discard it because it's not relevant
    r <- r[r$word != 1,]
    #if multiple predictions lead to the same word, combine their probabilities
    #r %<>% group_by(word) %>% summarize(prob = log(sum(exp(prob)))) %>%
    r <- arrange(r, desc(prob))
    
    if(top) {
      # sorting may be unnecessary as the matrix was built in descending 
      # probability order and slam appears to return in the same order
      if(top > nrow(r)) top <- nrow(r)
      r <- r[seq_len(top), ]
      #rv <- r$prob
      #names(rv) <- r$word
      #return(rv)
    }
    
    r
  }
  
  codeNgram <- function(text) {
    prepText(VCorpus(VectorSource(text)), profanity) %>% 
      myTokenizer(1, tokensOnly = TRUE) ->
      toks
    #discard words beyond the current ngram limit
    if(length(toks) > MAX_NGRAM - 1) toks <- 
        toks[seq(length(toks) - (MAX_NGRAM - 2), length(toks))]
    dict$wt(toks)
  }
  
  getAllNgrams <- function(ngram) {
    l <- length(ngram)
    if(l == 2) return(list(matrix(ngram, ncol=1)))
    lapply(seq(ifelse(MIN_NGRAM < 2, 2, MIN_NGRAM), 
               ifelse(l > (MAX_NGRAM - 1), (MAX_NGRAM - 1) - 1, l - 1)), 
           function(ngramLength){
             sapply(seq(l, ngramLength), function(endPosition) {
               #browser()
               ngram[seq(endPosition - ngramLength + 1, endPosition)]
             })
           })
  }
  
  predictFuzzy <- function(text, smooth = .4) {
    ngram <- codeNgram(text)
    if(length(ngram) < MAX_NGRAM - 1) 
      ngram <- c(rep(NA, MAX_NGRAM - length(ngram) - 1), ngram)
    pred <- getProb(ngram, predict = TRUE, top = 10)
    
    while(is.na(pred)[1]) {
      #sequentially remove words starting with the rarest
      ngram[which.max(ngram)] <- NA
      pred <- getProb(ngram, predict = TRUE, top = 10)
    }
    
    pred %<>% transmute(root = paste(dict$wti(ngram), collapse = " "), 
                        pred = dict$wti(word), prob = prob)
    #arrange(pred, desc(prob))
    pred
  }
  
  predictMulti <- function(text) {
    ngram <- codeNgram(text)
    pred <- getProb(ngram, predict = TRUE)
    while(is.na(pred)[1]) {
      ngrams <- getAllNgrams(ngram)
      #get bigram probs
      bigrams <- ngrams[[1]]
      bps <- sapply(seq_len(ncol(bigrams)), function(x)getProb(bigrams[,x]))
      bpna <- which(is.na(bps))
      #if any unknown bigrams exist, fuzz over the second terms of them
      if(length(bpna)) ngram[length(ngram) + 1 - bpna] <- NA else
        #othewise, fuzz over the second term of the rarest bigram
        ngram[length(ngram) + 1 - which.min(bps)] <- NA
      #try again to a probability
      pred <- getProb(ngram, predict = TRUE)
    }
    pred %<>% transmute(root = paste(dict$wti(ngram), collapse = " "), 
                        pred = dict$wti(word), prob = prob)
    pred
  }
  return(list(fuzz=predictFuzzy, multi=predictMulti))
}
