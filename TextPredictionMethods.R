library(dplyr); library(magrittr); library(tm); library(RWeka); library(slam); library(fastmatch)
#load("bigmark.RData")
#load("bigwords.RData")
prepText <- function(corp, profanity) {
  corp %<>% tm_map(removeNumbers) %>%
    #remove unicode / emoji
    tm_map(content_transformer(function(x)gsub("[^[:graph:][:space:]]", "", x))) %>%
    #remove punctuation that appears outside of words (i.e. preserve contractions, hyphenated words)
    tm_map(content_transformer(function(x){ 
      gsub("\\B[[:punct:]]+\\b|\\b[[:punct:]]+\\B|^[[:punct:]]+|[[:punct:]]+$|\\B[[:punct:]]+\\B", 
           "", x)})) %>% 
    tm_map(removeWords, words=profanity) %>%
    #remove contracted or hyphenated fragments remaining from removed profanity
    tm_map(content_transformer(
      function(x)gsub("\\B[[:punct:]][[:alpha:]]+", "", x))) %>%
    tm_map(stripWhitespace) 
}

myTokenizer <- function(x, max, min=NA, tokensOnly = FALSE) {
  if(is.na(min)) min <- max
  ngtok <- function(y) NGramTokenizer(
    y, Weka_control(max=max, min=min, delimiters=" \r\n\t\\/_"))
  if(tokensOnly) return(ngtok(as.character(x[[1]])))
  TermDocumentMatrix(
    x, control = list(
      tolower = FALSE,
      wordLengths = c(1, Inf),
      tokenize = ngtok
    )
  )
}

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
    ngram <- lapply(ngram, function(x) if(any(is.na(x)))alist(,)[[1]] else x)
    fill <- length(dim(markovArray)) - length(ngram)
    if(fill > 0) ngram <- c(ngram, as.list(rep(1, fill)))
    pred <- do.call(`[`, c(alist(markovArray),ngram))
    #collapse multi-dimensional arrays resulting from unknown word searches
    #by taking geometric mean (formely added their log proababilities,
    # but this seemd to over penalize)
    if(!length(pred$v)) return(NA)
    pred %<>% rollup(MARGIN = setdiff(seq_along(dim(pred)), pdim), FUN = mean) %>%
      drop_simple_sparse_array
    
    if(!predict) return(pred)
    
    r <- data.frame(word=pred$i[,1], prob=pred$v)
    #index one is a placeholder and the retrieved value is actually 
    #probability fo the root ngram due to the matrix structure
    #discard it because it's not relevant
    r <- r[r$word != 1,]
    
    if(top) {
      # sorting may be unnecessary as the matrix was built in descending 
      # probability order and slam appears to return in the same order
      r <- arrange(r, desc(prob))[top,]
      rv <- r$prob
      names(rv) <- r$word
      return(rv)
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
  
  predictFuzzy <- function(text, smooth = .4) {
    ngram <- codeNgram(text)
    if(length(ngram) < MAX_NGRAM - 1) 
      ngram <- c(rep(NA, MAX_NGRAM - length(ngram) - 1), ngram)
    pred <- getProb(ngram, predict = TRUE, top = 1)
    preds <- data.frame(
      root=paste(dict$wti(ngram), collapse = " "), 
      pred=ifelse(is.null(dict$wti(names(pred))), NA, dict$wti(names(pred))), 
      prob=pred, stringsAsFactors=F)
    if(all(is.na(ngram))) return(preds)
    for(i in seq_len(sum(!is.na(ngram)) - 1)) {
      ngram[which.max(ngram)] <- NA
      pred <- getProb(ngram, predict = TRUE, top = 1)
      preds <- rbind(preds, data.frame(
        root=paste(dict$wti(ngram), collapse = " "), 
        pred=ifelse(is.null(dict$wti(names(pred))), NA, dict$wti(names(pred))), 
        prob=pred + log(smooth)*i, stringsAsFactors=F))
    }
    arrange(preds, desc(prob))
  }
  return(predictFuzzy)  
}




createDictionary <- function(freq, thresh = .9) {
  library(fastmatch)
  #creates a dictionary so that various character vectors can be converted 
  #into the same factor-like integer representation 
  #returns a data.frame with the token text as row names for easy lookup
  # i.e. words["hello",] returns the integer level for "hello"
  if(thresh < 1) {
    cutoff <- which((cumsum(freq) / sum(freq)) > thresh)[1]
    #get the rest of the words with the same frequency count so 
    cutoff <- which(freq < freq[cutoff])[1]
    freq <- freq[seq_len(cutoff-1)]
  }
  words <- c("", names(freq))
  wt <- function(w) fmatch(w, words)
  wti <- function(w) words[as.numeric(w)]
  list(words=words, wti=wti, wt=wt, n=length(words))
}


markovMatrix <- function(freqs, dict, singleton_limit = 5) {
  library(slam)
  nWords <- dict$n
  nDim <- length(strsplit(names(freqs[[length(freqs)]][1]), " ")[[1]])
  mmat <- simple_sparse_array(matrix(1L, ncol=nDim), 0, 
                              dim = rep(nWords, nDim))
  wt <- dict$wt
  addDim <- function(f) {
    #convert each word position in the n-gram to a column in a data frame
    w <- strsplit(names(f), split = " ", fixed = TRUE)
    w %<>% do.call(rbind, .) %>% as.data.frame(stringsAsFactors = FALSE) 
    lastWord <- names(w)[length(names(w))]
    w %<>% cbind(f=unname(f))
    #replace words with their dictionary keys and drop rows with OOV words
    w %<>% mutate_each(funs(wt), -f) %>% 
      subset(., complete.cases(.))
    #convert the frequency count into log markov probability, bind to 
    #the n-gram data frame to simiplify subsetting
    if(lastWord == "V1" || length(freqs)==1) w$f <- log(w$f/sum(w$f)) else 
      w %<>% group_by_(.dots=as.list(setdiff(names(.), c(lastWord, "f")))) %>%
        mutate(f = log(f/sum(f)))
    #singletons with prob == 1 would be lost in sparse array because zero values
    #are treated as empty, so give them a near zero value limit higher order
    #singletons; they are less valuable as all of their information is usually
    #captured by the lower order ngrams
    if(ncol(w) - 1 <= singleton_limit) f <- ifelse(w$f == 0, -0.0001, w$f) else {
      w %<>% filter(f != 0)
      f <- w$f
    }
    #pad with 1's for any additional dimensions in the array
    w %<>% select(-f) %>% as.matrix %>%
      cbind(matrix(1, ncol = length(dim(mmat)) - ncol(.), nrow = nrow(.)))
    mmat[w] <<- unname(f) 
    NULL
  }
  lapply(freqs, addDim)
  mmat
}


rateModel <- function(tests, mod, samp=10, smooth=.4, seed=NA) {
  runTest <- function(test, samp) {
    if(!is.na(seed))set.seed(seed)
    samp <- sample(test, samp)
    src <- sub(" [[:graph:]]+$", "", names(samp))
    answer <- sub("^.* ", "", names(samp))
    result <- mapply(function(x, y){
      p <- mod(x, smooth)
      if(p[1, "pred"] == y && grepl("NA", p[1, "root"])) return(3)
      if(p[1, "pred"] == y) return(2)
      if(any(na.omit(p[ , "pred"]) == y)) return(1)
      0
    }, x = src, y = answer)
    c(match = sum(result > 1) / length(result), 
      almost = sum(result > 0) / length(result) - sum(result > 1) / length(result), 
      fuzz = sum(result > 2) / length(result))
  }
  lapply(tests, runTest, samp=samp)
}
