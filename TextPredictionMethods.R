library(plyr); library(dplyr); library(magrittr); library(tm); library(RWeka); library(slam); library(fastmatch)

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

prepTextClearPunct <- function(corp, profanity) {
  corp %<>% tm_map(removeNumbers) %>%
    #remove unicode / emoji
    tm_map(content_transformer(function(x)gsub("[^[:graph:][:space:]]", "", x))) %>%
    #separate words run together by periods
    #remove punctuation 
    tm_map(removePunctuation,  preserve_intra_word_dashes = TRUE) %>% 
    tm_map(content_transformer(tolower)) %>%  
    tm_map(removeWords, words=profanity) %>%
    #remove contracted or hyphenated fragments remaining from removed profanity
    tm_map(content_transformer(
      function(x)gsub("(^[[:punct:]]|\\B[[:punct:]])[[:alpha:]]+", "", x))) %>%
    tm_map(stripWhitespace) 
}

myTokenizer <- function(x, max, min=NA, tokensOnly = FALSE) {
  if(is.na(min)) min <- max
  ngtok <- function(y) NGramTokenizer(
    y, Weka_control(max=max, min=min, delimiters=" \r\n\t.,;:\"()?!"))
  if(tokensOnly) return(ngtok(as.character(x[[1]])))
  TermDocumentMatrix(
    x, control = list(
      tolower = FALSE,
      wordLengths = c(1, Inf),
      tokenize = ngtok
    )
  )
}



#optimized version of [.simple_sparse_array
myExtract <- function(m, is) {
  if(all(is.na(is)))return(m)
  mapply(function(a,b) {
    if(is.null(a) || is.na(a)) return(NULL)
    if(a < 0) t <- which(m$i[ , b] != -1*a) else
      t <- which(m$i[ , b] == a)
    if(!length(t)) return(NA)
    t}, a=is, b=seq_along(is), SIMPLIFY=FALSE) %>% 
    extract(!sapply(., is.null)) %>% Reduce(base::intersect, .) ->
    rows
  if(!length(rows) || is.na(rows)[1])return(simple_sparse_zero_array(rep(1, length(is))))
  simple_sparse_array(i = m$i[rows, , drop=FALSE], v = m$v[rows, drop=FALSE])
}


createDictionary <- function(freq, thresh = .9) {
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

#build a vector space projection of MAX_NGRAM dimensions and calculcate
#probabilities for all knonw word combindations
markovMatrix <- function(freqs, dict, singleton_limit = 5, probCalc = "total") {
  library(slam)
  nWords <- dict$n
  nDim <- length(strsplit(names(freqs[[length(freqs)]][1]), " ")[[1]])
  mmat <- simple_sparse_array(matrix(1L, ncol=nDim), 0, 
                              dim = rep(nWords, nDim))
  wt <- dict$wt
  addDim <- function(f) {
    #check for singleton clearance
    t <- strsplit(names(f)[1], split = " ", fixed = TRUE)[[1]]
    if(length(t) > singleton_limit) f <- f[f > 1]
    #convert each word position in the n-gram to a column in a data frame
    w <- strsplit(names(f), split = " ", fixed = TRUE)
    w %<>% do.call(rbind, .) %>% as.data.frame(stringsAsFactors = FALSE) 
    lastWord <- names(w)[length(names(w))]
    w %<>% cbind(f=unname(f))
    #replace words with their dictionary keys and drop rows with OOV words
    w %<>% mutate_each(funs(wt), -f) %>% 
      subset(., complete.cases(.))
    #convert the frequency count into log markov probability
    if(probCalc == "ngram") {
      if(lastWord == "V1") w %<>% mutate(f = log(f) - log(sum(f))) else {
        gvars <- as.list(setdiff(names(w), c(lastWord, "f")))
        w %<>% group_by_(.dots=gvars) %>%
          mutate(f = log(f) - log(sum(f)))
        #prevProbs <- apply(as.matrix(select_(w, .dots = gvars)), 
        #                   1, function(x) {myExtract(mmat, x)$v})
        prevProbs <- mmat[
          as.matrix(select_(w, .dots = gvars)) %>%
            cbind(matrix(1, ncol = length(dim(mmat)) - ncol(.), nrow = nrow(.)))]
        #w %<>% mutate(f = f + prevProbs)
        w$f <- w$f + prevProbs
      }
    } else {
      w %<>% mutate(f = log(f/sum(f)))
    }
    f <- w$f
    #pad with 1's for any additional dimensions in the array
    w %<>% select(-f) %>% as.matrix %>%
      cbind(matrix(1, ncol = length(dim(mmat)) - ncol(.), nrow = nrow(.)))
    mmat[w] <<- unname(f) 
    NULL
  }
  lapply(freqs, addDim)
  mmat
}

#speed access by breaking up markov array for lower order backoff predictions
splitMarkovArrays <- function(markovArray) {
  dims <- length(dim(markovArray))
  lapply(seq(1, dims), function(n){
    i <- as.list(c(rep(-1, n), rep(1, dims - n)))
    myExtract(markovArray, i) %>% drop_simple_sparse_array
  })
}


rateModel <- function(tests, mod, samp=10, fuzzSmooth=0.3545422, perpSmooth = 1.465925, 
                      seed=NA, noisy = F) {
  if(!is.na(seed))set.seed(seed)
  runTest <- function(test, samp) {
    samp <- sample(test, samp)
    src <- sub(" [[:graph:]]+$", "", names(samp))
    answer <- sub("^.* ", "", names(samp))
    result <- mapply(function(x, y){
      p <- mod(x, fuzzSmooth, perpSmooth)
      if(noisy){print(mutate(p, ans=y))}
      if(p[1, "pred"] == y && grepl("NA", p[1, "root"])) return(3)
      if(p[1, "pred"] == y) return(2)
      if(any(na.omit(p[ , "pred"]) == y)) {
        return(1)
      }
      0
    }, x = src, y = answer)
    c(match = sum(result > 1) / length(result), 
      almost = sum(result > 0) / length(result) - sum(result > 1) / length(result), 
      fuzz = sum(result > 2) / length(result))
  }
  lapply(tests, runTest, samp=samp)
}

optModel <- function(param, smoother = c("perp", "fuzz"), 
                     tests, model, samp = 50, seed = 897) {
  smoother = match.arg(smoother)
  if(smoother == "perp") t <- 
      rateModel(tests, model, perpSmooth = param, samp = samp, seed = seed) else
        t <- rateModel(tests, model, fuzzSmooth = 10^param, samp = samp, seed = seed)
  sum(sapply(t, function(x)x[1]))
}

removeUselessProbs <- function(markovArray, contProbs) {
  m <- cbind(as.data.frame(markovArray$i), prob = markovArray$v)
  markdim <- length(dim(markovArray))
  lastword <- names(m)[length(names(m)) - 1]
  gvars <- setdiff(names(m), c(lastword, "prob")) #names(m)[(-1:0 + length(names(m)))*-1]
  #too many ties for simple ranking to be valuable
  #m %<>% group_by_(.dots = gvars) %>% top_n(1, desc(prob))
  m <- ddply(m, gvars, function(g){
    g %<>% filter(prob == max(prob))
    if(nrow(g) > 1) {
      #break ties with total continuation count ties within c.count are broken
      #by unigram prob by presorting by word code (since codes were assinged in
      #decreasing unigram prob order)
      g %<>% arrange_(.dots = lastword) %>% as.data.frame
      o <- order(contProbs[g[ , lastword], markdim], decreasing = TRUE)
      g <- g[o[1], ] 
    }
    g
  })
  simple_sparse_array(as.matrix(select(m, -prob)), m$prob)
}

continuationProbabilityMatrix <- function(markovArrays) {
  pContinuation <- function(markovArray) {
    level = length(dim(markovArray))
    markovArray$i %>% as.data.frame %>% group_by_(.dots=names(.)[level]) %>%
      summarise(n = n()) %>% arrange_(.dots=(names(.)[1])) %>% as.data.frame %>%
      extract( , 2) %>% log %>% subtract(log(length(markovArray$v))) %>% c(0, .)
  }
  lapply(markovArrays, pContinuation) %>% unlist %>%
    matrix(ncol = length(markovArrays), byrow = FALSE)
}

trimMarkovArray <- function(markovArray, quant) {
  q <- quantile(markovArray$v, probs = quant)
  d <- markovArray$v < q
  simple_sparse_array(markovArray$i[!d, ], markovArray$v[!d])
}

getTime <- function(model, test, n = 5) {
  summary(system.time(sapply(sample(names(test), n), model$predict)))[3] / n
}