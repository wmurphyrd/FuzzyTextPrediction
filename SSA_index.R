## FIXME we now have drop_simple_sparse_array
`[.simple_sparse_array` <-
  function(x, ...)
  {
    print("myfunc")
    ## Note that calling x[] with a simple sparse array x will call the
    ## subscript method with args x and missing ...
    na <- nargs()
    if((na == 1L) || (na == 2L) && missing(..1))
      return(x)
    
    nd <- length(x$dim)
    
    ## Note there is a limit to representing integer numbers as 
    ## doubles.
    spos <- function(i) {
      if(!nrow(i)) 
        return(vector(mode = typeof(i), length = 0L))
      ## Scalar positions of array index matrices i in the usual row
      ## major ordering of arrays.
      if(ncol(i) > 1L) {
        ## This may not work on systems with BLAS issues
        ## as.vector(tcrossprod(c(1L, cumprod(x$dim[-nd])), i - 1L)) + 1L
        1L + row_sums((i - 1L) * rep(c(1L, cumprod(x$dim)[-nd]), 
                                     each = nrow(i)))
      } else
        as.vector(i)
    }
    
    if(na == 2L) {
      i <- ..1
      ## Single index subscripting.
      if(is.logical(i))
        stop("Logical subscripting currently not implemented.")
      else if(is.character(i))
        stop("Character subscripting currently not implemented.")
      else if(!is.matrix(i)) {
        ## 52-bit safe
        if(prod(x$dim) > 4503599627370496)
          stop("Numeric vector subscripting disabled for this object.")
        ## Shortcut
        if(!length(i)) 
          return(vector(mode = typeof(x$v), length = 0L))
        ## Let's hope we have a vector.
        ## What if we have both negatives and positives?
        if(is.double(i))
          i <- trunc(i)
        if(all(i >= 0, na.rm = TRUE)) {
          i <- i[i > 0]
          out <- vector(mode = typeof(x$v), length = length(i))
          if(length(out)) {
            ## Missing values.
            is.na(i) <- i > prod(x$dim)
            is.na(out) <- is.na(i)
            i <- fmatch(i, spos(x$i), 0L)
            out[i > 0L] <- x$v[i]
          }
        } else if(!any(is.na(i)) && all(i <= 0)) {
          if(prod(x$dim) > 16777216L)
            stop("Negative vector subsripting disabled for this object.")
          out <- vector(mode = typeof(x$v), prod(x$dim))
          out[spos(x$i)] <- x$v
          ## NOTE this fails if NAs are introduced by 
          ##	coercion to integer. 
          out <- out[i]
        }
        else stop("Cannot mix positive and negative subscripts.")
      }
      else {
        ## Shortcut
        if(!nrow(i)) 
          return(vector(mode = typeof(x$v), length = 0L))
        ## Ignore dimensions. 
        if(ncol(i) != nd) 
          return(do.call("[.simple_sparse_array", 
                         list(x = x, as.vector(i))))
        ## Note that negative values are not allowed in a matrix
        ## subscript.
        if(is.double(i))
          i <- trunc(i)
        if(any(i < 0, na.rm = TRUE))
          stop("Invalid subscript.")
        k <- .Call(R_all_row, i > 0, FALSE)
        i <- i[k, , drop = FALSE]
        out <- vector(mode = typeof(x$v), length = nrow(i))
        if(length(out)) {
          if(any(i > rep(x$dim, each = nrow(i)), na.rm = TRUE))
            stop("subscript out of bounds")
          ## Missing values.
          k <- k[k]
          is.na(out) <- is.na(k)
          rm(k)
          ## This is not really the fastest way to match rows, but is
          ## there an obvious better one?
          ## pos <- match(split(i, row(i)), split(x$i, row(x$i)), 0L)
          storage.mode(i) <- "integer"
          i <- .Call(R_match_matrix, x$i, i, 0L)[[2L]]
          out[i > 0L] <- x$v[i]
        }
      }
    }
    else {
      if(na != (nd + 1L))
        stop("Incorrect number of dimensions.")
      ## Get indices.
      args <- vector("list", na - 1L)
      for(k in seq_along(args)) {
        n <- as.name(sprintf("..%i", k))
        if (!do.call(missing, list(n)))
          args[[k]] <- eval(n)
      }
      ## Ready to go.
      dx <- x$dim
      pos <- rep.int(TRUE, length(x$v))
      ind <- vector("list", length = nd)
      for(k in seq_len(nd)) {
        i <- args[[k]]              # Given indices.
        if(is.null(i)) {
          ind[[k]] <- seq_len(dx[k])
          next
        }
        else if(!is.numeric(i))
          stop("Only numeric multi-index subscripting is implemented.")
        else {
          if (any(is.na(i)))
            stop("NA indices currently not allowed")
          if(is.double(i))
            i <- trunc(i)
          if(all(i >= 0)) {
            i <- i[i > 0]
            if(any(duplicated(i)))
              stop("Repeated indices currently not allowed.")
            if(any(i > dx[k]))
              stop("subscript out of bounds")
          } else if(all(i <= 0))
            ## NOTE this fails if NAs are introduced by 
            ##	    coercion to integer. 
            i <- seq_len(dx[k])[i]
          else
            stop("Cannot mix positive and negative subscripts.")
          ind[[k]] <- i
          dx[k] <- length(i)
          j <- fmatch(x$i[, k], i, 0L)
          x$i[j > 0L, k] <- seq_along(i)[j]
          pos <- pos & (j > 0L)
        }
      }
      if(!is.null(dnx <- x$dimnames))
        dnx[] <- Map("[", dnx, ind)
      out <- simple_sparse_array(x$i[pos, , drop = FALSE], x$v[pos],
                                 dx, dnx)
    }
    
    out
    
  }

myExtract <- function(m, is) {
  mapply(function(a,b) {
    if(is.null(a) || is.na(a)) return(NULL)
    t <- which(m$i[ , b] == a)
    if(!length(t)) return(NULL)
    else t
  }, a=is, b=seq_along(is), SIMPLIFY=FALSE) %>% extract(!sapply(., is.null)) %>%
    Reduce(base::intersect, .) ->
    rows
  simple_sparse_array(i = m$i[rows, , drop=FALSE], v = m$v[rows, drop=FALSE])
}

myExtract <- function(m, is) {
  mapply(function(a,b) {
    if(is.null(a) || is.na(a)) return(NULL)
    t <- which(m$i[ , b] == a)
    if(!length(t)) return(NULL)
    else t
  }, a=is, b=seq_along(is), SIMPLIFY=FALSE) %>% extract(!sapply(., is.null)) %>%
    Reduce(base::intersect, .) ->
    rows
  if(!length(rows))return(simple_sparse_zero_array(rep(1, length(is))))
  simple_sparse_array(i = m$i[rows, , drop=FALSE], v = m$v[rows, drop=FALSE])
}