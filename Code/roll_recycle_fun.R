# custom rolling window function that indexes vector in a circular fashion,
# returning observations in vector within window of width = size, number of windows = n,
# taken every by'th observation

roll.recycle <- function(vec, size, n, by=1, index=FALSE){
  vec.length <- length(vec)
  vals <- (seq(from = 1-floor(size/2), by = 1, length.out = 9) + rep(seq(from=0, by=by, length.out=n), each=size))%%vec.length
  use.inds <- matrix(vals, nrow=size)
  use.inds[use.inds==0] <- vec.length
  if(index){
    use.inds
  }else{
    matrix(vec[use.inds], nrow=size)
  }
  
}