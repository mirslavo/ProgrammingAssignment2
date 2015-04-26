## components for PA2

## cacheable matrix
makeCacheMatrix <- function(m = matrix()) {
  im <- NULL
  set <- function(m_new) {
    m <<- m_new
    im <<- NULL
  }
  get <- function() m
  setim <- function(im_new) im <<- im_new
  getim <- function() im
  list(set = set, get = get, setim = setim, getim = getim)
}

## maintain inverse matrix considering cached data
cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'
  im <- m$getim()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  im <- solve(m$get(), ...)
  m$setim(im)
  im
}
