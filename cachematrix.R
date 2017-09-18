##Assignment 2: Lexical Scoping

##First function creates a special vector of 4
 #functions that set up the cache for the matrix
##Second function checks if the matrix has already
 #been inversed. If not (NULL), it will be
 #fed into the solve function and retured

##Function 1: making the cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL               
  set <- function(y) {          
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#store 'makeCacheMAtrix' in a variable used below

##Function 2: checking or solving inverse
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

