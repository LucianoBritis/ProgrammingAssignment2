## Put comments here that give an overall description of what your functions do

# makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse:
makeCacheMatrix  <- function(x = matrix()) {
  m <- NULL
  set <-function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(
    set = set, get  = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

# read readme.md
#B <- matrix(c(1:4), ncol = 2, nrow = 2)
#B1 <- makeCacheMatrix(B)
#cacheSolve(B1)
#identity_matrix <- (B %*% cacheSolve(B1))
#identity_matrix
