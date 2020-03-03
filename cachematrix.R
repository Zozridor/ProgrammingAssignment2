## makeCacheMatrix() creates a special matrix object
## cacheSolve() computes the inverse of the special matrix
## If the inverse has already been solved, then cacheSolve
## will retrieve the inverse from makeCacheMatrix

## This function creates a place for the solution to
## be stored away from the global environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse)m<<-inverse
  getInverse <- function()m
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse
       )
}


## If this function is used, it will check if there is 
## a cached solution and retrieve it, saving computing power

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
