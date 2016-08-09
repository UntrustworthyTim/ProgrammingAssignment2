## makeCacheMatrix  Creates a matrix object that has the ability to cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  i = NULL
  
  set <- function(y){
    x <<- y
    i <<- Null
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## cacheSolve  Calculates the inverse of the matrix created by makeCacheMatrix. 
##If the inverse has already been calculated, this function will return the cached data instead.


cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
        ## Return a matrix that is the inverse of 'x'

