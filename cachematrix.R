## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix selts the default value of m variable used to store the mean as Null, assigns the value of y to x which is used to 
## store the matrix by means of the set function , get function retrieves the value of the ,setinverse to set the inverse of the matrix and
## get to retrieve the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matix passed in the makeCacheMatrix function, it retrieves the value from the cache if
## not calculated before by using the if loop , otherwise calculates the inverse of the matrix using the solve function and returns 
## its value

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
