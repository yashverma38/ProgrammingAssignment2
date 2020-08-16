## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   #creating a cache matrix function, defaulting x to a matrix
  inv <- NULL                                 # inv is null if no data is present
  set <- function(y) {                        # creating a set function
    x <<- y                                   
   inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,                #This is done to get $ operator in next function
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                #This function solves the data is not already done before
  m <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
