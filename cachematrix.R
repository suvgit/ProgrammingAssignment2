## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix, which is a list containing functions to 
## set the value of the matrix, get the value of the matrix, set the inverse 
## and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function(){x} 
  setinverse <- function(inverse){i <<- inverse} 
  getinverse <- function() {i}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## gets the cached inverse of the given matrix if not null. 
## Otherwise calculates the inverse if the cached data is null

cacheSolve <- function(xx, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- xx$getinverse()
  if(!is.null(i)){
    message("Getting cached data!!")
    return(i)
  }
  data <- xx$get()
  i <- solve(data)
  xx$setinverse(i)
  i
}

