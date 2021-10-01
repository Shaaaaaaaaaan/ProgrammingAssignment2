## Sean Justin O. Samson
## Math144 - E02
## Mapua University

## The two functions are used to create a special variable that saves a matrix and caches the inverse of the said matrix. 

## The first function, makeCacheMatrix creates a special “matrix”
## Setting and getting the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
          x <<- y
          a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse         #function to acquire the matrix
  getinverse <- function() a
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function determines the inverse of the special “matrix” created by makeCacheMatrix function above. 
## If theres a situation where the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {        #function to get cache data
  a <- x$getinverse()
  if (!is.null(a)) {                    #fuction to determine if the inverse matrix is null
          message("acquiring cached data")
          return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
