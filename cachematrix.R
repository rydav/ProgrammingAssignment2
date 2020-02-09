##Functions create and cache the inverse of a submitted matrix

##The makeCacheMatrix function creates a matrix object that then will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix. It will return if the inverse from the cache if it has already been computed

cacheSolve <- function(x, ...) {
    inv <- x$getinverse() 
    if(!is.null(inv)) { ##if inverse exists in cache, return inverse
      message("getting cached data")
      return(inv)
    }
    mat <- x$get() ##if inverse does not exist in cahce, set inverse
    i <- solve(mat, ...)
    x$setinverse(i)
    i
}
