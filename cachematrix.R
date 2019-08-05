## creates a object that stores a matrix and the matrix inverse for future use
## instead of recalculating a matrix inverse, the inverse is recalled

## creates the cached matrix object that is able to 

makeCacheMatrix <- function(x = matrix()) {
   
   xinv <- NULL
   set <- function(y){
      x <<- y
      xinv <<- NULL
   }
   get <- function() x
   setinv <- function(z) xinv <<- z
   getinv <- function() xinv
   list(set = set, get = get,
        setinv = setinv, getinv = getinv)
   
}


## checks to see if inverse has previously been calculated, 
## and if not returns the inverse of matrix x

cacheSolve <- function(x, ...) {
   xinv <- x$getinv()
   if(!is.null(xinv)) {
      return(xinv)
   }
   data <- x$get()
   xinv <- solve(data,...)
   x$setinv(xinv)
   xinv
   
}