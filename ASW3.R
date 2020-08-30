## we have two functions, makecahematrix that consists 
## of set, get, setinv, getinv and then we have makecachematrix
## to calculate the inverse for non squared i use the solve function and the library(mass)

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULLcache
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function(){
                          inver<-ginv(x)
                          inver%*%x
                          }
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting the cached data, please wait")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}

