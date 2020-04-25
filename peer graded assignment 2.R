cachematrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function()  x
set1 <- function(inverse)  inv <<- inverse
get1 <- function() inv 
list(set = set, get = get, set1 = set1, get1 = get1)
}

cachesolve <- function(x,...)
{
  inv <- x$get1()
  if(!is.null(inv)) 
    {
      message("getting cached result")
      return(inv)
    }  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

m <- matrix(1:9,3,3)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
