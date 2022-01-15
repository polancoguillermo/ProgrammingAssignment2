makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setdata <- function(y) {
    x <<- y
    i <<- NULL
  }
  getdata <- function () x
  setInverse <- function (inv) i <<- inv
  getInverse <- function () i
  list(setdata = setdata,
       getdata = getdata,
       setInverse = setInverse,
       getInverse = getInverse)
}




cachesolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
      print("getting cache matrix")
      return(i)
  } 
  dat <- x$getdata()
  i <- solve(dat, ...)
  x$setInverse(i)
  i
}