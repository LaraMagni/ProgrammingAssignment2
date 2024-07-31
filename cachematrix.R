## The two following functions allow to cache the inverse of a stored matrix instead of computing it

## The function makeCacheMatrix create a "matrix". This is a list containing the functions to set and get the value of the matrix and set and get the value of the inverse

makeCacheMatrix<- function (x=matrix()) {
  m <- NULL
  set<- function(y) {
    x<<-y
    m<<-NULL
  }
  get<- function() x
  setinverse<- function(inverse) m<<-inverse
  getinverse<- function() m
  list( set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function cacheSolve computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getinverse()
  if(!is.null(m)) {
    message ("getting chached data")
    return (m)
  }
  data<- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}
