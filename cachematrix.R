## Put comments here that give an overall description of what your
## functions do

##Create a class that stores a matrix and its inverse if previously computed. 

makeCacheMatrix <- function(x = matrix()) {
  ##minv stores the inverted matrix: initialize
  minv<- NULL
  set <- function(y)
  {
    x<<-y
    minv<<-NULL
  }
  ##Functions to get and set values
  get <- function() x
  getInverse <- function() minv
  setInverse <-function(solve) minv<<-solve
  #create list of functions so we don't get a closure error
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function that when fed a makeCacheMatrix matrix will either compute its inverse
##or get the stored inverse if available

cacheSolve <- function(x, ...) {
  #Get the cached value
  
  minv<- x$getInverse()
  
  ##Check to see if already computed
  ## If yes, give message and return
  if (!is.null(minv))
  {
    message("Using cached matrix")
    return (minv)
  }
  ##else invert it, store in the class and return
  toinvert<-x$get()
  minv <- solve(toinvert)
  x$setInverse(minv)
  minv
}
