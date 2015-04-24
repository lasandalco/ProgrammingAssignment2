makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
		x <<- y
		m <<- NULL
	  }
      get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set=set, get=get, setinv=setinv,
       getinv=getinv)
}	

######################################
cacheSolve <- function(x, ...) {
	 m <- x$getinv()
	 if(!is.null(m)) {
	    message("getting cached data")
	    return(m)
	 } 
	 i <- x$get()
	 m <- solve(i, ...)	
	 x$setinv(m)	
	 m			
}	
