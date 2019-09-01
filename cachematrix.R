## By Nicholas Barrett
## These functions allows the inverse of a matrix to be computed if it hasnt been
## and get it from the cache if it has been computed already 

#the function below sets the value of a matrix, then gets the value, sets the value of the inverse,
#then gets the value of the inverse, creating a list of all 4 of these functionm
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This computes the inverse of the above created matrix, and if the inverse has been calculated,
## it returns the cached calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
#below is a test of the functionality
x <- matrix(1:4,2,2)
x1 <- makeCacheMatrix(x) #input into the first function to create the format
cacheSolve(x1)#solves the inverse the first time and stores it
cacheSolve(x1)#gets the cached inverse, shown by the print "getting cached data"


