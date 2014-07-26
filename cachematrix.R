## This is a pair of functions which together allow matrix inversions to be cached 
## for later retrival in order to reduce overall computation time

#####################
## make CacheMatrix sets up a list of functions to store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL	

##set stores a new matrix and clears any previously stored inverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

##get is used to return the stored matrix
        get <- function() x

##setinv calculates the inverse of the matrix and stores
        setinv <- function(solve) m <<- solve

##getinv returns the stored inverse matrix
        getinv <- function() m

##establish named list of functions for calling from elsewhere
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

######################
## cacheSolve will return the inverse of a matrix IF the matrix has been 
## previously sent through the makeCacheMatrix function

cacheSolve <- function(x, ...) {
      
	##check to see in inverse is already stored 
	  m <- x$getinv()
        if(!is.null(m)) {

	##if stored then return inverse
                message("getting cached matrix inverse")
                return(m)
        }

	##else calculate, store and return inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m	
}
