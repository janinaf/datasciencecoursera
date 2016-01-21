# The function makeCacheMatrix creates a special "matrix" object that can cache
# its inverse

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

# the function requires the MASS package
#it returns the Moore-Penrose generalized inverse of A

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(ginv) inverse <<- ginv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The following function calculates the inverse of the special "vector" created
# with the above function. However, it first checks to see if the inverse has
# already been calculated. If so, it gets the inverse from the cache and skips the
# computation. Otherwise, it calculates the inverse of the data and sets the value
# of the inverse in the cache via the setinverse function.


# as the function above, this function requires the MASS package
#it returns the Moore-Penrose generalized inverse of A

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- ginv(data, ...)
        x$setinverse(inverse)
        inverse
}
