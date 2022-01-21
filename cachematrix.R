## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #inv will store the cache inverse matrix of x
        inv <- NULL 
        
        # set the value of the matrix x (not the inverse)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # get the value of the matrix x (not the inverse)
        get <- function() {
                x
        }
        
        # set the value of the inverse matrix (stored in inv)
        setinverse <- function(inverse) {
                inv <<- inverse
                
        }
        
        # get the value of the inverse matrix (stored in inv)
        getinverse <- function() {
                inv
        }
        
        # returns a list of all the above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## cacheSolve checks if there is a cached value and returns the value if 
##      available, otherwise recaculates the inverse and caches the new value


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # check if the inverse already exists
        m <- x$getinverse()
        
        # if m is not null, the inverse already exists, 
        # so we get it from the cache and return the inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if it doesn't exist, we calculate the inverse matrix,
        # store it, and return the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
