## The first function makes a list with methods that set and get a matrix and its inverse in an intrinsic environment
## variable 

## The second function is passed the list from the first and attempts to calculate and set its inverse. If the inverse is already available,
## the values is taken else it is calculated.

## makeCacheMatrix will create a matrix x and expose three methods to set and get x and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ## Initialize inverse
        ##set x in this environment with desired value, if inverese is already set then populate it
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        ## set inverse variable in parent environment to desired value and return the value
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Check if its already a cached inverse, if so return the same, else calculate.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data") ## populate the message
                return(inv) ## return the cached inverse
        }
        mat <- x$get()
        inv <- solve(mat, ...) ## solve the inverse of the given matrix
        x$setinverse(inv)  ## set the inverse of the matrix
        inv          ## return the inverse of the matrix
}