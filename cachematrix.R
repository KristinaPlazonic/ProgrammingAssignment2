## These functions are used to compute and store the inverse of a matrix
## They are useful for reusing results of long-running computations

## makeCacheMatrix makes a special cached matrix object with methods set and get for the original matrix
## and methods setinverse and getinverse for setting and getting the inverse of the original matrix
## 

makeCacheMatrix <- function(x = matrix()) {
	    inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will retrieve the pre-computed value of the matrix inverse if it exists 
## or compute inverse if it has not been computed previously
## If inverse has been computed previously, the value of the inverse, 
## stored in makeCacheMatrix, is returned with no further computations
## If inverse has not been computed previously, we compute the inverse (using the function "solve")
## and store for later use


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)   ## if cached value exists, the function exits here
        }
        data <- x$get()  ## get the matrix from the makeCacheMatrix object
        inv <- solve(data, ...)  ## compute inverse, since there is no cache
        x$setinverse(inv) ## save the inverse for later access as a cashed object
        inv   ## return the inverse
}
