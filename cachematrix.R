# `makeCacheMatrix` - generator of cacheMatrix object,
# which capable of storing inverse of a supplied matrix

# `cacheSolve` - function for retrieval of the inverse
# for the object generated through `makeCacheMAtrix` 
# supplied as its input

# Function for generating cacheMatrix object (list)
makeCacheMatrix <- function(x = matrix()) {
    # inverse variable used for storing inverse
    # of a input matrix
    inverse <- NULL
    
    # setter of the input matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # getter of a original (input) matrix
    get <- function() { 
        x
    }
    
    # matrix's inverse setter - used for cache
    setinverse <- function(inverse) { 
        inverse <<- inverse
    }
    
    # matrix's inverse getter
    getinverse <- function() { 
        inverse
    }
    
    # return list with all getter/setters methods 
    # defined
    list(
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


# `cacheSolve` returns inverse of a supplied cachedMatrix
# object. The input argument of the `cacheSolve` is the 
# output of the `makeCacheMatrix` function
cacheSolve <- function(x, ...) {
    # try to get inverse of `x` ...
    inverse <- x$getinverse()
    # if it was already calculated return the 
    # cached result ...
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    # otherwise get the original matrix ...
    data <- x$get()
    # inverse it ...
    inverse <- solve(data, ...)
    # save the result in cache ...
    x$setinverse(inverse)
    # and return the inverse.
    inverse
}
