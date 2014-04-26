# `makeCacheMatrix` - generator of cacheMatrix object
# that is capable of storing matrix's inverse
# `cacheSolve` - function for retrieval of the inverse
# for object generated through `makeCacheMAtrix` supplied 
# on its input

# Function for generating cacheMatrix objects (lists)
makeCacheMatrix <- function(x = matrix()) {
    # inverse variable used for storing inverse
    # of a input matrix. 
    inverse <- NULL
    
    # setter of the input matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # getter of a original matrix
    get <- function() { 
        x
    }
    
    # inverse setter - used for cache
    setinverse <- function(inverse) { 
        inverse <<- inverse
    }
    
    # inverse getter
    getinverse <- function() { 
        inverse
    }
    
    # return list with all getter/setters methods 
    # defiened
    list(
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


# `cacheSolve` returns inverse of a supplied cachedMatrix
# object. Please notice that the input argument of the 
# `cacheSolve` is the output of the `makeCacheMatrix`
cacheSolve <- function(x, ...) {
    # try to get inverse of `x` ...
    inverse <- x$getinverse()
    # if it was already calculated return the 
    # cached result ...
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # otherwise get the original matrix ...
    data <- x$get()
    # inverse it ...
    inverse <- solve(data, ...)
    # save the result in cache ...
    x$setinverse(inverse)
    # and return the inverse
    inverse
}
