## Creating a function that will cache the inverse of a matrix

## Creating a matrix and subsequent list including:
## matrix value is set, matrix value retreived, inverse value set, inverse value retreived

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set = function(y) {
                x <<- y
                i <<- NULL
        }
        get = function() x
        setinv = function(inverse)
                i <<- inverse
        getinv = function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## This function caches inverse value of matrix so the
## matrix inverse value does not need to be rerun each time
## the function is executed.  This is done by first checking to see
## if inverse value has been set.  If it has, chached value is retreived.
## If not, value is set.

cacheSolve <- function(x, ...) {
        i = x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data = x$get()
        i = solve(data)
        x$setinv(i)
        i
}

x = matrix(rnorm(16), 4, 4)
cm = makeCacheMatrix(x)
cm$get()