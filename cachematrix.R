## In this script, you will find two functions. The main target
## is to cache the inverse of a matrix, in order to avoid
## multiple calculations. The functions are as follows:

## 1. makeCacheMatrix: this functions gets a matrix and
## transforms it to a special type of matrix that stores
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse ,
             getInverse = getInverse )
}

## 2. cacheSolve: this functions gets a special type of 
## matrix transformed by makeCacheMatrix function and
## returns the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getInverse ()
        if(!is.null(m)) {
            message("Inverse matrix in cache: ")
            return(m)
        }
		data <- x$get()
        m <- solve(data, ...)
		x$setInverse(m)
        m
}

