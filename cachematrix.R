## The functions below cache the inverse of a matrix

## The first function creates a special matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function computes the inverse of the special 
## matrix returned by makeCacheMatrix. If the inverse is
## already calculated, cacheSolve should retrieve the reverse
## from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

### Sample run
### 1. Creating a matrix.
### x <- matrix(c(2,3,3,4), nrow = 2, ncol = 2)
### 2. Assigning a variable to makeCacheMatrix applied to the matrix
### m <- makeCacheMatrix(x)
### 3. Print the matrix
### m$get()
### 4. Running cacheSolve once (no cache)
### cacheSolve(m)
### 5. Running cacheSolve a second time ("getting cached data" message)
### cacheSolve(m)
