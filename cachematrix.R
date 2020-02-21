## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse
## of a matrix rather than computing it repeatedly


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}

## Test the functions
test1 <- makeCacheMatrix(matrix(1:4, 2, 2))

test1$get()
test1$getinverse()

cacheSolve(test1)


test2 <- makeCacheMatrix(matrix(rnorm(1:3, 1.5, 1), 2, 2))

test2$getinverse()
test2$get()

cacheSolve(test2)


test3 <- makeCacheMatrix(matrix(rnorm(1:16, 3,0.5), 4, 4))

test3$getinverse()
test3$get()

cacheSolve(test3)
