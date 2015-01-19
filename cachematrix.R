## Put comments here that give an overall description of what your
## functions do
## TEST FOR MY REPOSITORY
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m_inverse <<- inverse
        getinverse <- function() m_inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$getinverse()
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        data <- x$get()
        m_inverse <- solve(data, ...)
        x$setinverse(m_inverse)
        m_inverse
}

