## Programming Assignment 2 - Coursera R Programming - Jan 2014

## Below are two functions that are used to create a list that stores a matirx
## and cache's its inverse.

#The first function, makeCacheMatrix, creates a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

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


##The following function, cacheSolve, calculates the inverse of the matrix identified by the list 
##created by makeCacheMatrix.  It first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
##the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

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