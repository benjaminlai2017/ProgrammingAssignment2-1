## makeCacheMatrix is a vector of functions to create a special "matrix" object 
## that can cache its inverse 

## The first function, makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inversed matrix
## get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
            getmatrix = getmatrix)      
}


## cacheSolve function inverse the special "matrix" created with
## the makeChacheMatrix function. However, it first checks to see if the matrix has already 
## been calculated. If so, it gets the inversed matrix from the cache and skips the computation. 
## Otherwise, it inverse the matrix and sets the inversed matrix in 
## the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

#test the results of the above functions
#> a<-makeCacheMatrix(matrix(1:4,2,2))
#> cacheSolve(a)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(a)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5