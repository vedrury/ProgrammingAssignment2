## Put comments here that give an overall description of what your
## functions do
## These functions calculate a matrix's inverse and retrieve the "saved"/
## cached matrix inverse if previously determined without 
## recalculating it, which is especially useful for large matrices.

## Write a short comment describing this function
## MakeCacheMatrix creates a special "matrix" that will help with later caching 
## a matrix's inverse. It 1) sets the value of the matrix 2) retrieves/"gets" 
## the value of the matrix, 3) sets the inverse of the matrix 4) retrieves/
##"gets" the inverse of the matrix. MakeCacheMatrix returns a list of these 4 
##functions.

makeCacheMatrix<-function(x = matrix()) {  ## set matrix as argument default
        ## create empty inv variable, set to NULL
        inv <- NULL 
        ## define set function
        set <- function(z) { 
                ## assign new variable to x in parent environment
                x <<- z 
                ## if new matrix, inv is NULL in parent environment
                inv <<- NULL 
        }
        ## define get to return matrix x
        get <- function() x 
        ## define setinv as inverse
        setinv <- function(inverse) inv <<- inverse 
        ## define getinv to retrieve the value of inv
        getinv <- function() inv 
        # make list to be able to retrieve each function with $ operator
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv) 
}


## Write a short comment describing this function
## cacheSolve uses the information within makeCacheMatrix and the solve function
## to calculate the inverse of a given matrix. It checks if the matrix's
## inverse has already been calculated and cached and retrieves that value.
## Otherwise, cacheSolve calculates the inverse of the matrix with the solve
## function and returns its value.

cacheSolve<-function(x,...) {
        ##set inv as the getinv function of x
        inv <- x$getinv()
        ##if inv cached, send message and retrieve cached value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##if inv not cached, calculate inverse of x with solve and set as inv
        data1 <- x$get()
        inv <- solve(data1, ...)
        x$setinv(inv)
        ##return inv of x
        inv
}
