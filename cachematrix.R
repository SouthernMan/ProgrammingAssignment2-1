## ----------------------------------------------------------------------------
##
## The two functions contained in the R file allow a user to create an R object
## which can be used to store a matrix and its inverse.
##
## Once the Inverse Matrix has been calculated, it is cached within the
## makeCacheMatrix environment, so that it is avalable to the users or R 
## program without the need to re-calculate it.
##
## In keeping with R's principle of 'lazy evaluation', the inverse matrix is 
## not calculated until the first time it is required.
##
## These functions are used as follows:-
##    1. Create the R object using makeCacheMatrix(x), where x is a matrix.
##    2. Use cacheSolve(x) to retrun the iverse of matrix x.  the first time 
##       this function is called, it will calculate the inverse matrix, store  
##       theinverse matrix in the cache, and finally return the inverse matix.  
##       On subsequent calls, cacheSolve(x) will simply return the inverse 
##       matrix from the cache.  
##    3. If the matrix x is changed, the cached inverse of x will be deleted.
##       The next time cacheSolve(x) is called, the inverse for the new x will
##       be calculated.  The process described in 2. is folloed for the nex x.
##


## ----------------------------------------------------------------------------
##
## The makeCacheMatrix function works as follows:-
##
##    1. When makeCacheMatrix(x) is called, the matrix x is stored in the 
##       Matrix variable.  The MatrixInverse Variable is cleared, as the
##       inverse of Matrix x has not yet been calculated.
##       
##    2. The makeCacheMatrix$set(x) function is an alternative way to set the 
##       Matrix variable.  The MatrixInverse is cleared by this function.
##
##    3. The makeCacheMatrix$get() function returns the Matrix variable.
##
##    4. The makeCacheMatrix$setInverse(Inverse) stores the Inverse of the
##       x matrix in the MatrixInverse variable.
##
##    5. The makeCacheMatrix$getInverse() function returns the MatrixInverse
##       variable.
##
##

makeCacheMatrix <- function(x = matrix()) {
        Matrix        <- x
        MatrixInverse <- NULL

        set <- function(y) {
                Matrix        <<- y
                MatrixInverse <<- NULL
        }

        get <- function() {
                Matrix
        }

        setInverse <- function(Inverse) {
                MatrixInverse <<- Inverse
        }

        getInverse <- function() {
                 MatrixInverse
        }

        list(set        = set        , 
             get        = get        ,
             setInverse = setInverse ,
             getInverse = getInverse   )
}


## ----------------------------------------------------------------------------
##
## The cacheSolve function works as follows:-
##
##    1. When cacheSolve(x) is called, it is passed an R object created by the  
##       makeCacheMatrix function.  This R object can contain a Matrix and its
##       inverse.
##
##    2. The function gets the inverse matrix from the R Object.
##
##    3. If the value of the inverse matrix is null, then the inverse marix is
##       calculated and saved to the R object.  The inverse matrix is then
##       returned to the calling program.
##
##    4. If the value of the inverse matrix is not null, then the inverse marix 
##       is returned to the calling program.
## 

cacheSolve <- function(x, ...) {
        ## Check if the Inverse Matrix is available
        ##
        MatrixInverse <- x$getInverse()             
        if(!is.null(MatrixInverse)) {

                ## Inverse Matrix is available, so return a message
                ## to demonstrate functionality and return the inverse matrix
                ##
                message("Getting cached inverse matrix ...")
                return(MatrixInverse)
        }

        ## Inverse Matrix is not available, so return a message
        ## to demonstrate functionality, caluclate inverse matrix, save the 
        ## result in the cache for the next timeand return the inverse matrix
        ##
        message("Solving for inverse matrix ...")
        Matrix        <- x$get()
        MatrixInverse <- solve(Matrix, ...)

        x$setInverse(MatrixInverse)
        MatrixInverse
}

## -- EOF ---------------------------------------------------------------------