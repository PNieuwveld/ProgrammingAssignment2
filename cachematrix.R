## makeCacheMatrix and cacheSolve functions written by Peter Nieuwveld for the
## Coursera "R Programming" course.  12 August 2015.

## These functions create a matrix that can cache it's inverse, and when
## solving the inverse of a matrix will first check for the value in the global
## environment.


## makeCacheMatrix takes a matrix as an argument.  The matrix is assumed to be
## invertible, and no error checking is performed.  The matrix and it's inverse 
## are stored as matrices in teh Global environement.

makeCacheMatrix <- function(x = matrix()) {     ## Takes a matrix as argument
        inv <- NULL                     ## Initialise empty matrix for inverse
        setmatrix <- function(y) {      ## Stores x in Global env
                x <<- y                 
                inv <<- NULL
        }
        getmatrix <- function() x                       ## Retrieves x
        setinverse <- function(solve) inv <<- solve     ## Stores inv as Global
        getinverse <- function() inv                    ## Retrieves inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)   ## Returns a list of the functions
                                        #  defined in makeCacheMatrix function
}       ## End of makeCacheMatrix function


## cacheSolve takes the list of functions returned by makeCacheMatrix as an
## argument and returns the inverse of the matrix - from the cache if it exists,
## or otherwise solves the inverse and stores it in teh Global env.

cacheSolve <- function(x, ...) {        ## Takes a list of functions as args
        inv <- x$getinverse()                   ## Gets the inverse from Global
        if(!is.null(inv)) {                     ## If inv is defined in Global
                message("getting cached data")  #  env then retrieves from there
                return(inv)                     ## Escapes the function &
                                                #  returns cached value
        }
        data <- x$getmatrix()   ## If inv not storedin cache, retrieve matrix
        inv <- solve(data, ...) ## Calculate the inverse
        x$setinverse(inv)       ## Store inv in the Global env
}       ## End of cacheSolve function
