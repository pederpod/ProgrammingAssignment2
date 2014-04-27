## Two functions are contained within this script: a function that generates a 
## special "matrix" object that can cache its inverse (makeCacheMatrix) and a 
## function that performs the calculates the inverse of the special "matrix" 
## cacheSolve. In the case that the result was already calculated, it returns 
## the value from the cache.

## The first function, makeCacheMatrix creates a special "matrix", which is  
## simply a list containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

## NOTE: This script uses the solve function and assumes the matrix is square 
## (nrows = ncols). If the matrix is nonconforming, an error will be returned.

## The second function, cacheSolve, calculates the inverse of the matrix 
## specified in makeCacheMatrix or pulls the inverse from the cache in the case
## that it has already been solved by using the special "matrix" created by 
## makeCacheMatrix.
##   - The first if statement determines if the inverse is stored in the cache 
##     and retreives it if available. The return function gets the inverse and 
##     concludes the cacheSolve function.
##   - If no inverse is present in the cache, the matrix is pulled using x$get, 
##     solved, and stored as inv. The value of inv is stored to the cache with
##     x$setInverse(inv).


## List of Variables:
## x    -> input matrix to be solved. must be square.
## inv  -> stores the inverse of the matrix.
## y    -> new matrix to be assigned to x when makeCacheMatrix$set is called.
## data -> holds matrix when x$get called from within cacheSolve.
## ____________________________________________________________________________


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL               
        set <- function(y) {   ## reassigns value of x and resets m
                x <<- y
                inv <<- NULL
        }
        get <- function() x   ## gets the matrix
        setInverse <- function(inverse) inv <<- inverse   ## sets inverse matrix
        getInverse <- function() inv   ## gets values of inverse matrix
        list(set = set, get = get,   ## puts all functions into a list
             setInverse = setInverse, 
             getInverse = getInverse)  ## assigns names to list elements
}

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()   ## load contents of variable for inverse matrix
        if(!is.null(inv)) {   ## determine if inverse matrix has been solved
                message("getting cached data")
                return(inv)   ## return cached solution if yes and exit
        }                     ## else, continue calculating inverse
        data <- x$get()   ## load matrix data
        inv <- solve(data)   ## solve for inverse
        x$setInverse(inv)   ## preserve inverse matrix in cache
        inv   ## return inverse matrix
}
