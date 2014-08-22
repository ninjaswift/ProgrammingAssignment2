## Coursera R ProgrammingAssignment2

## Create a new "matrix" object that can cache its inverse.

makeInverse <- function(my_matrix = matrix()) {
        inverse_matrix <- NULL 
        set <- function(new_matrix) {
                my_matrix <<- new_matrix ## create new matrix
                inverse_matrix <<- NULL ## set variable to NULL so does not interfere with old data
        }
        get <- function() my_matrix
        setinverse <- function(new_inverse) inverse_matrix <<- new_inverse
        getinverse <- function() inverse_matrix
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## Compute and store inverse of matrix returned by makeInverse above.
## Retrieve inverse matrix from cache.

cacheInverse <- function(my_matrix, ...) {
        ## Return a matrix that is the inverse of 'my_matrix'
        my_inverse_matrix <- my_matrix$getinverse()
        if(!is.null(my_inverse_matrix)) { ## if matrix is cached, return message 
                message("getting cached data")
                return(my_inverse_matrix) ## print matrix
        }
        data <- my_matrix$get()
        my_inverse_matrix <- solve(data, ...)
        my_matrix$setinverse(my_inverse_matrix)
        my_inverse_matrix ## print inverse matrix
}
