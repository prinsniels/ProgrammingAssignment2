## Set of functions that make it possible to store a matrix and its
## inverse into cache

# makeCacheMatrix 
# takes a matrix and puths the matrix in cache
# the matrix is stored in Cache as x
# the inverse is stored in Cache as inv_v
# 
# the function returns a list with the memory pointers to the matrixes
# the user can reteive data via these pointers.
#
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the value of the inverse
#  4. get the value of the inverse
#  5. Build the list
#


makeCacheMatrix <- function(x = matrix()) {
    
    # inv_v is the basic container for the inverted matrixh
    inv_v <- NULL
    set <- function(y) {
        x <<- y
        inv_v <<- NULL
    }
    
    # create the basic functions used in this method
    get <- function() x
    
    # set inverse creates the iverse en puts it in cache as inv_v
    setInverse <- function(solve) inv_v <<- solve
    # get inverse pulls the Inverse form cache
    getInverse <- function() inv_v
    
    # the return list is build
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


# cacheSovle 
# function checks if the inverse of the matrix is in memory 
# via the inputed list
# if so, it retreives the inverse from memory
# if not, it computes and stores the matrix in memory
#

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    # get the inverse from memory if there..
    inv_v <- x$getInverse()
    if(!is.null(inv_v)) {
        message("getting cached data")
        # return and exit function if TRUE
        return(inv_v)
    }
    
    # get the data (original matrix)
    data_v <- x$get()
    # calc the inverse
    inv_v <- solve(data_v, ...)
    # store in memory
    x$setInverse(inv_v)
    # return the inverse of X
    inv_v
}
