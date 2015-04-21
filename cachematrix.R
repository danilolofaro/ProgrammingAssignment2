## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function, `makeCacheMatrix` creates a special "matrix", which is
# a list containing a function to
# 
# 1.  set the value of the matrix "setCM(matrix)"
#       - the set function doesn't automatically set NULL for the 
#         inverse, because in the assignment is indicated the 
#         possibility to change the matrix.
# 2.  get the value of the matrix "getCM()"
# 3.  set the value of the inverse of the matrix "setInv(matrix)"
# 4.  get the value of the inverse of the matrix "getInv()"


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setCM <- function(y) {
                x <<- y
                # inv <<- NULL
        }
        getCM <- function() x
        setInv <- function(i = matrix()) inv <<- i
        getInv <- function() inv
        list(setCM = setCM, getCM = getCM,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function
# The following function calculates the inverse of the cached "matrix"
# created with the above function. It first checks if the
# original matrix (from which to calculate the inverse has been changed). 
# In that case it prints a message and continues with the calculation 
# of the inverse of the "new" matrix. If not the case, it checks if
# the inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data using solve if the matrix is square, or ginv (MASS package) in other cases,
# sets the value of the inverse in the cache via the `setInv` function 
# and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInv()
        
        # if the inverse is cached, check if the "original" matrix is changed
        # otherwise returns the cached inverse
        if(!is.null(inv)) { 
                if (prod(diag(x$getCM() %*% x$getInv()))!=1) {
                message("matrix is changed")
                }
                else if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
        }
        # computes the inverse of the matrix
        data <- x$getCM()
        message("computing the inverse of the matrix")
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
