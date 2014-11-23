makeCacheMatrix <- function(x = matrix()) {
  
## mx_inv stores cached inverse matrix
mx_inv <- NULL

# set_mx sets the matrix
set_mx <- function(y) {x <<- y  
                       mx_inv <<- NULL
                      }

# Gets the matrix 
get_mx <- function() x

set_inv <- function(inv) mx_inv <<- inv   ## Sets the inverse
get_inv <- function() mx_inv              ## Gets the inverse
  
list(set_mx=set_mx,get_mx=get_mx,set_inv=set_inv,get_inv=get_inv)
}


## This function returns an inverse matrix of x
cacheSolve <- function(x, ...) {
                                mx_inv <- x$get_inv()
                                if(!is.null(mx_inv)) {
                                                      message("getting cached data")
                                                      return(mx_inv)
                                                      }
                                data <- x$get_mx()
                                mx_inv <- solve(data, ...)
                                x$set_mx(mx_inv)
                                mx_inv
}
