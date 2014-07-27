## R Programming assignment 2

## This function creates an environment and provides the functions to manipulate
## that environment
#  0: 20140725 - initial version from the assignment
#  1: 20140725 - modified makeCacheMatrix() to show structure then added trace prints
#  2: 20140727 - more comments and minor fixes, cmt out messages
#
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                                                     # orginal       
    set <- function(y) {                                            # orginal - creates set function
        x <<- y                                                     # orginal - stores matrix in parent env
        inv <<- NULL                                                # orginal - creates null inv in parent env
#        message("set", x, inv)                                      # added for tracing
    }                                                               # orginal
    get <- function() {                                             # modified to show structure
        x                                                           # orginal - gets matrix from parent env
#        message("get", x)                                           # added for tracing
    }                                                               # modified to show structure
    setinv <- function(z) {                                         # modified to show structure
        inv <<- z                                                   # orginal - stores inv in parent env
#        message("setinv", z, inv)                                   # added for tracing     
    }                                                               # modified to show structure
    getinv <- function() {                                          # modified to show structure
        inv                                                         # orginal - gets inv from parent env
#        message("getinv", inv)                                      # added for tracing     
    }                                                               # modified to show structure
    list(set = set, get = get,setmean = setmean,getmean = getmean)  # orginal
}

## this function uses the environment and functions created by makeCacheMatrix() to return 
## the inverse of a matrix, either by computing it or from a cached version
#
#  Dependencies: must call makeCacheMatrix() first
#
#  0: 20140725 - initial version from the assignment, approach based on the vector mean example
#  1: 20140725 - cmt out message
#
cacheSolve <- function(x, ...) { 
    inv <- x$getinv()                                               # get the inverse
    if(!is.null(inv)) {                                             # if there is a cached version
#        message("getinv", inv)                                      #   trace
        return(inv)                                                 #   return it
    } else {                                                        # else 
        data <- x$get()                                             #   get the matrix
        inv <- solve(data, ...)                                     #   compute the invers
        x$setinv(inv)                                               #   cache it
#        message("compute & cache inv",inv)                          #   trace it
        return(inv)                                                 #   return it
    }
}                                                                   # should never return fom here
