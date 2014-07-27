## R Programming assignment 2

## This function creates an environment and provides the functions to manipulate
## that environment
#  0: 20140725 - initial version from the assignment
#  1: 20140725 - modified makeCacheMatrix() to show structure then added trace prints
#
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                                                     # orginal       
    set <- function(y) {                                            # orginal
        x <<- y                                                     # orginal
        inv <<- NULL                                                # orginal
        message("set", x, inv)                                      # added for tracing
    }                                                               # orginal
    get <- function() {                                             # modified to show structure
        x                                                           # orginal
        message("get", x)                                           # added for tracing
    }                                                               # modified to show structure
    setinv <- function(z) {                                         # modified to show structure
        inv <<- z                                                   # orginal
        message("setinv", x, inv)                                   # added for tracing     
    }                                                               # modified to show structure
    getinv <- function() {                                          # modified to show structure
        m                                                           # orginal
        message("getinv", m)                                        # added for tracing     
    }                                                               # modified to show structure
    list(set = set, get = get,setmean = setmean,getmean = getmean)  # orginal
}

## this function uses the environment and functions created by makeCacheMatrix() to return 
## the inverse of a matrix, either by computing it or from a cached version
#
#  Dependencies: must call makeCacheMatrix() first
#
#  0: 20140725 - initial version from the assignment, approach based on the vector mean example
#  1: 20140725 - 
#
cacheSolve <- function(x, ...) { 
    inv <- x$getinv()                                               # get the inverse
    if(!is.null(inv)) {                                             # if there is a cached version
        message("getinv", inv)                                      #   trace
        return(inv)                                                 #   return it
    } else {                                                        # else 
        data <- x$get()                                             #   get the matrix
        inv <- solve(data, ...)                                     #   compute the invers
        x$setinv(inv)                                               #   cache it
        message("compute & cache inv",inv)                          #   trace it
        return(inv)                                                 #   return it
    }
    message("can't get here!!!")                                    # trace - flow error if get here
}                                                                   # should never return fom here
