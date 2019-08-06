## This program solves for inverse matrix and caches them. 

## "makeCacheMatrix" receives matrix input and gives list output.
## Lexical scoping established here enables "cacheSolve" to call from variables loaded here.

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        set <- function(y){
                x <<- y
                xInv <<- NULL
        }
        get <- function() x
        setInv <- function(z) xInv <<- z
        getInv <- function() xInv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

##  "cacheSolve" solves for inverse of this matrix if not available or returns cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInv <- x$getInv()
        if(!is.null(xInv)){
                message("getting cached inverse")
                return(xInv)
        }
        data <- x$get()
        xInv <- solve(data)
        x$setInv(xInv)
        xInv
}
