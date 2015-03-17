## Functions output inverse matrix from cache (if cached matrix exist), output inverse matrix from matrix input
## if cached matrix doesnt exist

## Matrix Cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                       ##assign m  NULL value in local env
        set <- function(y) {                            ##assign set fun(y), which
                x <<- y                                 ##assign x y in in external env
                m <<- NULL                              ##assign m NULL value in external env
        }                                               
        get <- function() x                             ##function get() output x                             
        setmatrix <- function(matrix) m <<- matrix      ##function setmatrix() assign m matrix value in ext env
        getmatrix <- function() m                       ##function getmatrix() output m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Check Cache, if cache empty do math and set up cache, another output cache

cacheSolve <- function(x, ...) {
        #assign cache value to m in local env
        m <- x$getmatrix()
        
        #output matrix if cache exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #find inverse matrix from input, if matrix invertible
        data <- x$get()
        m <- if ((nrow(data)-ncol(data)) == 0 && det(data) != 0) { 
                solve(data)
        } else {
                message("Matrix is not invertible, please input square matrix, where determinant isn't equal to 0")
        }
        
        #caching matrix
        x$setmatrix(m)
        
        #output !NULL result
        if(!is.null(m)) {
                return(m)
        } else {
                invisible(m)
        }
}