
## Here is my solution 

makeCacheMatrix <- function(x = matrix()){
        inver <- NULL
        set <- function(y) { 
                x <<- y
                inver <<- NULL 
        }
        get <- function() {x}
        setInverse <- function(inverse) {inver <<- inverse}
        getInverse <- function() {inver}
        list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}

cacheSolve <- function(x, ...) {
        inver <- x$getInverse()
        if( !is.null(inver)){
                message("data is cached")
                return(inver)
        }
        m <- x$get()
        inver <- solve(m,...)
        x$setInverse(inver)
        inver
}

zmatrix <- makeCacheMatrix(matrix(1:4, ncol= 2, nrow = 2))
zmatrix$get()
zmatrix$getInverse()
cacheSolve(zmatrix)
cacheSolve(zmatrix)
zmatrix$getInverse()