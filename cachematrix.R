## If the contents of a matrix are not changing, it may make sense to cache the value of its inverse
## so that when we need it again, it can be looked up in the cache rather than recomputed.

## Si el contenido de la matriz no varía, puede merecer la pena guardar el valor de su inversa en la memoria caché
## para que cuando lo necesitemos de nuevo, lo cojamos de ahí en vez de recalcularlo


## This function creates a special "matrix" object that can cache its inverse.

## Esta función crea un objeto "matriz" especial que puede guardar su inversa en la caché 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

## Esta función calcula la inversa de la "matriz" especial de la función de arriba.
## Si ya ha sido calculada, y la matriz no ha variado, entonces devuelve el valor en la memoria caché

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
