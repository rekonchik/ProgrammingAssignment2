
## The function takes as input a matrix. The data is cached.
## Inside the function prescribed four functions
## that allow you to access the data stored in the cache and change them.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_solve <- function(solve) m <<- solve
  get_solve <- function() m
  list(set = set, get = get,
       set_solve = set_solve,
       get_solve = get_solve)
}


## The function takes as argument the result of
## the previous solve for matrix and calculates if the calculation
## has not been made previously. Otherwise it returns the value
## of the calculated earlier for the same array of cache.

cacheSolve <- function(x, ...) {
  m <- x$get_solve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_solve(m)
  m
}
