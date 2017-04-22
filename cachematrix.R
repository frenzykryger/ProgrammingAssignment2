## Implementation of memoization for matrix inversion operation
## It's done using implementation for memoization of single argument functions 

## Wraps object into special vector which is able to store cached evaluation result

makeCache <- function(x) {
  cache <- list()
  set <- function(y) {
    x <<- y
    cache <<- list()
  }
  get <- function() x
  setcached <- function(val, name) cache[[name]] <<- val
  getcached <- function(name) cache[[name]]
  list(set = set, get = get,
       setcached = setcached,
       getcached = getcached)
}

## Wraps single argument function returning version which caches result of computation
## It's possible to pass > 1 arguments to memoized function but only first one
## is used for caching 

memoize <- function(func, name) {
  function(cache, ...) {
    cached <- cache$getcached(name)
    if(!is.null(cached)) {
      message("getting cached data")
      return(cached)
    }
    data <- cache$get()
    val <- func(data, ...)
    cache$setcached(val, name)
    val
  }
}

## Wraps matrix into special vector with ability to cache inversion result

makeCacheMatrix <- function(x = matrix()) {
  makeCache(x)
}

## version of matrix solve which caches computation result 

cacheSolve <- memoize(solve, 'solve')