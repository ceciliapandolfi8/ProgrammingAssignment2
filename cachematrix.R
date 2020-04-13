## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly
## The following pair of functions caches the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of inverse of the matrix
#4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## First, it checks if the inverse has already been calculated and the matrix has not changed.
## Then, cacheSolve retrieves the inverse from the cache.
## If not, it computes the inverse, sets the value in the cache via setinverse function
## For this example, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Sample run:
## First, create a matrix
> x <- matrix(c(1,2,3,4),2,2)
> x
     [,1] [,2]
[1,]    1    3
[2,]    2    4

## Then, run makeCacheMatrix of x
> m <- makeCacheMatrix(x)
> m 
$set
function(y) {
        x <<- y
        i <<- NULL
    }
<bytecode: 0x0000000a92270180>
<environment: 0x0000000a9946c498>

$get
function() x
<bytecode: 0x0000000a91c07bc0>
<environment: 0x0000000a9946c498>

$setinverse
function(inverse) i <<- inverse
<bytecode: 0x0000000a906c3c10>
<environment: 0x0000000a9946c498>

$getinverse
function() i
<bytecode: 0x0000000a90bccd90>
<environment: 0x0000000a9946c498>

# Inverse returned after computation
> cacheSolve(m)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

# Inverse returned from cache
> cacheSolve(m)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
