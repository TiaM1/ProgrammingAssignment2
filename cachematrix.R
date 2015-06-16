## makeCachematrix creates a matrix that can cache its inverse
## cacheSolve looks for the cached inverse and if it doesn't exist it calculates it


makeCacheMatrix <- function (x = matrix()){
  m = NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmat <- function(solve) m <<-solve
  getinvmat <- function() m
  list (set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
}




cacheSolve <- function(x = matrix, ...){
  m <- x$getinvmat()
  if(!is.null(m)){
    message("getting cached")
    return(m)
  } else {
    data <- x$get()
    invmat <- solve(data, ...)
    x$setinvmat(m)
    m
  }
}
