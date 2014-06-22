## Calculate inverse matrix and store it in a 'cache'

## constructor function that takes a square matrix as an argument

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL 
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<-solve
    getinverse<-function() m
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse = getinverse)
}


## calls functions stored in the list returned by the makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse() ## try to get cached version first
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...) ## calculate inverse matrix if not available
    x$setinverse(m) ## and cache it 
    m
}
