## caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()){
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setsolve<-function(solve)m<<-solve
    getsolve<-function()m
    list(set=set,get=get,
         setsolve=setsolve,
         getsolve=getsolve)
}

cacheSolve <- function(x, ...) {
    m<-getElement(x,"getsolve")()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-getElement(x,"get")()
    m<-solve(data,...)
    getElement(x,"setsolve")(m)
    m
}
