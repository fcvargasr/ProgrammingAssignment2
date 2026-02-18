## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Crea un objeto especial "matriz" que puede almacenar en caché su inversa.
#el objeto en cuestion es una lista que contine funciones para:
#establecer(set) el valor de la matriz
#obtener (get) el valor de la matriz
#establecer el valor de la inversa (setInv) y obtener (getInv) el valor de la inevrsa
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  get <- function() x  ## Función para obtener la matriz actual
  setInv <- function(inverse) inv <<- inverse  ## Función para establecer la inversa de la matriz
  getInv <- function() inv## Función para obtener la inversa almacenada en caché
  list(set = set,  ## Retorna una lista con todas las funciones disponibles
       get = get,
       setInv = setInv, 
       getInv = getInv)
}


## Write a short comment describing this function
##Calcula la inversa de la matriz creada en makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv() ### Intenta obtener la inversa almacenada en caché
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)## Retornar la inversa almacenada y salir de la función
  }
  data <- x$get()   ## Obtener la matriz original demakeCacheMatrix
  inv <- solve(data, ...)## Calcula la inversa usando la función solve()
  x$setInv(inv) ## Almacenar la inversa calculada en caché
  inv
}
