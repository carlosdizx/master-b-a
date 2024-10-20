#PSeudoaletorios
sample(1:10000,10)


set.seed(999)
sample(1:10000,10)
help(sample)

RNGversion("3.5.3")
RNGkind()
set.seed(999)
sample(1:10000,10)

RNGversion("4.0.3")
RNGkind()
set.seed(999)
sample(1:10000,10)

# Pregunta 1: �Cu�l es el valor de la desviaci�n est�ndar del vector (1, 2, ., 1000)?
sd(1:1000)

# Pregunta 2: �Utilizando la semilla 999, cu�l es el valor m�ximo del
#             vector aleatorio de 10 elementos entre 1 y 10.000?
set.seed(999)
max(sample(1:10000,10))

# Pregunta 3: �C�mo puedo cargar el fichero "mi_file.csv" en R?
myFile <- read.csv("mi_file.csv")

# Pregunta 4: �C�mo se instalar�a el paquete ggplot2 en R?
install.packages("ggplot2")

# Pregunta 5: Crea una funci�n en R que reciba como par�metro un vector y
#             nos lo devuelve en orden inverso
inv <- function(x) {
  v <- c(x[1])
  for(i in 2:length(x)){
    v <- c(x[i], v)
  }
  return(v)
}

inv(c(2,5,8,3))


# Pregunta 6: Crea una funci�n en R que reciba como par�metro un vector y
#             nos lo devuelve en orden inverso
#opci�n 2
rev2 <- function(x) {
  return(rev(x))
}
rev2(1:10)

# Pregunta 7 y 8: Escribe una funci�n en R que reciba un entero como par�metro y
#             escriba tantas veces en pantalla la frase "You can not pass!!!!"
#             como indique ese par�metro (usa la funci�n "print")
r <- function(x){
  for(i in 1:x)
  {
    print('You can not pass!!!!')
  }
}

r(6)

x = 10
for(i in 1:x)
{
  print('You can not pass!!!!')
}

# Pregunta 9: Carga el dataset Iris y dibuja el histograma para la variable Sepal.Width
data(iris)
hist(iris$Sepal.Width)

# Pregunta 10: �C�mo podemos saber el directorio de trabajo actual?
getwd()

# Pregunta 11: Implementa la funci�n de Fibonacci de forma recursiva:
fib <- function(x){
  if(x <= 1)
  {
    return (x);
  }
  else
  {
    return (fib(x-2)+fib(x-1));
  }
}

fib(10)

# Pregunta 12: Implementa el siguiente pseudoc�digo en R
# QUICKSORT(v)
#   Si longitud(v) <= 1 entonces return v;
#   pivot = v[0];
#   izda = "elementos de v menores que pivot"
#   centro = "elementos de v iguales que pivot"
#   dcha = "elementos de v mayores que pivot"
#   return QUICKSORT(izda) + centro + QUICKSORT(dcha);
quicksort <- function(v){
  if(length(v) <= 1)
    return(v)
  pivot <- v[1]
  izda <- v[v < pivot]
  centro <- v[v == pivot]
  dcha <- v[v > pivot]
  return(c(quicksort(izda), centro, quicksort(dcha)))
}

quicksort(c(2,8,1, 4, 8, 0, 7, 7))

quicksort(c(2,8,1, 4, 8, 0, 7, 6))

quicksort(c(2,8,0, 7, 7))

quicksort(sample(1:100000, 10000))







