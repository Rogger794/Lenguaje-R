#Operaciones con R

# Numeros complejos

(1i)^2
(1 + 5i) + (2 -i)
(1 + 5i) - (4i)
(1 +i) * (1 +i)
(1 + 2i) / (3 +4i)
(1i + (1i)^2 + (1i)^3 + (1i)^4 + (1i)^5) / (1 + 1i)

Re(3+2i); Im(3+2i)
Mod(3+2i); Arg(3-5i)
Conj(8 -6i)

exp(pi/4*1i)
cos(pi/4) +1i*sin(pi/4)

(-8)^(1/3)
(-8+0i)^(1/3)

# Muestra numerica, error de redondeo y redondeo

Arg(3+ 4i)
options(digits=7)

round(1234.679)
round(1234.679, -2)
round(1234.679, 2)

signif(1234.679)
signif(1234.679, 2)

ceiling(3.9)
ceiling(4.2)

floor(3.2)
floor(5.6)

trunc(3.7)
trunc(7.1)

options(digits=7)

x <- runif(3)
x
x + 123456
x
formatC(x, digits = 2, big.mark = ",", format = "f")
formatC(x, digits = 7, big.mark = ",", format = "E")


.7-.6-.1
.7/.1-7
.7/.1
zapsmall(.7/.1) - 7

1/0
log(0)
Inf*Inf
Inf/Inf
0/0

# Asignando variables

t <- pi/4
st <- sin(t)
t
st
asin(st)

# Multiple asignamiento
sec. = 1; min. = 60*sec.; hr. = 60*min.; dia. = 24*hr.
sem. = 7*dia.; yr. = 365.25*dia.; siglo. = 100*yr.
3*siglo./sec.

grado. = pi/180
sin(30*grado.)

# Encuentra todas las variables en el entorno R. 
ls()

# Remueve todas las variables y mucho otros objetos

rm(list = ls())

# Operadores relacionales

.5 == 1/2
.3/.1 == 3
all.equal(.3/.1,3)

# Vectores

x <-  c(3.2, 1.7, -11.3, -0.67, 4, 0.8)
x[3]
x[1:4]; x[c(2,3,5)]
x[3] <- 10.0
x
x[7] <- 4.3
x
x[9] = 9
x
x[-8]

# Operaciones con vectores

x -1
3 * x
x^2
sin(x/2)
length(x)
mean(x)
sd(x)
var(x)
min(x)
range(x)
sum(x)
prod(x)
cumsum(x)
cumprod(x)

summary(x)

# Recycling

x1 = c(1,2,3,4,5)
y1 = c(1,2)
 x1*y1
 
# Generando secuencias
 
#Secuencias regulares
 
1: 10
5.7:-3.7
 
n = 10
1:n-1

1:(n -1)

seq(3,8,.5)
seq(by=0.45,from=2.7,to=6.7)
seq(-pi,pi,length.out=12) # 12 valores


# Repitiendo valores

y <- 2; rep(y,5)
w <-  c(4,5); rep(w,5)
rep(w, each=5)

# Secuencias de numeros aleatorios

# Secuencia de n numeros uniformemente distribuidos

runif(6,-2,2)
runif(6)


# Secuencia de n numeros aleatorios desde una distribucion normal 

rnorm(6,9,1.5)
rnorm(5)

x = 1:6
err <- rnorm(6,0,0.1)
x + err

x*(1 + err)

set.seed(123)
round(rnorm(5),3)

# Operadores logicos

v <- runif(8,-3,3)
v > 0
which(v>0)
v[v > 0]

v1 <- seq(.1,.6,.1)/.1; v1
w1 <- 1:6; w1

v1 == w1
all.equal(v1, w1)

# Velocidad en la formacion de vectores

n=10000
v = 1
system.time(for (i in 2:n) v[i] = i)


v1 = numeric(n)
system.time(for (i in 2:n) v1[i] = i)

system.time({v2 = 1:n})

head(v2)

tail(v2)

# Producto escalar y producto triple

escalar <-  function(u,v) as.numeric(u%*%v)
vecnorm <- function(v)sqrt(dot(v,v))

u <- c(1,2,3)
v <- c(4,5,6)
escalar(u,v)
vecnorm(u)

norm(as.matrix(u), "F")

triple<- function(u,v) {c(u[2]*v[3]-u[3]*v[2],
                          + u[3]*v[1]-u[1]*v[3], u[1]*v[2]-u[2]*v[1])}
triple(u,v)

#install.packages("pracma")

require(pracma)
dot(u,v)
cross(u,v)


# Matrices

m <- matrix(c(3,-4.2,-7.1,0.95),nrow=2,ncol=2)
m
m <- matrix(c(3,-4.2,-7.1,0.95),nrow=2,byrow = T)
m
m[2,3]
m[2,]
m[,3]

x = 1:3; y = 1:3
rbind(x,y)
cbind(x,y)


diag(c(12,24, 36))
diag(rep(1,3))
matrix(rep(0,9), nrow = 3)


# Usando la funcion outer

x = 1:3; y = 1:3
outer(x,y)
outer(x,y, FUN = "+")

w <- c(1, 2.3, 2,3,4,8,12, 43)
q <-c(2, 4)
outer(w, q, "log")
outer(w,q, "+")
outer(w,q,"*")
t<- c("a", "b")
outer(w, q, "paste")

x <- 1:9; y <-2:8
names(x)<-x; names(y)<-y 
outer(y, x, "^")

# Mas sobre Matrices

A1 <- matrix(1:4, 2, 2)
A1
A2 <- matrix(c(1,0, 0, 1),nrow=2 )
A2

kronecker(A1, A2)
kronecker(A2, A1)

# Submatrices

m3 <- matrix(1:9, 3, 3, byrow = T)
m3
m3[1:2,c(1, 3)]

m1 <- matrix(c(3.78,-4.2,-7.1,0.95, 4.3, -1.2),nrow=2,ncol=3)
rownames(m1) = c("A","B")
colnames(m1) = c("v1","v2","v3")
m1
m1[,"v1"]

summary(m1[, "v1"])

m1 <- matrix(1:6, nrow=2, ncol=3, byrow=T, dimnames = list(c("A","B"),c("v1","v2","v3")))
m1


# Operaciones con matrices

m1 -2
m1/5
options(digits = 3)
sqrt(m1)
m1^{-1}

m1 + m1
m1/m1

# Multiplicacion de matrices

M1 <- matrix(runif(9),3,3); M1
M2 <- matrix(runif(9),3,3); M2

M3 <- M1 %*% M2; M3

t(M1)
det(M1)

A <- matrix(1:4, 2,2)
B <- matrix(5:8, 2, 2)

crossprod(A,B)

t(A) %*% B
tcrossprod(A,B)
A %*% t(B)

require(Matrix)

D <- matrix(1:9,3,3)
D
expm(D)


# Algo de LAPACK

M1_inv <- solve(M1)
solve(solve(M1))

# Probando la igualdad exacta

identical(M1, solve(solve(M1)))

# Igualdad dentro de la precision de maquina

all.equal(M1, solve(solve(M1)))


# Ejemplo

A <- matrix(c(1,1/2,1/3, 1/2,1/3,1/4, 1/3,1/4,1/5),nrow=3, byrow=T)
A_inv <- solve(A)
b <- c(1,0,0)
x <- A_inv %*% b
x

A %*% x
solve(A,b)


# Autovalores y Autovectores

eigen(M1)
eigen(M1)$values

# Ejemplo: Los autovalores de una matriz hermitiana son reales
# y sus autovectores son ortonormales 

MH <- matrix(c(1,2,3, 2,5,-1, 3,-1,7),3,3,byrow=T)
MHval <- eigen(MH)$values
MHval
MHvec <- eigen(MH)$vectors
MHvec
MHvec[, 1]

MHvec[,1]%*%MHvec[,1]
MHvec[,2]%*%MHvec[,3]
MHvec[,3]%*%MHvec[,3]

# Ejemplo: Los autovalores son reales y sus autovectores son ortonormales
# con sus conjugadas complejas, si la matriz es hermitiana compleja.

HI <-matrix(c(1,2+7i,3,  2-7i,5,-1, 3,-1,7),3,3,byrow=T)
HIval <- eigen(HI)$values
HIval
HIvec <- eigen(HI)$vectors
HIvec[, 1]%*% Conj(HIvec[, 1])
HIvec[, 1]%*% Conj(HIvec[, 2])


# SVD (Descomposicion de valores singulares)

# Ejemplo: Matriz de Hilbert

hilbert <- function(n) {i=1:n; 1/outer(i-1,i,"+")}
h4 <- hilbert(4)
h4
s4 <- svd(h4)
s4

eigen(h4)

eigen(h4)$values == svd(h4)$d
all.equal(eigen(h4)$values, svd(h4)$d)

# Paquete Matrix

library(Matrix)
data(CAex)
image(CAex)
CAex.eigval=eigen(CAex, only.values=TRUE)$values
zapsmall(CAex.eigval)

# Tiempo de procesos

n <- 1:1e6
system.time(for (i in n) sqrt(x)) / length(n)
system.time(for (i in n) x ^ 0.5) / length(n)
system.time(cs <- cumsum(sin(runif(1e7))))

