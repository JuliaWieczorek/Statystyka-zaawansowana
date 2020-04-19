# zadanie 1
# Dla zbioru danych iris
# a) Dla każdego rodzaju osobno w oparciu o 200 prób bootstrapowych oszacuj średnią i medianę długości płatka

library(datasets)
data(iris)
summary(iris)

virginica <- iris[iris$Species == "virginica",]
setosa <- iris[iris$Species == "setosa", ]
versicolor <- iris[iris$Species == "versicolor", ]

bootstrap <- function(X, n){ #X- wektor danych, n- liczba powtórzeń bootstrapowych
  mi <- mean(X)
  boot <- lapply(1:n, function(i) sample(X, replace = T))
  e.mean <- sapply(boot, mean)
  e.median <- sapply(boot, median)
  structure(list("estymator średniej"=e.mean, "estymator mediany"=e.median))
}

virginica_boot = bootstrap(virginica$Petal.Length, 200)
setosa_boot = bootstrap(setosa$Petal.Length, 200)
versicolor_boot= bootstrap(versicolor$Petal.Length, 200)

plot(virginica_boot$'estymator średniej')
plot(setosa_boot$'estymator średniej')
plot(versicolor_boot$'estymator średniej')


plot(virginica_boot$'estymator mediany')
plot(setosa_boot$'estymator mediany')
plot(versicolor_boot$'estymator mediany')

# b) Wyznacz bootstrapowe przedziały ufności dla średniej i mediany dla zmiennej długości płatka. 
N <- 10
stata <- numeric(N)
statb <- numeric(N)
statc <- numeric(N)
statx <- numeric(N)
staty <- numeric(N)
statz <- numeric(N)

for (i in 1:N){
  a <- sample(virginica_boot$'estymator średniej', replace=T)
  b <- sample(setosa_boot$'estymator średniej', replace=T)
  c <- sample(versicolor_boot$'estymator średniej', replace=T)
  x <- sample(virginica_boot$'estymator mediany', replace=T)
  y <- sample(setosa_boot$'estymator mediany', replace=T)
  z <- sample(versicolor_boot$'estymator mediany', replace=T)
  stata[i] <- mean(a)
  statb[i] <- mean(b)
  statc[i] <- mean(c)
  statx[i] <- mean(x)
  staty[i] <- mean(y)
  statz[i] <- mean(z)
}

quantile(stata, c(0.05, 0.95))
quantile(statb, c(0.05, 0.95))
quantile(statc, c(0.05, 0.95))
quantile(statx, c(0.05, 0.95))
quantile(staty, c(0.05, 0.95))
quantile(statz, c(0.05, 0.95))

# c) Wykonaj powyższe zadania dla 3000 replikacji

N=3000

virginica_boot = bootstrap(virginica$Petal.Length, N)
setosa_boot = bootstrap(setosa$Petal.Length, N)
versicolor_boot= bootstrap(versicolor$Petal.Length, N)

plot(virginica_boot$'estymator średniej')
plot(setosa_boot$'estymator średniej')
plot(versicolor_boot$'estymator średniej')

plot(virginica_boot$'estymator mediany')
plot(setosa_boot$'estymator mediany')
plot(versicolor_boot$'estymator mediany')

stata <- numeric(N)
statb <- numeric(N)
statc <- numeric(N)
statx <- numeric(N)
staty <- numeric(N)
statz <- numeric(N)

for (i in 1:N){
  a <- sample(virginica_boot$'estymator średniej', replace=T)
  b <- sample(setosa_boot$'estymator średniej', replace=T)
  c <- sample(versicolor_boot$'estymator średniej', replace=T)
  x <- sample(virginica_boot$'estymator mediany', replace=T)
  y <- sample(setosa_boot$'estymator mediany', replace=T)
  z <- sample(versicolor_boot$'estymator mediany', replace=T)
  stata[i] <- mean(a)
  statb[i] <- mean(b)
  statc[i] <- mean(c)
  statx[i] <- mean(x)
  staty[i] <- mean(y)
  statz[i] <- mean(z)
}

quantile(stata, c(0.05, 0.95))
quantile(statb, c(0.05, 0.95))
quantile(statc, c(0.05, 0.95))
quantile(statx, c(0.05, 0.95))
quantile(staty, c(0.05, 0.95))
quantile(statz, c(0.05, 0.95))

# zadanie 2
# Linia produkcyjna mikroprocesorów w kolejnych dniach dawała
#następujące liczby wadliwych elementów:
# 12, 9, 21, 14, 7, 17. W związku z tym dokonano remontu, 
#po którym przez pierwsze sześć dni liczby braków wyniosły:
# 5, 9, 3, 11, 8, 19. Czy na poziomie isotności 0.05 można twierdzić,
#że w wyniku remontu liczba braków zmienila się o 15%?

#test permutacyjny dla wariancji

#H0: Brak istotnych różnic statystycznych w liczbie wadliwych elementów.
#HA: Istenieją istotne różnice w liczbie wadliwych elementów.

diff <- function(samp) {
  s = sort(samp)
  l = length(samp)
  d = 1:(l-1)
  for(k in 2:l){
    d[k-1] = s[k] - s[k-1]
  }
  return(d)
}

aly <- function(samp) {
  stat = 0
  l=length(samp)
  for(k in l:1)
    stat=stat+k*(l+1-k)*samp[k]
  return(stat)
  
}

vartest <- function(samp1, samp2, NMonte){
  d1=diff(samp1)
  d2=diff(samp2)
  l=length(d1)
  stat0=aly(d1)
  pd=d1
  cnt=0
  for(j in 1:NMonte){
    r=rbinom(l,1,.5)
    for(k in 1:l){
      pd[k]=ifelse(r[k], d1[k], d2[k])
    }
    if(aly(pd)>stat0) cnt=cnt+1
  }
  return(cnt/NMonte)
}

elem1 <- c(12, 9, 21, 14, 7, 17)
elem2 <- c(5, 9, 3, 11, 8, 19)
elem1v1 <- elem1*0.85 #założenie o 15%
vartest(elem1, elem2, 200)
#0.515
vartest(elem1v1, elem2, 200)
#0.615

# zadanie 3
#W oparciu o macierz odległości wyznaczyć wartości statystyki trzecich najbliższych sąsiadów.

#literówka z zadaniu X1Y1 albo Y1X1 przyjmuje wartosc 1.78

macierz <- rbind(c(0.00, 2.21, 2.49, 1.78, 3.11, 2.94),
      c(2.21, 0.00, 2.60, 3.27, 1.45, 3.29),
      c(2.49, 2.60, 0.00, 2.14, 0.43, 3.52),
      c(1.78, 3.27, 2.14, 0.00, 2.69, 2.60),
      c(3.11, 1.45, 0.43, 2.69, 0.00, 3.48),
      c(2.94, 3.29, 3.52, 2.60, 3.48, 0.00))
colnames(macierz) <- c('X1', 'X2', 'X3', 'Y1', 'Y2', 'Y3')
rownames(macierz) <- c('X1', 'X2', 'X3', 'Y1', 'Y2', 'Y3')
dane <- data.frame(macierz)
dane
library(FNN)

statysytkaKNN <- function(dane, neighbour, wartosc){ #dane w data.frame, liczba dla którego sąsiada, dlugosc pierwsego wektora
  NS <- get.knn(dane, k=neighbour)
  index <- NS$nn.index
  i1 <- sum(index <= wartosc)
  i2 <- sum(index > wartosc)
  wektor <- c(i1, i2)
  statysytka <- sum(wektor)/neighbour*nrow(dane)
  return(statysytka)
}
wartosc <- nrow(dane)/2
statysytkaKNN(macierz, 3, wartosc)


# zadanie 4
X <- matrix(rnorm(5, mean=5, sd=3))
Y <- matrix(rnorm(6, mean=5, sd=3))
macierz4 <- rbind(X, Y)
o <- rep(0, nrow(macierz4))
dane <- data.frame(cbind(macierz4, o))
dane
library(FNN)
NS <- get.knn(dane, k=nrow(macierz4)-1)

# a) W oparciu o wygenerowane macierze obserwacji, dla każdego elementu
#macierzy Z wyznacz indeks k-tego najbliższego sąsiada
NS$nn.index

# b) W oparciu o macierz wyznaczoną w poprzednim punkcie wyznacz
#wartosci statystyk pierwszych, trzecich i piątych najblizszych
#sąsiadów
w <- length(X)
statysytkaKNN(dane, 1, w)
statysytkaKNN(dane, 3, w)
statysytkaKNN(dane, 5, w)

#c)
bootstrapKNN <- function(X, wartosc, n, neighbour){ #X- wektor danych, n- liczba powtórzeń bootstrapowych
  N1 <- statysytkaKNN(X, neighbour, wartosc) #statystyka oryginalna
  boot <- lapply(1:n, function(i) sample(X, replace = T)) #próby bootstrapowe
  n.test <- sapply(boot, function(i) statysytkaKNN(X, neighbour, wartosc)) #statystyki bootstrapowe
  cnt <- 0 #licznik
  for (i in 1:length(n.test)){
    if(n.test[i]>=N1){cnt=cnt+1}
  }
  p_value <- (cnt+1)/(n+1)
  return((p_value))}

bootstrapKNN(dane, w, 10, nrow(dane)-1)

# zadanie 5
X <- matrix(rnorm(5))
Y <- matrix(rnorm(6))
macierz4 <- rbind(X, Y)
o <- rep(0, nrow(macierz4))
dane <- data.frame(cbind(macierz4, o))
dane
library(FNN)
NS <- get.knn(dane, k=nrow(macierz4)-1)

#a)
NS$nn.index

#b)
w <- length(X)
statysytkaKNN(dane, 1, w)
statysytkaKNN(dane, 3, w)
statysytkaKNN(dane, 5, w)

#c)
bootstrapKNN(dane, w, 10, nrow(dane)-1)


