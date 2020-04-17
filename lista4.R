# zadanie 1
# Dla zbioru danych iris
# a) Dla każdego rodzaju osobno w oparciu o 200 prób bootstrapowych oszacuj średnią i medianę długości płatka

library(datasets)
data(iris)
summary(iris)

virginica <- filter(iris, Species == "virginica")
setosa <- filter(iris, Species == "setosa")
versicolor <- filter(iris, Species == "versicolor")

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
# Linia produkcyjna mikroprocesorów w kolejnych dniach dawała następujące liczby wadliwych elementów:
# 12, 9, 21, 14, 7, 17. W związku z tym dokonano remontu, po którym przez pierwsze sześć dni liczby braków wyniosły:
# 5, 9, 3, 11, 8, 19. Czy na poziomie isotności 0.05 można twierdzić, że w wyniku remontu liczba braków zmienila się o 15%?