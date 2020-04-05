#zad. 1 Wygenerować próbę 20 elementową z rozkładu normalnego
x = rnorm(20)
hist(x)

#zad. 2 Wyznaczyć estymator średniej korzystając z metody bootstrap oraz jackknife
#metoda bootstrap
boot <- lapply(1:1, function(i) sample(x, replace = T)) 
      #lapply przechodzi przez zestaw danych, takich jak lista lub wektor, i wywołując określoną funkcję dla każdego elementu
      #w tym wypadku tą funkcją jest sample, która losowo zmienia kolejność elementów
      #1:4 oznacza ilość prób bootstrapowych

e.mean <- sapply(boot, mean) #oblicza średnia dla każdej z próby bootstrapowej
e.mean
hist(e.mean) #wyświetlanie histogramu rozkładu median

jackknife <- function(X, ES) { #X - wektor danych, ES- funkcja estymująca (np. mean)
  n <- length(X)
  ests <- numeric(n)
  for (i in 1:n)
    ests[i] <- ES(X[-i])
    return (ests)}

#zad. 3 Oszacuj błąd standardowy i obciążenia dla estymatora średniej.
standard.error <- function(x) { #błąd std.
  sd(x)/sqrt(length(x))
}

S2var <- function(X){ #wariancja
  m <- mean(X)
  sum((x-m)^2)/(length(X))
}

bootstrap <- function(X, n){ #X- wektor danych, n- liczba powtórzeń bootstrapowych
  mi <- mean(X)
  boot <- lapply(1:n, function(i) sample(X, replace = T))
  e.mean <- sapply(boot, mean)
  se <- standard.error(e.mean)
  bias <- mean(e.mean) - mi 
  structure(list("SE"=se, "obciążenie estymatora średniej"=bias, "estymator średniej" = e.mean))
}

jackknife2 <- function(X, ES) { 
  mi <- mean(X) 
  n <- length(X)
  ests <- numeric(n)
  for (i in 1:n)
    ests[i] <- ES(X[-i])
  se = standard.error(ests)
  bias <- mean(ests)- mi
  structure(list("SE" = se, "obciążenie estymatora średniej" = bias, "estymator średniej" = ests))}

bootstrap(x, 20)
jackknife2(x, mean)

#zad. 4 Powtórzyć powyższe postępowanie 100-krotnie. 
#Czy obie metody dają powtarzalne wyniki?
#Czy jakąś metodę można uznać za lepszą?

#powtórzenie 100-krotne
for (i in 1:100){
  x = rnorm(20, 0, 1)
  print(bootstrap(x, 10))
  print(jackknife2(x, mean))
}

#prób 100
x = rnorm(100, 0, 1)
bootstrap(x, 100)
jackknife2(x, mean)


#zad. 5 Wykonaż powyższe punkty dla dwóch innych wybranych rozkłądów prawdopodobieństwa.

#powtórzenie 100-krotne
X2 <- rchisq(20, df = 15)
poisson <- rpois(20, lambda = 3)
for (i in 1:100){
  print(bootstrap(X2, 5))
  print(jackknife_sd(X2, mean))
  print(bootstrap(poisson, 5))
  print(jackknife_sd(poisson, mean))}

#prób 100
X2 <- rchisq(100, df = 15)
poisson <- rpois(100, lambda = 3)
bootstrap(X2, 100)
jackknife_sd(X2, mean)
bootstrap(poisson, 100)
jackknife2(poisson, mean)