#zad. 1 Wygenerować próbę 20 elementową z rozkładu normalnego
x = rnorm(20, 0, 1)
hist(x)

#zad. 2 Wyznaczyć estymator średniej korzystając z metody bootstrap oraz jackknife
boot <- lapply(1:20, function(i) sample(x, replace = T)) #metoda bootstrap
e.mean <- sapply(boot, mean)
e.mean
hist(e.mean)
mean(e.mean)

#metoda jackknife
jackknife <- function(X, ES) { #X - wektor danych, ES- funkcja estymująca (np. mean)
  n <- length(X)
  ests <- numeric(n)
  for (i in 1:n)
    ests[i] <- ES(X[-i])
    hist(ests)
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
  boot <- lapply(1:n, function(i) sample(X, replace = T))
  se <- standard.error(X) #dodać obciążenie
  e.mean <- sapply(boot, mean)
  rm.var <- sapply(boot, S2var)
  ob <- mean(e.mean) #nie jestem pewna, które to będzie obciążenie estymatora śr.
  structure(list("SE"=se, "obciążenie estymatora średniej"=rm.var, "estymator średniej" = e.mean))
}

jackknife_sd <- function(X, ES) { #metoda jackknife z 
  n <- length(X)
  ests <- numeric(n)
  for (i in 1:n)
    ests[i] <- ES(X[-i])
  se = standard.error(ests)
  ob <- sqrt((n-1) * mean((ests - mean(ests))^2))
  structure(list("SE" = se, "obciążenie estymatora średniej" = ob, "estymator średniej" = ests))}


#zad. 4 Powtórzyć powyższe postępowanie 100-krotnie. 
#Czy obie metody dają powtarzalne wyniki?
#Czy jakąś metodę można uznać za lepszą?

for (i in 1:100){
  x = rnorm(20, 0, 1)
  print(bootstrap(x, 10))
  print(jackknife_sd(x, mean))
}

#zad. 5 Wykonaż powyższe punkty dla dwóch innych wybranych rozkłądów prawdopodobieństwa.

X2 <- rchisq(20, df = 15)
for (i in 1:100){
  poisson <- rpois(20, lambda = 3)
  bootstrap(X2, 5)
  jackknife_sd(X2, mean)
  bootstrap(poisson, 5)
  jackknife_sd(poisson, mean)}