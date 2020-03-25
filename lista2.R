#zad. 1 Wygenerować próbę 20 elementową z rozkładu normalnego
x = rnorm(20, 0, 1)

#zad. 2 Wyznaczyć estymator średniej korzystając z metody bootstrap oraz jackknife
boot <- lapply(1:5, function(i) sample(x, replace = T))
e.mean <- sapply(boot, mean)
e.mean

jackknife <- function(X, ES) {
  n <- length(X)
  ests <- numeric(n)
  for (i in 1:n)
    ests[i] <- ES(X[-i])}

#zad. 3 Oszacuj błąd standardowy i obciążenia dla estymatora średniej.
sqrt(var(e.mean)) #błąd std bootstrap
print(paste("błą"))

varnieob <- var(x)
n <- length(x)
varob <- varnieob*((n-1)/n)
varnieob
varob

S2var <- function(X){
  m <- mean(X)
  v <- sum((x-m)^2)/(length(X))
  return(v)
}
rm.var <- sapply(boot, S2var)
mean(rm.var) #średnia z obciążenia estymatora średniej z próby[4]

jackknife_sd <- function(X, ES) {
  n <- length(X)
  ests <- numeric(n)
  for (i in 1:n)
    ests[i] <- ES(X[-i])
  sqrt((n-1) * mean((ests - mean(ests))^2))}
