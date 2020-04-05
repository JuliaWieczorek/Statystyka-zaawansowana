S2var <- function(X){
  m <- mean(X)
  sum((x-m)^2)/(length(X))
}

standard.error <- function(x) {
  sqrt(var(x)/length(x))
}

variance_nieob <- function(x) {
  n <- length(x)
  m <- mean(x)
  (1/(n - 1)) * sum((x - m)^2)
}

jackknife_sd <- function(X, ES) {
  n <- length(X)
  ests <- numeric(n)
  for (i in 1:n)
    ests[i] <- ES(X[-i])
  sqrt((n-1) * mean((ests - mean(ests))^2))}


print(paste("wariancja nieobciążona metody bootstrap", varnieob <- var(x)))
print(paste("wariancja nieobciążona metody bootstrap, moja funkcja", variance_nieob(x)))
print(paste("długość wektora x", n <- length(x)))
print(paste("wariancja obciążona metody bootstrap", varnieob*((n-1)/n)))

