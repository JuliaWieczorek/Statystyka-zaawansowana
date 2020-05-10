partie <- c(rep(1, 1), rep(2, 1), rep(3, 1), rep(4, 1), rep(5, 1))
chemik <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5), rep(5, 5))
preparaty <- c('A', 'B', 'C', 'D', 'E', 'B', 'C', 'D', 'E', 'A', 'C', 'D', 'E', 'A', 'B', 'D', 'E', 'A', 'B', 'C', 'E', 'A', 'B', 'C', 'D')
sila <- c(-1, -8, -7, 1, -3, -5, -1, 13, 6, 5, -6, 5, 1, 1, -5, -1, 2, 2, -2, 4, -1, 11, -4, -3, 6)
dane <- data.frame(partie, chemik, preparaty, sila)
A <- dane$sila[dane$preparaty=='A']
B <- dane$sila[dane$preparaty=='B']
C <- dane$sila[dane$preparaty=='C']
D <- dane$sila[dane$preparaty=='D']  
E <- dane$sila[dane$preparaty=='E']
Z <- cbind(A, B, C, D, E)
X <- matrix(sila, 5, 5)
apply(X, 2, sum) #suma kolumn
apply(X, 1, sum) #suma wierszy
apply(Z, 2, sum) #suma obiektów
SST(X) #ok
SSR(X) #ok
SSC(X) #ok
SSA <- function(dane, litery){
  sum_col=apply(litery,2,sum)
  sum_col_do_2=sum(sum_col^2)
  first = sum_col_do_2/nrow(dane)
  return(first-all(dane))
}
SSA(X, Z) #ok

SSE1 = SST(X) - SSA(X, Z) - SSR(X) - SSC(X)

MS <- function(dane_ma, p, litery, SSE){
  MSA <- SSA(dane_ma, litery)/(p-1)
  MSR <- SSR(dane_ma)/(p-1)
  MSC <- SSC(dane_ma)/(p-1)
  MSE <- SSE/((p-2)*(p-1))
  return(structure(list('MSA'=MSA, 'MSR'=MSR, 'MSC'=MSC, 'MSE'=MSE, 'stopnie'=p-1)))
}
MS(X, 5, Z, SSE1)

statF <- function(dane_ma, p,litery, SSE){
  s1 <- MS(dane_ma, p, litery, SSE)$MSA
  s2 <- MS(dane_ma, p, litery, SSE)$MSE
  return(s1/s2)
}
statF(X, 5, Z, SSE1)

p=5
ms=MS(X, p, Z)
A <- c(SSA(X, Z), p-1, ms$MSA, NA)
R <- c(SSR(X), p-1, ms$MSR, statF(X, p, Z))
C <- c(SSC(X), p-1, ms$MSC, NA)
E <- c(SSE(X, Z), (p-2)*(p-1), ms$MSE, NA)
t <- c(SST(X), ((p^2)-1), NA, NA)

tabelka <- rbind(A, R, C, E, t)

colnames(tabelka) <- c('SS', 'df', 'MS', 'F')
rownames(tabelka) <- c('obiekty', 'wiersze', 'kolumny', 'błąd', 'całkowita')
tabelka_zad2a = tabelka
tabelka_zad2a
qf(.95, df1=p-1, df2=(p-2)*(p-1)) #zapytac się Eweline
