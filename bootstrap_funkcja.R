#X- wektor danych, n- liczba powtórzeń bootstrapowych, E-funkcja estymująca (np. mean)

bootstrap <- function(X, n, ES){ 
  mi <- mean(X)
  boot <- lapply(1:n, function(i) sample(X, replace = T))
  e <- sapply(boot, ES)
  return (e)
}

bootstrap2 <- function(X, n, ES){
  N=length(X)
  boot_stat=c()
  for (i in 1:n){
    boot_stat[i]=ES(sample(X, N, replace=T))
  }
  return(boot_stat)
}

jackknife <- function(X, ES) {
  n <- length(X)
  ests <- numeric(n)
  for (i in 1:n)
    ests[i] <- ES(X[-i])
  return (ests)}

library(FNN)
statysytkaKNN <- function(dane, neighbour, wartosc){ #dane w data.frame, liczba dla którego sąsiada, dlugosc pierwsego wektora
  NS <- get.knn(dane, k=neighbour)
  index <- NS$nn.index
  block1 <- NS$nn.index[1:3, ]
  block2 <- NS$nn.index[4:6, ]
  i1 <- sum(block1 <= wartosc)
  i2 <- sum(block2 > wartosc)
  wektor <- c(i1, i2)
  statysytka <- sum(wektor)/neighbour*nrow(dane)
  return(statysytka)
}

bootstrapKNN <- function(X, wartosc, n, neighbour){ #X- wektor danych, n- liczba powtórzeń bootstrapowych
  N1 <- statysytkaKNN(X, neighbour, wartosc) #statystyka oryginalna
  boot <- lapply(1:n, function(i) sample(as.matrix(X), replace = T)) #próby bootstrapowe
  n.test <- sapply(boot, function(i) statysytkaKNN(as.matrix(i), neighbour, wartosc)) #statystyki bootstrapowe
  cnt <- 0
  for (i in 1:length(n.test)){
    if(n.test[i]>=N1){cnt=cnt+1}
  }
  p_value <- (cnt+1)/(n+1)
  return((p_value))}



######
#ANOVA
#H0: Wytrzymałość materiały jest taka sama. mi1=mi2
#HA: Wytrzymałość materiału jest różna. 
all <- function(dane){
  sum_row=apply(dane,1,sum)
  up = sum(sum_row)^2
  down = prod(dim(dane))
  return(wartosc = up/down)
}

SSt <- function(dane){
  all_do_2 = apply(dane, 1:2, function(x) x^2)
  return (sum(all_do_2) - all(dane))
}

SSA <- function(dane){
  sum_row=apply(dane,1,sum)
  sum_row_do_2=sum(sum_row^2)
  first = sum_row_do_2/ncol(dane)
  return(first-all(dane))
}

SSB <- function(dane){
  sum_col=apply(dane,2,sum)
  sum_col_do_2=sum(sum_col^2)
  first = sum_col_do_2/nrow(dane)
  return(first-all(dane))
}

SSE <- function(dane){
  return(SSt(dane) - SSA(dane) - SSB(dane))
}

MS <- function(dane){
  a <- (nrow(dane)-1)
  b <- (ncol(dane)-1)
  MS1 <- SSA(dane)/a
  MS2 <- SSB(dane)/b
  MS3 <- SSE(dane)/(a*b)
  return(list(MS1, MS2, MS3))
}

statF <- function(dane){
  a <- (nrow(dane)-1)
  b <- (ncol(dane)-1)
  MS1 <- SSA(dane)/a
  MS2 <- SSB(dane)/b
  MS3 <- SSE(dane)/(a*b)
  F1 <- MS1/MS3
  F2 <- MS2/MS3
  statF <- F1/F2
  p=pf(statF,a-1,prod(dim(dane))-a)
  structure(list('SSA'=SSA(dane), 'SSB'=SSB(dane), 'SSE'=SSE(dane) ,'MS1'=MS1, 'MS2'=MS2, 
                 'MS3'=MS3, 'F1'=F1, 'F2'=F2, 'F' =statF, 'p'=p))
}

##############
#estymowanie brakującej wartości w macierzy wzorem yatesa
#tab[1,] -> wiersz 282
#tab[,1] -> kolumna 227
ENAN <- function(dane, nrow, ncol){
  n_row <- length(dane[nrow, ])
  n_col <- length(dane[ ,ncol])
  sum_row <- apply(dane[nrow, ], 1, sum,na.rm=TRUE)
  sum_col <- apply(dane[ ,ncol, drop=F], 2, sum, na.rm=TRUE)
  all <- sum(dane, na.rm=TRUE)
  E <- ((n_row*sum_row) + (n_col*sum_col - all))/((n_row-1)*(n_col-1))
  return(E)
}
