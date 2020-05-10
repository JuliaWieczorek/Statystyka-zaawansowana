#zadanie 1
kw1 <- matrix(c(1, 2, 3, 4, 2, 3, 4, 1, 3, 4, 1, 2, 4, 1, 2, 3), 4, 4)
kw2 <- matrix(c(1, 2, 3, 4, 2, 3, 4, 1, 3, 4, 1, 2, 4, 1, 2, 3), 4, 4)
kw3 <- matrix(c(1, 2, 3, 4, 2, 4, 1, 3, 3, 1, 4, 2, 4, 3, 2, 1), 4, 4)
kw4 <- matrix(c(1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 2, 1, 4, 3, 1, 2), 4, 4, )
kw1
kw2
kw3
kw4

#zadanie 2

#dane
partie <- c(rep(1, 1), rep(2, 1), rep(3, 1), rep(4, 1), rep(5, 1))
dni <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5), rep(5, 5))
skladniki <- c(1, 3, 2, 4, 5,
               2, 5, 1, 3, 4,
               4, 1, 3, 5, 2,
               3, 4, 5, 2, 1,
               5, 2, 4, 1, 3)

skladniki <- c('A', 'C', 'B', 'D', 'E',
               'B', 'E', 'A', 'C', 'D',
               'D', 'A', 'C', 'E', 'B',
               'C', 'D', 'E', 'B', 'A',
               'E', 'B', 'D', 'A', 'C')
reakcja <- c(8, 11, 4, 6, 4, 
             7, 2, 9, 8, 2, 
             1, 7, 10, 6, 3,
             7, 3, 1, 6, 8, 
             3, 8, 5, 10, 8)

dane <- data.frame(partie, dni, skladniki, reakcja)
X = matrix(dane$reakcja, 5, 5)
X #reakcja
A <- dane$reakcja[dane$skladniki=='A']
B <- dane$reakcja[dane$skladniki=='B']
C <- dane$reakcja[dane$skladniki=='C']
D <- dane$reakcja[dane$skladniki=='D']  
E <- dane$reakcja[dane$skladniki=='E']
Z <- cbind(A, B, C, D, E)

#a)
summary(dane$reakcja)
par(mfrow=c(2,2))
plot(reakcja~partie+dni+skladniki, dane)

all <- function(dane){
  sum_row=apply(dane,1,sum)
  up = sum(sum_row)^2
  down = prod(dim(dane))
  return(wartosc = up/down)
}

SST<-function(dane){
  return(sum(dane^2)-all(dane))
}

SSA <- function(dane, litery){
  sum_col=apply(litery,2,sum)
  sum_col_do_2=sum(sum_col^2)
  first = sum_col_do_2/nrow(dane)
  return(first-all(dane))
}

SSR <- function(dane){
  sum_row=apply(dane,1,sum)
  sum_row_do_2=sum(sum_row^2)
  first = sum_row_do_2/ncol(dane)
  return(first-all(dane))
}

SSC <- function(dane){
  sum_col=apply(dane,2,sum)
  sum_col_do_2=sum(sum_col^2)
  first = sum_col_do_2/nrow(dane)
  return(first-all(dane))
}

SSE <- function(dane, litery){
  return(SST(dane) - SSA(dane, litery) - SSR(dane) - SSC(dane))}

MS <- function(dane_ma, p, litery){
  MSA <- SSA(dane_ma, litery)/(p-1)
  MSR <- SSR(dane_ma)/(p-1)
  MSC <- SSC(dane_ma)/(p-1)
  MSE <- SSE(dane_ma, litery)/((p-2)*(p-1))
  return(structure(list('MSA'=MSA, 'MSR'=MSR, 'MSC'=MSC, 'MSE'=MSE, 'stopnie'=p-1)))
}

statF <- function(dane_ma, p,litery){
  s1 <- MS(dane_ma, p, litery)$MSA
  s2 <- MS(dane_ma, p, litery)$MSE
  return(s1/s2)
}

#H0: Składniki nie wpływają na czas reakcji chemicznej.
#HA: Składniki mają istotny wplyw na czas reakcji chemicznej.
#alfa = 0.05

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
qf(.95, df1=p-1, df2=(p-2)*(p-1)) #3.259167
#F=11.30917
#F>qf -> HA

#b) 
dane[((dane$dni==4 & dane$partie==3) == TRUE),] = NA
X[3,4]<- NA

ENAN_square <- function(dane_asmatrix, dane_asdataframe, ncol, nrow, p){
  A <- dane_asdataframe$skladniki[nrow]
  y.A.sum <- sum(dane_asdataframe$reakcja[dane_asdataframe$skladniki==A], na.rm=TRUE) #j
  sum_row <- sum(dane_asmatrix[nrow, ], na.rm=TRUE) #i
  sum_col <- sum(dane_asmatrix[ ,ncol], na.rm=TRUE) #k
  all <- sum(dane_asmatrix, na.rm=TRUE)
  E <- (p*(sum_row+y.A.sum+sum_col)-2*all)/((p-2)*(p-1))
  return(E)
}
e <- ENAN_square(X, dane, 4, 3, 5)
X[3,4]<- e
dane[18,]$partie = 3
dane[18,]$dni=4
dane[18,]$skladniki='A'
dane[18,]$reakcja=e

p=5
ms=MS(X, p, Z)
Ab <- c(SSA(X, Z), p-1, ms$MSA, NA)
Rb <- c(SSR(X), p-1, ms$MSR, statF(X, p, Z))
Cb <- c(SSC(X), p-1, ms$MSC, NA)
Eb <- c(SSE(X, Z), (p-2)*(p-1), ms$MSE, NA)
tb <- c(SST(X), ((p^2)-1), NA, NA)

tabelkab <- rbind(Ab, Rb, Cb, Eb, tb)

colnames(tabelkab) <- c('SS', 'df', 'MS', 'F')
rownames(tabelkab) <- c('obiekty', 'wiersze', 'kolumny', 'błąd', 'całkowita')
tabelka_zad2b = tabelkab
tabelka_zad2b

##ANOVA
#H0: Składniki nie wpływają na czas reakcji chemicznej.
#HA: Składniki mają istotny wplyw na czas reakcji chemicznej.
#alfa = 0.05
#model <- lm(reakcja~partie+dni+skladniki, dane)
#anova(model)
tabelka_zad2b #1.19573
#stopnie swobody F(p-1,(p-2)(p-1))
qf(.95, df1=p-1, df2=(p-2)*(p-1)) #3.259167 
#F<qf -> H0

#c)

mi = mean(X) #6.2
alfa_i = sum(apply(X, 1, mean) -mi) #efekt i-tego wiersza (z/bez -mi)
tao_i = sum(apply(Z, 2, mean)-mean(Z)) #efekt j-tego obiektu (z/bez -mi)
beta_i = sum(apply(X, 2, mean)-mi) # efekt k-tej kolumny (z/bez -mi)
#mi_i = mi + tao_i

#d)
#PRZEDZIAŁY UFNOŚCI
#Wyznacz przedziały ufności dla różnic średnich testowanych składników
alfa=0.05
r <- Z-mi
sapply(r, function(x) quantile(r[,x], probs = c(alfa/2, 1-alfa/2)))


#zadanie3
montaz <- c(rep(1, 1), rep(2, 1), rep(3, 1), rep(4, 1))
operator<- c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4))
metody <- c('C', 'B', 'A', 'D', 'D', 'C', 'B', 'A', 'A', 'D', 'C', 'B', 'B', 'A', 'D', 'C')
czas <- c(10,7,5, 10, 14, 18, 10, 10, 7, 11, 11, 12, 8, 8, 9, 14)
dane3 <- data.frame(montaz, operator, metody, czas)
A3 <- dane3$czas[dane3$metody=='A']
B3 <- dane3$czas[dane3$metody=='B']
C3 <- dane3$czas[dane3$metody=='C']
D3 <- dane3$czas[dane3$metody=='D']  
Z3 <- cbind(A3, B3, C3, D3)
X3 <- matrix(czas, 4,4)

p3=4
ms3=MS(X3, p3, Z3)
A3 <- c(SSA(X3, Z3), p3-1, ms3$MSA, NA)
R3 <- c(SSR(X3), p3-1, ms3$MSR, statF(X3, p3, Z3))
C3 <- c(SSC(X3), p3-1, ms3$MSC, NA)
E3 <- c(SSE(X3, Z3), (p3-2)*(p3-1), ms3$MSE, NA)
t3 <- c(SST(X3), ((p3^2)-1), NA, NA)

tabelka3 <- rbind(A3, R3, C3, E3, t3)

colnames(tabelka3) <- c('SS', 'df', 'MS', 'F')
rownames(tabelka3) <- c('obiekty', 'wiersze', 'kolumny', 'błąd', 'całkowita')
tabelka_zad3a = tabelka3
tabelka_zad3a #F=13.80952
qf(.95, df1=p3-1, df2=(p3-2)*(p3-1)) #4.757063
#F>qf -> HA

#zadanie 4
stanowisko_pracy <- c('b', 'a', 'd', 'g',
                      'g', 'd', 'a', 'b',
                      'd', 'g', 'b', 'a', 
                      'a', 'b', 'g', 'd')
dane4 <- data.frame(montaz, operator, metody, czas, stanowisko_pracy)
a <- dane4$czas[dane4$stanowisko_pracy=='a']
b <- dane4$czas[dane4$stanowisko_pracy=='b']
d <- dane4$czas[dane4$stanowisko_pracy=='d']
g <- dane4$czas[dane4$stanowisko_pracy=='g']
Y <- cbind(a, b, d, g)
#dane4
#X3 #czas
#Y #stanowisko_pracy
#Z3 #metody
SSLG <- function(dane, litery){
  sum_col=apply(litery,2,sum)
  sum_col_do_2=sum(sum_col^2)
  first = sum_col_do_2/nrow(dane)
  return(first-all(dane))
}


SSE4 <- function(dane, litery1, litery2){
  return((SST(dane) - SSLG(dane, litery1) - SSLG(dane, litery2) - SSR(dane) -SSC(dane)))}

MS4 <- function(dane_ma, p, litery1, litery2){
  MSL <- SSLG(dane_ma, litery1)/(p-1)
  MSR <- SSR(dane_ma)/(p-1)
  MSC <- SSC(dane_ma)/(p-1)
  MSG <- SSLG(dane_ma, litery2)/(p-1)
  MSE <- SSE4(dane_ma, litery1, litery2)/((p-2)*(p-1))
  return(structure(list('MSL'=MSL, 'MSR'=MSR, 'MSC'=MSC, 'MSG'=MSG, 'MSE'=MSE, 'stopnie'=p-1)))
}


statF4 <- function(dane_ma, p, litery1, litery2){
  s1 <- MS4(dane_ma, p, litery1, litery2)$MSL
  s2 <- MS4(dane_ma, p, litery1, litery2)$MSE
  return(s1/s2)
}
ms4=MS4(dane_ma=X3, p=p3, litery1=Z3, litery2=Y)
A4 <- c(SSLG(X3, Z3), p3-1, ms4$MSA, NA)
R3 <- c(SSR(X3), p3-1, ms4$MSR, statF4(dane_ma=X3, p=p3, litery1=Z3, litery2=Y))
C3 <- c(SSC(X3), p3-1, ms4$MSC, NA)
G4 <- c(SSLG(X3, Y), p3-1)
E3 <- c(SSE4(X3, Z3, Y), (p3-2)*(p3-1), ms4$MSE, NA)
t4 <- c(SST(X3), ((p3^2)-1), NA, NA)

tabelka4 <- rbind(A3, R3, C3, E3, t4)

colnames(tabelka4) <- c('SS', 'df', 'MS', 'F')
rownames(tabelka4) <- c('obiekty', 'wiersze', 'kolumny', 'błąd', 'całkowita')
tabelka_zad4 = tabelka4
tabelka_zad4 #145
qf(.95, df1=p3-1, df2=(p3-2)*(p3-1)) #4.757
#F>qf -> HA