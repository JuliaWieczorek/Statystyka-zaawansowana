#zad.1
sub1 = c(73, 68, 74, 71, 67)
sub2 = c(73, 67, 75, 72, 70)
sub3 = c(75, 68, 78, 73, 68)
sub4 = c(73, 71, 75, 75, 69)

tabela = rbind(sub1, sub2, sub3, sub4)
tab = as.matrix(tabela)
colnames(tab) = c('1', '2', '3', '4', '5')

#a)
#model yij = mi + efekt substancji chemicznej (i-ty czynnik) + błąd losowy
mean(tab) #71.75

apply(tab, 1, summary)
apply(tab, 2, summary)

apply(tab, 1, shapiro.test) #r.norm dla każdej substancji
apply(tab, 2, shapiro.test) #jeden materiał nie ma

#H0: var1 = var2
#HA: var1 =/ var2 (nie są sobie równe)

apply(tab, 1, var)
apply(tab, 2, var)


var.test(sub1, sub2) #p=1
var.test(sub1, sub3) #p=0.4969
var.test(sub1, sub4) #p=0.768
var.test(sub2, sub3) #p=0.4969
var.test(sub2, sub4) #p=0.769
var.test(sub3, sub4) #p=0.3365

#H0: Substancja nie ma wpływu na wytrzymałość materiału. theta1 = theta2=0
#HA: Substancja ma wpływ na wytrzymałość materiały. theta1=/0 dla przynajmniej jednego materiału

#alfa=0.05
statF(tab)
#F=0.082
#p-wartosc=0.92 > alfa

#b

#estymatory punktowe
mi = mean(tab) #71.75
theta_i = apply(tab, 1, mean) - mi
mi_i = mi + theta_i
#mi_i == apply(tab1, 1, mean)


#estymatory przedziałowe
#qt? dopytać się Eweliny
yi. = apply(tab, 1, mean)
yj. = apply(tab, 2, mean)
alfa = 0.05/2
N_2 = prod(dim(tab))-a
sq = sqrt((SSE(tab)/(a*b))/ncol(tab))

#c)

#ANOVA przed usunięciem danych
#H0: Substancja nie ma wpływu na wytrzymałość materiału. theta1 = theta2=0
#HA: Substancja ma wpływ na wytrzymałość materiały. theta1=/0 dla przynajmniej jednego materiału

#alfa=0.05
statF(tab)
#F=0.082
#p-wartosc=0.92 > alfa

tab[2, 3]=NA

ENAN <- function(dane, nrow, ncol){
  n_row <- length(dane[nrow, ])
  n_col <- length(dane[ ,ncol])
  sum_row <- apply(dane[nrow, ], 1, sum,na.rm=TRUE)
  sum_col <- apply(dane[ ,ncol, drop=F], 2, sum, na.rm=TRUE)
  all <- sum(dane, na.rm=TRUE)
  E <- ((n_row*sum_row) + (n_col*sum_col - all))/((n_row-1)*(n_col-1))
  return(E)
}

ENAN(tab,2,3)
tab[2,3]=80
statF(tab)
#F=0.066
#p=0.93>0.05

#d
tab[2,3]=NA
tab[4,4]=NA

#di
tab
ENAN(tab, 2, 3)
tab[2,3] = 86
tab
ENAN(tab, 4, 4)
tab[4,4] = 78
tab[2,3] = NA
tab
ENAN(tab, 2, 3)
tab[2,3] = 80
tab[4,4] = NA
tab
ENAN(tab, 4, 4)
tab[4,4] = 78
tab[2,3] = NA
tab
ENAN(tab, 2, 3)
tab[2,3] = 80
tab[4,4] = NA
tab
ENAN(tab, 4, 4)
tab[4,4] = 78

#dii
tab[2,3] = 80
tab[4,4] = 78
statF(tab)
#F=0.088
#p=0.916>0.05

#zad.2
tm1 <- c(13, 22, 18, 39)
tm2 <- c(16, 24, 17, 44)
tm3 <- c(5, 4, 1, 22)
tabela2 = rbind(tm1, tm2, tm3)
tab2 = as.matrix(tabela2)
colnames(tab2) = c('1', '2', '3', '4')
tab3 = as.data.frame(tabela2)

#a)
mean(tab2) #18.75

apply(tab2, 1, summary)
apply(tab2, 2, summary)

apply(tab2, 1, shapiro.test) 
apply(tab2, 2, shapiro.test)

#H0: var1 = var2
#HA: var1 =/ var2 (nie są sobie równe)

apply(tab2, 1, var)
apply(tab2, 2, var)


v1 <- var.test(tm1, tm2) #p=0.8219
v2 <- var.test(tm1, tm3) #p=0.7824
v3 <- var.test(tm2, tm3) #p=0.618

#H0: Substancja nie ma wpływu na wytrzymałość materiału. theta1 = theta2=0
#HA: Substancja ma wpływ na wytrzymałość materiały. theta1=/0 dla przynajmniej jednego materiału

#alfa=0.05
statF(tab2)
#F=0.635
#p-wartosc=0.44 > alfa

#b)

#krok0
apply(tab2, 2, mean)
rank(apply(tab2, 2, mean))
ms=MS(tab2)$'MS2'
sqrt(ms)

#R4vs1
mean(tab3$V4)-mean(tab3$V1) #23.66 #max-min
sqrt(ms)*qtukey(0.95, 4, 9) #103.86

#R4vs3
mean(tab3$V4)-mean(tab3$V3) #23

#R4vs2
mean(tab3$V4)-mean(tab3$V2) #18.33

#R2vs1
mean(tab3$V2)-mean(tab3$V1) #5.33
sqrt(ms)*qtukey(0.95, 3, 9) #92.89

#R2vs3
mean(tab3$V2)-mean(tab3$V3) #4.66 #max-min


#R3vs1
mean(tab3$V3)-mean(tab3$V1) #0.66
sqrt(ms)*qtukey(0.95, 2, 9) #75.26

###########
standard.error <- function(x) { 
  sd(x)/sqrt(length(x))
}
s = sqrt(standard.error(tab2))
r4 = qtukey(0.95, 4, 9)
r3 = qtukey(0.95, 3, 9)
r2 = qtukey(0.95, 2, 9)

R4 = s*r4 #8.55
R3 = s*r3 #7.65
R2 = s*r2 #6.20

apply(tab2, 2, mean)
rank(apply(tab2, 2, mean))

#4vs1
mean(tab3$V4)-mean(tab3$V1) #23.66>R4
#4vs3
mean(tab3$V4)-mean(tab3$V3) #23>R3
#4vs2
mean(tab3$V4)-mean(tab3$V2) #18.33>R2
#2vs1
mean(tab3$V2)-mean(tab3$V1) #5.33<R3
#2vs3
mean(tab3$V2)-mean(tab3$V3) #4.66<R2
#3vs1
mean(tab3$V3)-mean(tab3$V1) #0.66<R2

#Istnieją znaczące różnice między wszystkimi
#parami oprócz (2vs1, 2vs3, 3vs1)