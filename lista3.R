#zad.1 
k <- c(65, 79, 90, 75, 61, 85, 98, 80, 97, 75)
n <- c(90, 98, 73, 79, 84, 81, 98, 90, 83, 88)

shapiro.test(k)
shapiro.test(n)

var2=function(X,Y){
  varX=var(X)
  varY=var(Y)
  T=varX/varY
  nX=length(X)
  nY=length(Y)
  p=1-pf(T,nX-1,nY-1,lower.tail=TRUE)+pf(T,nX-1,nY-1,lower.tail=FALSE)
  list(stat=T,p_value=p)
}

var2(k,n)
var.test(k,n)

t.test(k,n, paired=T)

permutation_par <- function(x, y){
  diff <- x-y
  N<-10 
  sumorig<-sum(diff)
  n<-length(diff)
  stat<-numeric(n)
  stat_oryg<-t.test(x,y, paired=T) 
  print (stat_oryg)
  cnt=0
  for(i in 1:N){
    for(j in 1:n){
      stat[j]<-ifelse(t.test(x,y)$p.statistic < 0.5, diff[j], -diff[j])
    }
    if(sum(stat)>=sumorig)
      cnt=cnt+1
  }
  p<-1/N
  return (p)
}
permutation_par(k,n)


#zad. 2 

r2015 <- c(141.85, 134.36, 131.87, 137.28, 122.72, 136.44, 128.96, 136.86)
r2017 <- c(125.10, 122.00, 123.10, 119.92, 124.11, 121.91, 122.00, 135.95, 127.10, 125.10)

shapiro.test(r2015)
shapiro.test(r2017)

m <- length (r2015)
n <- length (r2017)
N <- 400 

test = wilcox.test(r2015, r2017, alternative = 'less')$statistic
test
counter = 0 
z=c(r2015, r2017)
combin = combn((m+ n), m) 

for (i in 2: N){ 
  x1 <-z[combin[,i]]     
  y1 <-z[-combin[,i]]  
  test2 = wilcox.test(x1,y1, alternative = 'less')$statistic
  if( test2 >= test ){ counter = counter+1}  
}

counter/N

#zad.3 
dane <- r2017*0.9
shapiro.test(dane) 
N <- 400 
n <- length(dane)
m <- length(r2017)
suma <- sum(dane)

test_n <- wilcox.test(r2017, dane)$statistic 
cnt <- 0 
z <- c(dane,r2017) 
combin <- combn((m+n),m) 
for (i in 2:N){  
  n_przed1 <- z[combin[,i]] 
  n_po1 <- z[-combin[,i]] 
  n_test1 <- wilcox.test(n_przed1,n_po1)$statistic
  if(n_test1 <= test_n){cnt=cnt+1}
}
c <- cnt/N
c
if(c<=0.5){
  a=(c*2)
  print(a)
}else{
  b=(2-c*2)
  print(b)
}

#zad. 4 
library(mvtnorm)
a<-matrix(rmvnorm(100, mean(0,0)), ncol=2)
n<-50

library(bootstrap)

theta <- function(x, a){
  cor(a[x,1], a[x,2])
}
przedzial <- boott(1:n, theta, a, perc=c(0.05, 0.95))
przedzial[1]