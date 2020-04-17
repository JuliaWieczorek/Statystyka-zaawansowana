##### Zadanie 1
proba<-rnorm(20)
proba

##### Zadanie 2 i 3
# boostrap i jackknife dla 20 powtórzeń
boot20<-numeric(length(proba))
for (i in 1:20) boot20[i]<-mean(sample(proba, 20, replace=T))
mean(boot20)
obc_s=abs(mean(proba)-mean(boot20))
obc_s
blad_s=sd(boot20)/sqrt(length(boot20))
blad_s


#jackknife
jack<-c()
suma<-c()
for (i in 1:length(proba)){
  jack[i]<-mean(proba[-i])
}
for (i in 1:length(jack)){
  suma[i]=(jack[i]-mean(jack))^2
}

obc_j_s2<-((length(jack)-1)*(mean(jack)-mean(proba)))
obc_j_s2

var_j=(length(jack)-1)/length(jack)*sum(suma)
var_j
blad_j=sqrt(var_j)/sqrt(length(jack))
blad_j


##### Zadanie 4
#bootstrap
srednie<-c()
boot_100<-numeric(length(20))
blad_100<-c()

for (i in 1:100){
  proba_100<-rnorm(20)
  for (j in 1:20){
    boot_100[j]<-mean(sample(proba_100, 20, replace=T))
  }
  obc_100=abs(mean(proba_100)-mean(boot_100))
  srednie[i]<-mean(obc_100)
  blad_100[i]=sd(boot_100)/sqrt(length(boot_100))
  
}
srednie
mean(srednie)
blad_100
mean(blad_100)

#jackknife
srednie_j<-c()
sumy<-c()
blad_100_j<-c()

for (i in 1:100){
  proba_100_j<-rnorm(20)
  jack_100<-c()
  for (j in 1:20){
    jack_100[j]<-mean(proba_100_j[-j]) #wektor średnich
  }
  for (k in 1:length(jack_100)){
    sumy[k]=(jack_100[k]-mean(jack_100))^2
  }
  srednie_j[i]<-((length(jack_100)-1)*(mean(jack_100)-mean(proba_100)))
  var_100=(length(jack_100)-1)/length(jack_100)*sum(sumy)
  blad_100_j[i]=sqrt(var_100)/sqrt(length(jack_100))
}
srednie_j
mean(srednie_j)
blad_100_j
mean(blad_100_j)


##### Zadanie 5
#dla rozkładu poissona(rpois) oraz chi-kwadrat (rchisq)
#POISSON

##### Zadanie 1
proba<-rpois(20,2)
proba

##### Zadanie 2 i 3
# boostrap i jackknife dla 20 powtórzeń
boot20<-numeric(length(proba))
for (i in 1:20) boot20[i]<-mean(sample(proba, 20, replace=T))
mean(boot20)
obc_s=abs(mean(proba)-mean(boot20))
obc_s
blad_s=sd(boot20)/sqrt(length(boot20))
blad_s


#jackknife
jack<-c()
suma<-c()
for (i in 1:length(proba)){
  jack[i]<-mean(proba[-i])
}
for (i in 1:length(jack)){
  suma[i]=(jack[i]-mean(jack))^2
}

obc_j_s2<-((length(jack)-1)*(mean(jack)-mean(proba)))
obc_j_s2

var_j=(length(jack)-1)/length(jack)*sum(suma)
var_j
blad_j=sqrt(var_j)/sqrt(length(jack))
blad_j


##### Zadanie 4
#bootstrap
srednie<-c()
boot_100<-numeric(length(20))
blad_100<-c()

for (i in 1:100){
  proba_100<-rpois(20, 2)
  for (j in 1:20){
    boot_100[j]<-mean(sample(proba_100, 20, replace=T))
  }
  obc_100=abs(mean(proba_100)-mean(boot_100))
  srednie[i]<-mean(obc_100)
  blad_100[i]=sd(boot_100)/sqrt(length(boot_100))
  
}
srednie
mean(srednie)
blad_100
mean(blad_100)

#jackknife
srednie_j<-c()
sumy<-c()
blad_100_j<-c()

for (i in 1:100){
  proba_100_j<-rpois(20, 2)
  jack_100<-c()
  for (j in 1:20){
    jack_100[j]<-mean(proba_100_j[-j]) #wektor średnich
  }
  for (k in 1:length(jack_100)){
    sumy[k]=(jack_100[k]-mean(jack_100))^2
  }
  srednie_j[i]<-((length(jack_100)-1)*(mean(jack_100)-mean(proba_100)))
  var_100=(length(jack_100)-1)/length(jack_100)*sum(sumy)
  blad_100_j[i]=sqrt(var_100)/sqrt(length(jack_100))
}
srednie_j
mean(srednie_j)
blad_100_j
mean(blad_100_j)

#### CHI_KWADRAT
##### Zadanie 1
proba<-rchisq(20, 2)
proba

##### Zadanie 2 i 3
# boostrap i jackknife dla 20 powtórzeń
boot20<-numeric(length(proba))
for (i in 1:20) boot20[i]<-mean(sample(proba, 20, replace=T))
mean(boot20)
obc_s=abs(mean(proba)-mean(boot20))
obc_s
blad_s=sd(boot20)/sqrt(length(boot20))
blad_s


#jackknife
jack<-c()
suma<-c()
for (i in 1:length(proba)){
  jack[i]<-mean(proba[-i])
}
for (i in 1:length(jack)){
  suma[i]=(jack[i]-mean(jack))^2
}

obc_j_s2<-((length(jack)-1)*(mean(jack)-mean(proba)))
obc_j_s2

var_j=(length(jack)-1)/length(jack)*sum(suma)
var_j
blad_j=sqrt(var_j)/sqrt(length(jack))
blad_j


##### Zadanie 4
#bootstrap
srednie<-c()
boot_100<-numeric(length(20))
blad_100<-c()

for (i in 1:100){
  proba_100<-rchisq(20, 2)
  for (j in 1:20){
    boot_100[j]<-mean(sample(proba_100, 20, replace=T))
  }
  obc_100=abs(mean(proba_100)-mean(boot_100))
  srednie[i]<-mean(obc_100)
  blad_100[i]=sd(boot_100)/sqrt(length(boot_100))
  
}
srednie
mean(srednie)
blad_100
mean(blad_100)

#jackknife
srednie_j<-c()
sumy<-c()
blad_100_j<-c()

for (i in 1:100){
  proba_100_j<-rchisq(20, 2)
  jack_100<-c()
  for (j in 1:20){
    jack_100[j]<-mean(proba_100_j[-j]) #wektor średnich
  }
  for (k in 1:length(jack_100)){
    sumy[k]=(jack_100[k]-mean(jack_100))^2
  }
  srednie_j[i]<-((length(jack_100)-1)*(mean(jack_100)-mean(proba_100)))
  var_100=(length(jack_100)-1)/length(jack_100)*sum(sumy)
  blad_100_j[i]=sqrt(var_100)/sqrt(length(jack_100))
}
srednie_j
mean(srednie_j)
blad_100_j
mean(blad_100_j)