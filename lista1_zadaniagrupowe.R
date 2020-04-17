dane=c(138.5,138.5,140,141,141,143.5, 145,147,148.5,150,153,154,155,156.5,157,158.5,159,159,159,160.5,161, 162)
summary(dane)
n=length(dane)
##  A ##

#Wykres
hist(dane, main="Wzrost studentów", ylab="Częstość występowania w danym przedziale",xlab='Wzrost', col="red")

##  B ##
#H0: dane mają rozkład normalny
#H1: dane nie mają rozkładu normalnego
shapiro.test(dane)

# p-value = 0.02798, odrzucamy H0 na rzecz H1

##  C ##
library(boot)
N <- 5
norm=stat=numeric(N) 
for (i in 1:N){
  dane1= sample (dane , n, replace=T)
  stat[i] = var(dane1)
  norm[i]=shapiro.test(dane1)$p.value
}
norm
shapiro.test(dane1)
#p<0,05, dane nie mają rozkładu normalnego
library(boot)
N <- 100
stat  <- numeric(N) 
blad=med=sr=od=stat
for (i in 1:N){
  dane1= sample (dane , n, replace=T)
  stat[i] = var(dane1)
  sr[i] = mean(dane1)
  od[i]=sd(dane1)
  med[i]=median(dane1)
  blad[i]=od[i]/sqrt(n)
  
}
#średnie ze wszystkich powtórzeń:
sr
#średnia na podstawie uśrednionej wartości sr:
srednia=mean(sr)
#odchylenia ze wszystkich powtórzeń:
od
#średnia wartość odchylenia ze wszystkich od
odchylenie=mean(od)
#mediana z każdego powtórzenia:
med
#średnia wartość mediany w każdym powtórzeniu
mediana=median(med)

## D ###
# W oparciu o 100 prób bootstrapowych oszacuj średni wzrost
n = length(Studenci)
N = 100
wzrost=numeric(N)
for (i in 1:N){
  StudBoot = sample(Studenci,n,replace = T)
  wzrost[i]=mean(StudBoot)
}
mean(wzrost)

## E ##
# W oparciu o próby bootstrapowe wylosowane w poprzednim kroku oszacuj błąd standardowy i obciążenia dla estymatora średniej z próby

sd(wzrost)/sqrt(length(Studenci))  # oszacowany błąd standardowy
obciazenie=mean(wzrost)-mean(Studenci)
obciazenie # oszacowane obciążenie

## F ##
# Powtórz zadania z punktów d-e dla 200, 500, 1000 i 10000 prób bootstrapowych. Czy precyzja oszacowania zależy od ilości prób bootstrapowych?

Srednie=cbind(c(mean(Studenci),sd(Studenci),mean(Studenci)-mean(Studenci),sd(wzrost)/sqrt(length(Studenci))))
N = c(100,200,500,1000,10000)
for (j in N){
  wzrost=numeric(j)
  for(i in 1:j){
    StudBoot = sample(Studenci,n,replace = T)
    wzrost[i]=mean(StudBoot)}
  Srednie=cbind(Srednie,c(mean(wzrost),sd(wzrost),mean(wzrost)-mean(Studenci),sd(wzrost)/sqrt(length(Studenci))))
}
colnames(Srednie)=c('Nasza proba',N)
rownames(Srednie)=c('Srednia','Odchylenie','Obciazenie',"Błąd Standardowy")
Srednie

## G ##
j=1
obciazenie=numeric(8)
odchylenie=numeric(8)
os_x=numeric(8)
for (N in c(5,10,15,20,25,30,35,40)){
  stat <- numeric(N)
  for (i in 1:N){
    classB = sample(class, n, replace=T)
    stat[i] = median(classB)
  }
  os_x[j] = N
  obciazenie[j] = abs(median(stat)-median(class))
  odchylenie[j] = sd(stat)
  j=j+1
}
plot(os_x, obciazenie)
plot(os_x, odchylenie)
