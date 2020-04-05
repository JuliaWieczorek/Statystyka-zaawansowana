#zad.1 W celu porównania dwóch różnych metod nauczania 
#(klasycznej i innowacyjnej) 10-ciu losowo wybranych 
#uczniów było najpierw nauczanych metodą klasyczną,
#a następnie podobny materiał był im przekazywany 
#metodą innowacyjną. Otrzymali oni następujące wyniki
#punktowe podczas testów:

k <- c(65, 79, 90, 75, 61, 85, 98, 80, 97, 75)
n <- c(90, 98, 73, 79, 84, 81, 98, 90, 83, 88)

#Czy na poziomie istotno±ci 0.05 możemy twierdzić, 
#że zastosowane metody są porównywalne?



#H0: Zastosowane metody są porównywalne.
#HA: Zastosowane metody nie są porównywalne.

sprowane <- function(x, y){
  diff <- x-y
  N<-500 #permutacja
  sumorig<-sum(diff)
  n<-length(diff)
  stat<-numeric(n)
  i=0
  for(i in 1:N){
    for(j in 1:n){
      stat[j]<-ifelse(runif(i) < 0.5, diff[j], -diff[j])
    }
    if(sum(stat)>=sumorig)
      i=i+1
  }
  i/N #p-value
}
sparowane(k,n)

#p-value = 1.002 > alfa = 0.05 zatem nie mamy podstaw do odrzucenia H0.


#zad. 2 Pewien kierowca twierdzi, że z każdym rokiem 
#jego auto zużywa więcej paliwa. Porównywaa on liczbę
#przejechanych kilometrów po zatankowaniu 10 litrów 
#paliwa teraz i przed dwoma laty.

r2015 <- c(141.85, 134.36, 131.87, 137.28, 122.72, 136.44, 128.96, 136.86)
r2017 <- c(125.10, 122.00, 123.10, 119.92, 124.11, 121.91, 122.00, 135.95, 127.10, 125.10)

#Zwerykowa¢ czy jego przypuszczenie jest słuszne.

#zad.3 Rozważy¢ powyższe zadanie w sytuacji gdyby 
#kierowca twierdził, że po dwóch latach użytkowania 
#auta na tej samej ilości paliwa przejeżd»ż o około 10%
#kilometrów mniej.

#zad. 4 Wygeneruj próbe 100 par obserwacji z dwuwymiarowego
#rozkładu normalnego ze średnią μ = (0, 0) 
#i macierzą kowariancji Σ = I. Wyznacz 95% bootstrapowe 
#przedziaay ufności dla współczynnikóW korelacji.