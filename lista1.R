#zadanie 4.
#(a) Zwizualizuj dane na wykresie.
x = c(138.5, 138.5, 140.0, 141.0, 141.0, 143.5, 145.0, 147.0, 148.5, 150.0, 153.0, 154.0,   155.0, 156.5, 157.0, 158.5, 159.0, 159.0, 159.0, 160.5, 161.0, 162.0)
plot(x)
hist(x)

#Czy wiemy z jakiego rozkładu pochodzą te dane? Czy na poziomie istotności 0.05 możemy twierdzić, że dane pochodzą z rozkładu normalnego?
shapiro.test(x) # dane nie pochodzą z r. norm.

#Wylosuj 5 prób bootstrapowych z danej próby, co możemy powiedzieć o ich rozkładzie?
resamples5 <- lapply(1:5, function(i) sample(x, replace = T))
#resamples5
for (i in resamples5) {print(shapiro.test(i))}
boxplot(resamples5)

#W oparciu o 100 prób bootstrapowych oszacuj średni wzrost.
resamples100 <- lapply(1:100, function(i) sample(x, replace = T))
boxplot(resamples100)

r.mean <- sapply(resamples100, mean)
r.mean

#W oparciu o próby bootstrapowe wylosowane w poprzednim kroku oszacuj błąd standardowy i obcążenia dla estymatora średniej z próby.
sqrt(var(r.mean)) #błąd std[1]

varnieob <- var(x)
n <- length(x)
varob <- varnieob*((n-1)/n)
varnieob[2]
varob[3]

S2var <- function(X){
  m <- mean(X)
  v <- sum((x-m)^2)/(length(X))
  return(v)
}
rm.var <- sapply(resamples100, S2var)
mean(rm.var) #średnia z obciążenia estymatora średniej z próby[4]


#Powtórz zadania z punktów (d) i (e) dla 200, 500, 1000 i 10000 prób bootsrapowych. Czy precyzja oszacowania zależy od ilości prób bootstrapowych?
  
#Oszacuj precyzję oszacowania mediany z próby w zależności od ilości prób bootstrapowych.


#Sposób z wykładu
n <- length(x)
N <- 5
stat <- numeric(N)
for (i in 1:N){
  a = sample(x, n, replace=T)
  stat[i] = var(a)
}
boxplot(stat)
stripchart(stat)
