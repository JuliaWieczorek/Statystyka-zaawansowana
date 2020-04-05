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
#boxplot(resamples100)

r.mean <- sapply(resamples100, mean)
r.mean
mean(r.mean)

#W oparciu o próby bootstrapowe wylosowane w poprzednim kroku oszacuj błąd standardowy i obcążenia dla estymatora średniej z próby.
standard.error <- function(x) { #błąd std. wystarczy sd()
  sd(x)/sqrt(length(x))
}

mi <- mean(resamples100)
e.mean <- sapply(resamaples100, mean) #estymatory średnich
se <- standard.error(e.mean) #błąd std.
bias <- mean(e.mean) - mi #obciążenie estyamtora średniej


#Powtórz zadania z punktów (d) i (e) dla 200, 500, 1000 i 10000 prób bootsrapowych. 
#Czy precyzja oszacowania zależy od ilości prób bootstrapowych?
  
bootstrap <- function(X, n){ #X- wektor danych, n- liczba powtórzeń bootstrapowych
  mi <- mean(X)
  boot <- lapply(1:n, function(i) sample(X, replace = T))
  e.mean <- sapply(boot, mean)
  se <- standard.error(e.mean)
  bias <- mean(e.mean) - mi 
  structure(list("SE"=se, "obciążenie estymatora średniej"=bias))
}

bootstrap(x, 200)
bootstrap(x, 500)
bootstrap(x, 1000)
bootstrap(x, 10000)

#Oszacuj precyzję oszacowania mediany z próby w zależności od ilości prób bootstrapowych.

bootstrap_ES <- function(X, n, ES){ #X- wektor danych, n- liczba powtórzeń bootstrapowych, ES- estymator funkcji
  mi <- ES(X)
  boot <- lapply(1:n, function(i) sample(X, replace = T))
  e <- sapply(boot, ES)
  se <- standard.error(e)
  bias <- mean(e) - mi 
  structure(list("SE"=se, "obciążenie estymatora mediany"=bias))
}

bootstrap_ES(x, 200, median)
bootstrap_ES(x, 500, median)
bootstrap_ES(x, 1000, median)
bootstrap_ES(x, 10000, median)
