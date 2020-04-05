x <- c(2,4,6,8)
y<-c(5,7,8,9,10)
t<-sum(x,y)
h<- c(rep(0,length(x)), rep(1, length(y)))
w<-sample(z,4)
t1<- sum(w)
#sparawdzamy czy t1>w, więc funkcja indykatorowa będzie miała wartość 1

w<- sample(z,4)
t2<- sum(w)
#znowu sprawdzamy