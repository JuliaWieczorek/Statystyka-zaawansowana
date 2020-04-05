diff <- function(samp) {
  s = sort(samp)
  l = length(samp)
  d = 1:(l-1)
  for(k in 2:l){
    d[k-1] = s[k] - s[k-1]
  }
  return(d)
}

aly <- function(samp) {
  stat = 0
  l=length(samp)
  for(k in l:1)
    stat=stat+k*(l+1-k)*samp[k]
  return(stat)
  
}

vartest <- function(samp1, samp2, NMonte){
  d1=diff(samp1)
  d2=diff(samp2)
  l=length(d1)
  stat0=aly(d1)
  pd=d1
  cnt=0
  for(j in 1:NMonte){
    r=rbinom(l,1,.5)
    for(k in 1:l){
      pd[k]=ifelse(r[k], d1[k], d2[k])
    }
    if(aly(pd)>stat0) cnt=cnt+1
  }
  return(cnt/NMonte)
}

x1=c(129,123,126,128.5,121)
x2=c(153,154,155,156,158)
vartest(x1,x2,1600)