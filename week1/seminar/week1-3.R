r80 <- rnorm(80)
r60 <- rnorm(60, mean=20, sd=10)

r <- append(r80, r60)
#h1 <- hist(r80)
#h2 <- hist(r60)

h<-hist(r)
lines(h$counts~h$mids, col=2)
rug(r)

#skew(rnorm(1000))

freq <- h$density
plot(c(0, cumsum(freq))~ seq(min(r), max(r), by=(max(r)-min(r))/(length(freq))), type='b',
     xlab='x', ylab='F*')


var.r <- r[order(r)]
summary(var.r)
rug(h)