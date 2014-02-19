#========================
# task 1
beginMoment <- function(data, r) {
  mean(data**r)
}

centralMoment <- function(data, r) {
  mean((data-mean(data))**r)
}

# not corrected variance
variance <- function(data) {centralMoment(data,2) }

# this function will generate k samples from normal distribution and calculate f1 and f2 functions for each sample
generate.f2 <- function(n, k, f1, f2, mean=1, sd=0.5) {
  sapply(1:k, FUN = function(k, n) {cbind(f1(rnorm(n, mean=mean, sd=sd)), f2(rnorm(n, mean=mean, sd=sd)))}, n=n)
}

# let's generate corrected and not corrected variances for samples from N(1, 0.5) with sample sizes 10, 30 and 1000
data.10 <- generate.f2(10, 1000, var, variance)
data.30 <- generate.f2(30, 1000, var, variance)
data.1000 <- generate.f2(1000, 1000, var, variance)

# let's see how sample size affects variance
op <- par(mfrow=c(1,2))
max.y <- max(data.10)

boxplot(cbind('not corrected'=data.10[2,], 'corrected'=data.10[1,]), ylim=c(0,max.y), main='N=10')
abline(h=mean(data.10[1,]), col='red')
abline(h=mean(data.10[2,]), col='blue')

boxplot(cbind('not corrected'=data.30[2,], 'corrected'=data.30[1,]), ylim=c(0,max.y), main='N=30')
abline(h=mean(data.30[1,]), col='red')
abline(h=mean(data.30[2,]), col='blue')
par(op)

boxplot(cbind('not corrected'=data.1000[2,], 'corrected'=data.1000[1,]), main='N=1000')
abline(h=mean(data.1000[1,]), col='red')
abline(h=mean(data.1000[2,]), col='blue')

# let's plot densities and see that our estimated params goes better as sample size increases
plot.dnorms <- function(data, n) {
  data.c <- data[1,]
  data.nc <- data[2,]
  x <- seq(0, 2, 0.01)
  plot(dnorm(x, mean=1, sd=0.5)~x, type='l', main=paste('N =', n))
  lines(dnorm(x, mean=1, sd=sqrt(mean(data.c)))~x, col='red')
  lines(dnorm(x, mean=1, sd=sqrt(mean(data.nc)))~x, col='blue')
}  

op <- par(mfrow=c(1,2))
plot.dnorms(data.10, '10')
plot.dnorms(data.30, '30')
par(op)
plot.dnorms(data.1000, '1000')

#========================
# task 2

data.10 <- generate.f2(10, 1000, mean, median)
data.30 <- generate.f2(30, 1000, mean, median)
data.1000 <- generate.f2(1000, 1000, mean, median)

plot.boxplots <- function(data, n) {
  data.mean <- data[1,]
  data.median <- data[2,]
  boxplot(cbind('mean'=data.mean, 'median'=data.median), main=paste('N =', n))
  abline(h=mean(data.mean), col='red')
  abline(h=mean(data.median), col='blue')
}
op <- par(mfrow=c(1,2))
plot.boxplots(data.10, '10')
plot.boxplots(data.30, '30')
par(op)
plot.boxplots(data.1000, '1000')
# and so we see smaller range for mean than for medians


#========================
# task 3

data.1 <- rnorm(1000, mean=1, sd=0.5)
data.2 <- rnorm(10, mean=100, sd=10)
data <- append(data.1, data.2)
mean(data)
median(data)
mean(data, trim=0.01)
mean(data, trim=0.1)
# so we see that mean is not doing well with outliers
# median does better
# and trimmed means are pretty good

# let's see distibution of statistics got from same sample sizes
generate.funs <- function(n1, n2, k=1000, mean1=1, mean2=50, sd1=0.5, sd2=5) {
  data <- sapply(1:k, FUN=function(n) {
    data <- append(rnorm(n1, mean=mean1, sd=0.5), rnorm(n2, mean=100, sd=10))
    cbind(mean(data), median(data), mean(data, trim=0.01), mean(data, trim=0.1))
  })
  data <- as.data.frame(t(data))
  names(data) <- c('mean','median', 'mean-0.01', 'mean-0.1')
  data
}
data <- generate.funs(1000, 10, 100)
boxplot(data)
# and we see that again mean is going to do worse than median and trimmed means