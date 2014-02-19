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

# this function will generate k samples from normal distribution and calculate f function for each sample
generate.f <- function(n, k, f, mean=1, sd=0.5) {
  sapply(1:k, FUN = function(k, n) {f(rnorm(n, mean=mean, sd=sd))}, n=n)
}

# let's generate corrected and not corrected variances for samples from N(1, 0.5) with sample sizes 10, 30 and 1000
data.c <- cbind('10'=generate.f(10, 1000, var),
                '30'=generate.f(30, 1000, var),
                '1000'=generate.f(1000, 1000, var))
data.nc <- cbind('10'=generate.f(10, 1000, variance),
                 '30'=generate.f(30, 1000, variance),
                '1000'=generate.f(1000, 1000, variance))

# let's see how sample size affects variance
op <- par(mfrow=c(1,2))
max.y <- max(c(data.c[,'10'], data.nc[,'10']))

boxplot(cbind('not corrected'=data.nc[,'10'], 'corrected'=data.c[,'10']), ylim=c(0,max.y), main='N=10')
abline(h=mean(data.c[,'10']), col='red')
abline(h=mean(data.nc[,'10']), col='blue')

boxplot(cbind('not corrected'=data.nc[,'30'], 'corrected'=data.c[,'30']), ylim=c(0,max.y), main='N=30')
abline(h=mean(data.c[,'30']), col='red')
abline(h=mean(data.nc[,'30']), col='blue')
par(op)

boxplot(cbind('not corrected'=data.nc[,'1000'],'corrected'=data.c[,'1000']), main='N=1000')
abline(h=mean(data.c[,'1000']), col='red')
abline(h=mean(data.nc[,'1000']), col='blue')

# let's plot densities and see that our estimated params goes better as sample size increases
plot.dnorms <- function(data.c, data.nc, n) {
  x <- seq(0, 2, 0.01)
  plot(dnorm(x, mean=1, sd=0.5)~x, type='l', main=paste('N =', n))
  lines(dnorm(x, mean=1, sd=sqrt(mean(data.c[,n])))~x, col='red')
  lines(dnorm(x, mean=1, sd=sqrt(mean(data.nc[,n])))~x, col='blue')
}  

op <- par(mfrow=c(1,2))
plot.dnorms(data.c, data.nc, '10')
plot.dnorms(data.c, data.nc, '30')
#plot.dnorms(data.c, data.nc, '1000')
par(op)

#========================
# task 2

data.mean <- cbind('10'=generate.f(10, 1000, mean),
                '30'=generate.f(30, 1000, mean),
                '1000'=generate.f(1000, 1000, mean))
data.median <- cbind('10'=generate.f(10, 1000, median),
                 '30'=generate.f(30, 1000, median),
                 '1000'=generate.f(1000, 1000, median))

plot.boxplots <- function(data.mean, data.median, n) {
  boxplot(cbind('mean'=data.mean[,n], 'median'=data.median[,n]), main=paste('N =', n))
  abline(h=mean(data.mean[,n]), col='red')
  abline(h=mean(data.median[,n]), col='blue')
}
op <- par(mfrow=c(1,2))
plot.boxplots(data.mean, data.median, '10')
plot.boxplots(data.mean, data.median, '30')
par(op)
plot.boxplots(data.mean, data.median, '1000')
