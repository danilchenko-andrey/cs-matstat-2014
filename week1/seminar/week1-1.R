n <- 100
data <- rnorm(n)
data1 <- rexp(n)

hist(data, col=rgb(0,1,1,1))
boxplot(data)

beginMoment <- function(data, r) {
  mean(data**r)
}

centralMoment <- function(data, r) {
  mean((data-mean(data))**2)
}

min(data)
max(data)

#размах
max(data) - min(data)

mean(data)
median(data)

#выборочная дисперсия
s2 <- centralMoment(data, 2)
s2
s <- sqrt(s2)
s
#исправленная дисп
var(data)
sd(data)

mu3 <- centralMoment(data, 3)
#skew
mu3 / s**3
skew <- function(data) {
  s2 <- centralMoment(data, 2)
  s <- sqrt(s2)
  centralMoment(data, 3) / s**3
}

# kurtosis
mu4 <- centralMoment(data, 4)
(mu4 / s**4) - 3

#коэф. вариации
s/mean(data)d