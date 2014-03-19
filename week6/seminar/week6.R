#' # Семинар 6. Критерии согласия
#' ## Задание 1. Критерий Колмогорова-Смирнова для проверки распределения с теоретическим
#' 

n <- 30
k <- 1000
#' Смоделируем выборку из нормального распределения `r k` раз, посчитаем критерий 
#' Колмогорова-Смирнова против нормального распределения с исходными параметрами и
#' против нормального распределения с эспирическими параметрами
sapply(1:k, function(k) {
  sample <- rnorm(n, 2, 1)
  k.t <- ks.test(sample, 'pnorm', 2, 1)
  k.e <- ks.test(sample, 'pnorm', mean(sample), sd(sample))  
  cbind(k.t$p.value, k.e$p.value, k.t$statistic, k.e$statistic)
}) -> ps

#' $\alpha$ для теоретических параметров 
sum(ps[1,] < 0.05) / k
#' $\alpha$ для эмпирических параметров
sum(ps[2,] < 0.05) / k

#' средняя разница между p-value 
mean(ps[1,] - ps[2,])

#' Посмотрим на статистики в обоих случаях
hist(ps[4,], col=rgb(1,0,0, 0.3), main='Histogram of KS statistic', xlab='D')
hist(ps[3,], col=rgb(0,1,0, 0.3), add=T)
legend('topright', c('theoretical', 'empirical'), col=c('green', 'red'), pch=15)

#' Видно, что для теоретических параметров значение D завышено 
#' 

#' ## Задание 2. Проверка распределения на нормальность. Сравнение критериев
#' 
require(nortest)

generate.tests <- function(n, k) {
  sapply(1:k, function(k) {
    a <- 2
    b <- 5
    sample <- runif(n, a, b)
    shapiro <- shapiro.test(sample)$p.value
    ks.e <- ks.test(sample, 'pnorm', mean(sample), sd(sample))$p.value
    ks.t <- ks.test(sample, 'pnorm', (b+a)/2, sqrt((b-a)^2/12))$p.value
    ad <- ad.test(sample)$p.value
    l <- lillie.test(sample)$p.value
    cbind(shapiro, ks.e, ks.t, ad, l)
  })
}

#' Смоделируем 1000 раз выборку из равномерного распределения, измерим p-value разных тестов
n <- 30
k <- 1000
ps <- generate.tests(n, k)

#' Измерим мощности
powers <- sapply(1:nrow(ps), function(n) {
  sum(ps[n,] < 0.05) / k  
})
test.names <- c('Shapiro', 'KS e', 'KS t', 'AD', 'Lilliefors')
powers.df <- data.frame(name = test.names, value=powers)
powers.df
#' На графике
plot(powers.df, type='h', xlab='Test', ylab='Power', main='Power of tests')

#' Посчитаем мощности для разных размеров выборки
ns <- seq(50, 500, 50) 
sapply(ns, function(n) {
  ps <- generate.tests(n, k)
  sapply(1:nrow(ps), function(n) {
    sum(ps[n,] < 0.05) / k  
  })  
}) -> ps.n

plot(ps.n[1,]~ns, type='l', ylim=c(0,1), ylab='Power', lwd=2, main='Power for runif')
lines(ps.n[2,]~ns, col=2, lwd=2)
lines(ps.n[3,]~ns, col=3, lwd=2)
lines(ps.n[4,]~ns, col=4, lwd=2)
lines(ps.n[5,]~ns, col=5, lwd=2)
legend('right', test.names, col=1:6, pch=1)

#' И для экспоненциального распределения
generate.tests <- function(n, k) {
  sapply(1:k, function(k) {
    sample <- rexp(n, 2)
    shapiro <- shapiro.test(sample)$p.value
    ks.e <- ks.test(sample, 'pnorm', mean(sample), sd(sample))$p.value
    ks.t <- ks.test(sample, 'pnorm', 1/2, 1/2)$p.value
    ad <- ad.test(sample)$p.value
    l <- lillie.test(sample)$p.value
    cbind(shapiro, ks.e, ks.t, ad, l)
  })
}

#' Посчитаем мощности для разных размеров выборки
ns <- seq(10, 100, by=10)
sapply(ns, function(n) {
  ps <- generate.tests(n, k)
  sapply(1:nrow(ps), function(n) {
    sum(ps[n,] < 0.05) / k  
  })  
}) -> ps.n

plot(ps.n[1,]~ns, type='l', ylim=c(0,1), ylab='Power', lwd=2, main='Power for rexp')
lines(ps.n[2,]~ns, col=2, lwd=2)
lines(ps.n[3,]~ns, col=3, lwd=2)
lines(ps.n[4,]~ns, col=4, lwd=2)
lines(ps.n[5,]~ns, col=5, lwd=2)
legend('right', test.names, col=1:6, pch=1)


#' # Задание 3. QQ-plot
#' 
n <- 5000
sample <- rnorm(n, 5, 2)
q.e.norm <- sample[order(sample)]
q.t <- sapply((0:(n-1))/n, FUN=qnorm, mean=5, sd=2)
hist(sample)
qqplot(q.e.norm, q.t, xlab='Empirical quantiles', ylab='Theoretical quantiles')
abline(0, 1, col=2)

sample <- rbeta(n, 1, 2)
q.e.beta1 <- sort(sample)
hist(sample)
qqplot(q.e.beta1, q.t, xlab='Empirical quantiles', ylab='Theoretical quantiles')
abline(0, 1, col=2)

sample <- rbeta(n, 2, 1)
q.e.beta2 <- sort(sample)
hist(sample)
qqplot(q.e.beta2, q.t, xlab='Empirical quantiles', ylab='Theoretical quantiles')
abline(0, 1, col=2)

sample <- rgamma(n, 5, 0.1)
q.e.gamma <- sort(sample)
hist(sample)
qqplot(q.e.gamma, q.t, xlab='Empirical quantiles', ylab='Theoretical quantiles')
abline(0, 1, col=2)

sample <- rnorm(n, 0, 2)
q.e.norm2 <- sort(sample)
hist(sample)
qqplot(q.e.norm2, q.t, xlab='Empirical quantiles', ylab='Theoretical quantiles')
abline(0, 1, col=2)

sample <- rnorm(n, 5, 2)
q.e.norm <- sort(scale(sample))
q.t <- sapply((0:(n-1))/n, FUN=qnorm, mean=0, sd=1)

sample <- rbeta(n, 1, 2)
q.e.beta1 <- sort(scale(sample))

sample <- rbeta(n, 2, 1)
q.e.beta2 <- sort(scale(sample))

sample <- rgamma(n, 5, 0.1)
q.e.gamma <- sort(scale(sample))

sample <- rnorm(n, 0, 2)
q.e.norm2 <- sort(scale(sample))

plot(q.e.norm, q.t, type='l', xlab='Empirical quantiles (normalized)', ylab='Theoretical quantiles')
lines(q.e.beta1, q.t, col=2)
lines(q.e.beta2, q.t, col=3)
lines(q.e.gamma, q.t, col=4)
lines(q.e.norm2, q.t, col=5)
legend('bottomright', c('rnorm(5,2)', 'rbeta(1,2)', 'rbeta(2,1)', 'rgamma(5,0.1)', 'rnorm(0,2)'), col=1:5, pch=15)
