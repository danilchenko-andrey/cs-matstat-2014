#' # Семинар 5. Доверительные интервалы, критерии хи-квадрат и Колмогорова-Смирнова
#' ## Задание 1. Доверительный интервал для среднего и разности средних нормального распределения
#' ### Часть 1
n <- 30
sample <- rnorm(n)

alpha1 <- 0.95
t.test(sample, conf.level=alpha1)$conf.int

#' Увеличим $\alpha$
alpha2 <- 0.999
t.test(sample, conf.level=alpha2)$conf.int
#' получился более широкий доверительный интервал.
#' 

#' Увеличим стандартное отклонение
sample2 <- rnorm(n, 0, 100)
alpha2 <- 0.95
t.test(sample2, conf.level=alpha2)$conf.int
#' снова получился более широкий доверительный интервал.
#' 

#' Увеличим размер выборки
n2 <- 1000
sample3 <- rnorm(n2)
alpha2 <- 0.95
t.test(sample3, conf.level=alpha2)$conf.int
#' Доверительный интервал сузился — чего и следовало ожидать.
#' 

#' Смоделируем выборку 1000 раз, посчитаем доверительный интервал среднего
generate.means <- function(k, n, m=0, sd=1) {
  sapply(1:k, function(k) {
    mean(rnorm(n, m, sd))
  })
}

sample.of.means <- generate.means(1000, 30)
hist(sample.of.means)
quantile(sample.of.means, c(0.025, 0.975))

#' ### Часть 2
n <- 30
sample1 <- rnorm(n, 1, 1)
sample2 <- rnorm(n, 2, 1)
t.test(sample2, sample1, conf.level=0.95)$conf.int
#' Доверительный интервал не включает 0 -> принимаем $H_1$.
#' 

#' Теперь увеличим размер выборки...
n <- 1000
sample1 <- rnorm(n, 1, 1)
sample2 <- rnorm(n, 2, 1)
t.test(sample2, sample1, conf.level=0.95)$conf.int
#' снова принимаем $H_1$, но доверительный интервал на этот раз уже.
#' 


#' ## Задание 2. Критерий Колмогорова—Смирнова
n <- 50
sample1 <- rnorm(n, 1, 1)
sample2 <- rnorm(n, 2, 1)

plot(ecdf(sample1), ylim=c(0,1), main='Empirical cumulative distributions')
plot(ecdf(sample2), ylim=c(0,1), col=2, add=T)
legend('bottomright', c('norm', 'exp'), col=c(1,2), pch=16)

ks.test(sample1, sample2)
ks.test(sample1, sample2, alternative='g')
ks.test(sample1, sample2, alternative='l')
#' Полученные p-value вполне адекватны — p-value двусторонней гипотезы в два раза превышает p-value односторонней

sample1 <- rnorm(n, 1, 2)
sample2 <- rnorm(n, 1, 1)

plot(ecdf(sample1), ylim=c(0,1), main='Empirical cumulative distributions')
plot(ecdf(sample2), ylim=c(0,1), col=2, add=T)
legend('bottomright', c('norm', 'exp'), col=c(1,2), pch=16)


ks.test(sample1, sample2)
ks.test(sample1, sample2, alternative='g')
ks.test(sample1, sample2, alternative='l')
#' p-value альтернатив слишком малы — мы стремимся принять обе односторонние гипотезы — это странно...

n1 <- 20
n2 <- 10
sample1 <- append(rnorm(n1, 1, 1), runif(n2, 0, 2))
sample2 <- append(rnorm(n1, 2, 1), runif(n2, 0, 2))

plot(ecdf(sample1), ylim=c(0,1), main='Empirical cumulative distributions')
plot(ecdf(sample2), ylim=c(0,1), col=2, add=T)
legend('bottomright', c('norm', 'exp'), col=c(1,2), pch=16)


ks.test(sample1, sample2)
ks.test(sample1, sample2, alternative='g')
ks.test(sample1, sample2, alternative='l')
#' На этот раз нормальные p-value

sample1 <- rnorm(n, 1)
sample2 <- rexp(n)
plot(ecdf(sample1), ylim=c(0,1), main='Empirical cumulative distributions')
plot(ecdf(sample2), ylim=c(0,1), col=2, add=T)
legend('bottomright', c('norm', 'exp'), col=c(1,2), pch=16)

ks.test(sample1, sample2)
ks.test(sample1, sample2, alternative='g')
ks.test(sample1, sample2, alternative='l')
#' Хм... кажется, снова нечестно — стремимся принять обе односторонние гипотезы

sample1 <- rnorm(n)
sample2 <- rexp(n)
plot(ecdf(sample1), ylim=c(0,1), main='Empirical cumulative distributions')
plot(ecdf(sample2), ylim=c(0,1), col=2, add=T)
legend('bottomright', c('norm', 'exp'), col=c(1,2), pch=16)

ks.test(sample1, sample2)
ks.test(sample1, sample2, alternative='g')
ks.test(sample1, sample2, alternative='l')
#' Теперь все ок.
#' 
#' Перед применением критерия стоит смотреть на распределения!

#' ## Задание 3. Критерий хи-квадрат
#' Построим хи-квадрат для одной выборки
sample <- rnorm(5000, 0, 100)
h1 <- hist(sample)
breaks <- h1$breaks
counts <- h1$counts
sum(counts < 5)

p.j <- diff(pnorm(breaks, 0, 100))

n.j <- sum(counts)
chi.sq <- sum((counts - n.j*p.j)**2 / (n.j*p.j))
#' Получили p-value `r 1 - pchisq(chi.sq, length(counts))`.
#' 


#' Смоделируем выборку 1000 раз
n <- 5000
k <- 1000
ps <- sapply(1:k, FUN=function(k) {
  sample <- rnorm(n, 0, 100)
  h1 <- hist(sample, plot=F)
  breaks <- h1$breaks
  counts <- h1$counts
  p.j <- diff(pnorm(breaks, 0, 100))
  
  while ((sum(counts < 5) > 0 || sum(n*p.j) > 0) && length(counts) > 5) {
    nc <- length(counts)
    counts <- c(counts[1] + counts[2], counts[3:(nc-2)], counts[nc-1]+counts[nc])
    breaks <- breaks[c(-2, -nc)]
    p.j <- diff(pnorm(breaks, 0, 100))
  }
  
  if (sum(n*p.j) > 0) {
    warning('Has bins with less than 5 predicted count')
  }
  chi.sq <- sum((counts - n*p.j)**2 / (n*p.j))
  1 - pchisq(chi.sq, length(counts))
})
#' Мощность критерия: `r sum(ps>0.05)/k`.
hist(ps, main='Histogram of p-values')
abline(v=0.05, col=2, lty=5, lwd=3)
