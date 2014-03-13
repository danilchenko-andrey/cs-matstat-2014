# Task 1: paired and independed test

cor <- 0.6

K <- matrix(c(1, 0.6, 0.6, 1), ncol=2)

A <- chol(K)

# our function that will generate correlated samples
generate.f2 <- function(n, k, A, diff) {
    XY <- sapply(1:k, FUN = function(k, n) {
      X <- cbind(rnorm(n, 0, 1), rnorm(n, 0, 1))
      Y <- A %*% t(X) + diff
      cbind('X'=Y[1,], 'Y'=Y[2,])
    }, n=n)
}

n <- 50
k <- 1000
XY <- generate.f2(n, k, A, diff=c(2,30))
X <- XY[1:(n/2),]
Y <- XY[(1+(n/2)):n,]

sapply(1:k, FUN=function(i) {
  result <- t.test(X[,i], Y[,i], paired=T)
  result$p.value
}) -> p.paired

# power of paired test
sum(p.paired > 0.05) / k

sapply(1:k, FUN=function(i) {
  result <- t.test(X[,i], Y[,i], paired=F)
  result$p.value
}) -> p.unpaired

# power of unpaired test
sum(p.unpaired > 0.05)/k

# task 2 - ANOVA
#================
n <- 50
# создадим группы, одна из которых смещена
sample1 <- rnorm(n, 0, 1)
sample2 <- rnorm(n, 0, 1)
sample3 <- rnorm(n, 1, 1)
# объединим группы в один датасет
sample <- c(sample1, sample2, sample3)
group <- rep(c('a','b','c'), rep(n,3))
data <- data.frame(sample, factor=group)
# anova
result <- aov(sample~factor,data)
summary(result)
# получили, что P(>F) << 0.05 — значит, что в наших группах есть различия

# создадим новые группы, в которых нет различий
sample1 <- rnorm(n, 0, 1)
sample2 <- rnorm(n, 0, 1)
sample3 <- rnorm(n, 0, 1)
# объединим группы в один датасет
sample <- c(sample1, sample2, sample3)
group <- rep(c('a','b','c'), rep(n,3))
data <- data.frame(sample, factor=group)
# anova
result <- aov(sample~factor,data)
summary(result)
# получили, что P(>F) > 0.05 — значит, что в наших группах нет различий


# task 3 - non-parametric criteries
n1 <- 20
n2 <- 10
k <- 1000
sample1 <- append(rnorm(n1, 4, 2), runif(n2, 2, 4))
sample2 <- append(rnorm(n1, 5, 2), runif(n2, 4, 2))
hist(sample2, col=rgb(1,0,0,0.3), probability=T, ylim=c(0,0.3))
hist(sample1, col=rgb(0,1,0,0.3), probability=T, ylim=c(0,0.3), add=T)
abline(v=mean(sample2), col='red', lty=5)
abline(v=mean(sample1), col='green', lty=5)
abline(v=median(sample2), col='red', lwd=2)
abline(v=median(sample1), col='green', lwd=2)

ps <- sapply(1:k, FUN=function(k) {
  sample1 <- append(rnorm(n1, 4, 2), runif(n2, 2, 4))
  sample2 <- append(rnorm(n1, 5, 2), runif(n2, 3, 7))
  p.w <- wilcox.test(sample1, sample2)$p.value
  p.t <- t.test(sample1, sample2)$p.value
  cbind(p.w, p.t)
})
# power of mann-whitney test
sum(ps[1,] < 0.05)/k
# power of student test
sum(ps[2,] < 0.05)/k

