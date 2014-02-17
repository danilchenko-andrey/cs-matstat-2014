ns <- c(10, 50, 100, 1000)
generate <- function(n, k) {
  res <- list(rep(NA, n), k)
  for (i in seq(1,k)) {
    res[i] <- list(rnorm(n))
  }
  res
}

data10 <- generate(10, 1000)
data50 <- generate(50, 1000)
data100 <- generate(100, 1000)
data1000 <- generate(1000, 1000)
data <- list(data10, data50, data100, data1000)

hist(sapply(FUN=mean, data10))
hist(sapply(FUN=mean, data50))
hist(sapply(FUN=mean, data100))
hist(sapply(FUN=mean, data1000))

boxplot(data50, data100)