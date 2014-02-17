ns <- c(10, 50, 100, 1000)

generate.means <- function(n, k, mean=1, sd=0.5) {
  sapply(1:k, FUN = function(k, n) {mean(rnorm(n, mean=mean, sd=sd))}, n=n)
}

data <- cbind('10'=generate.means(10, 1000), 
              '50'=generate.means(50, 1000), 
              '100'=generate.means(100, 1000),
              '1000'=generate.means(1000, 1000))
boxplot(data)
# видно, что медианы одинаковые, а межквартильный размах и дисперсия) уменьшается с увеличением объема

print.stat <- function(data, need.hist=F) {
  h<-hist(data)
  lines(h$counts~h$mids, col=2)
  rug(data)
  print(paste('max-min:', max(data)-min(data)))
  print(paste('mean:', mean(data)))
  print(paste('sd:', sd(data)))
  print(paste('Cv:', var.coef(data), '%'))
  if (need.hist) {
    h
  }
}

# подробнее:
print.stat(data[,'10'])
print.stat(data[,'50'])
print.stat(data[,'100'])
print.stat(data[,'1000'])
# видно, что уменьшился размах, стандартное отклонение и коэффициент вариации

# N.B. если среднее близко к 0, то Cv может быть странный
# можно запустить generate.mean c mean=0, sd=1