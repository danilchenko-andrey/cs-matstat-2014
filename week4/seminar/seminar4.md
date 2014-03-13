Критерии однородности (параметрические и непараметрические)
==========================================================
Задание 1: Парный T-test
------------------------

Сперва смоделируем связанные выборки...

```r
n <- 50
k <- 1000
mus <- c(2, 4)
sds <- c(1, 1)

cor <- matrix(c(1, -0.6, -0.6, 1), nrow = 2)
print(cor)
```

```
##      [,1] [,2]
## [1,]  1.0 -0.6
## [2,] -0.6  1.0
```

```r

generate.paired <- function(n, mus, sds, cor) {
    X0 <- rnorm(n)
    Y0 <- rnorm(n)
    K <- cor * (sds %*% t(sds))
    A <- chol(K)
    
    X <- c()
    Y <- c()
    for (t in 1:n) {
        hh <- t(A) %*% c(X0[t], Y0[t])
        X[t] <- hh[1] + mus[1]
        Y[t] <- hh[2] + mus[2]
    }
    # plot(X, Y, col = 'blue')
    data.frame(X = X, Y = Y)
}
```


Теперь 1000 раз смоделируем выборки и посчитаем парный и независимый тесты Стьюдента:

```r
ps <- sapply(1:k, FUN = function(i) {
    data <- generate.paired(n, mus, sds, cor)
    X <- data$X
    Y <- data$Y
    p.paired <- t.test(X, Y, paired = T, var.equal = T)$p.value
    p.unpaired <- t.test(X, Y, paired = F, var.equal = T)$p.value
    cbind(p.paired, p.unpaired)
})

p.paired <- ps[1, ]
p.unpaired <- ps[2, ]
```


Пример полученных p-value...

```r
head(cbind(p.paired, p.unpaired))
```

```
##       p.paired p.unpaired
## [1,] 5.498e-13  1.214e-20
## [2,] 5.665e-13  2.366e-21
## [3,] 1.196e-09  1.854e-15
## [4,] 4.427e-11  2.272e-18
## [5,] 1.749e-10  6.456e-17
## [6,] 6.767e-10  9.932e-16
```


Итого: 0 раз p-value парного теста было меньше p-value независимого.

Мощность парного теста: 1 (из 1000 раз 0 раз приняли неверную $H_0$).

Мощность независимого теста: 1 (из 1000 раз 0 раз приняли неверную $H_0$).


Задание 2.
----------
??
