
aic <- function(model) {
  k <- length(model$coefficients)
  n <- length(model$fitted.values)
  2 * k + n * floor(log(mean(model$residuals^2)) + 1)
}

bic <- function(model) {
  k <- length(model$coefficients)
  n <- length(model$fitted.values)
  2 * k * log(n) + n * floor(log(mean(model$residuals^2)) + 1)
}

op <- par(mfrow=c(2,2))

N <- 300
x1 <- rnorm(N, 5, 5)
x2 <- rnorm(N, 2, 1)
x3 <- runif(N, 2, 4)

e1 <- rnorm(N, 0, 1)

# good data to fit
y <- x1 + 3*x2 + 2 * x3 + e1

model1 <- glm(y ~ x1 + x2 + x3)
summary(model1)

plot(model1)

# aic and bic coefs
aic(model1)
bic(model1)
# and for simpler models
aic(glm(y~x1))
bic(glm(y~x1))

# let's add some outliers
y.out <- y
y.out[5] <- 100
y.out[21] <- 242

model2 <- glm(y.out ~ x1 + x2 + x3)
summary(model2)
plot(model2)

# new strange errors
y.strange <- y + rnorm(N, 0, 10)^5

model3 <- glm(y.strange ~ x1 + x2 + x3)
summary(model3)
plot(model3)

# now let's build more complex data
y2 <- 2 + x1 + x2*x3 + 2*x3 + rnorm(N, 4)
model4 <- glm(y2 ~ x1 + x2 + x3)
summary(model4)
plot(model4)

model5 <- glm(y2 ~ x1 * x2 * x3)
summary(model5)
plot(model5)
par(op)


# task 2

# correlated factor
x4 <- 0.1 * x3 + rnorm(N, 0, 0.1)
model6 <- glm(y ~ x1 + x2 + x3 + x4)
summary(model6)
plot(model6)

# independent factor
x5 <- rexp(N, 2)
model7 <- glm(y ~ x1 + x2 + x3 + x5)
summary(model7)
plot(model7)

# week model
model8 <- glm(y ~ x1 + x2)
summary(model8)
anova(model1, model8, test='Chisq')
anova(model1, model7, test='Chisq')
anova(model1, model6, test='Chisq')

pairs(y ~ x1 + x2 + x3 + x4)
# x3 and x4 are correlated

library(MASS)
stepAIC(glm(y ~ x1 + x2 + x3 + x4 + x5), direction='both')
stepAIC(glm(y ~  x1 + x2 + x3), direction='both', scope=list(upper=~x1 + x2 + x3 + x1*x2 + x2*x3 + x1*x3 + x1*x2*x3, lower=~1))
