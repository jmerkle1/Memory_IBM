y <- seq(0,2,.001)
x <- seq(0,2,.001)
y <- (exp(y) - (y + 1))/2
y <- ifelse(y > 2, 2, y)
plot(y ~ x, xlim = c(0,2), ylim = c(0,2))
y[which(x == .5)]
y[which(x == .25)]


