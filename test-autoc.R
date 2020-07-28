
x <- 1:100
x <- rnorm(100)
plot(x, type = "l")

acf(1:100, lag.max = 100)
acf(1:100, lag.max = 50)
acf(1:100, lag.max = 20)

acf(100:1, lag.max = 100)
acf(seq(1, 100, 2), lag.max = 50)

plot(rep(0,100), type= "l")
acf(rep(0,100), lag.max = 100)
