set.seed(2025)
M <- 10^4
Nneed <- 324
lambda <- 0.591
alpha <- 32.4
beta <- 54.8
t <- seq(1, 550, 1)


timepg <- rep(0, M)
csumpg <- rep(0, M)

dgammagamma <- function(t, alpha, b, c) {
	log_dens <- alpha * log(b) - lbeta(alpha, c) + (c - 1) * log(t) - (alpha + c) * log(b + t)
	dens <- exp(log_dens)
	return(dens)
}

### Option 1

simulate_time_to_threshold <- function(Nneed, alpha, beta) {
	csumpg <- 0
	timepg <- 0
	while (csumpg < Nneed) {
		lambdar <- rgamma(1, shape = alpha, rate = beta)
		csumpg <- csumpg + rpois(1, lambda = lambdar)
		timepg <- timepg + 1
	}
	return(timepg)
}

timepg_1 <- replicate(M, simulate_time_to_threshold(Nneed, 3.24, 5.48))
timepg_2 <- replicate(M, simulate_time_to_threshold(Nneed, 324, 548))

plot(density(timepg_2), col = "blue", lwd = 2,
		 main = "Effect of alpha/beta on timepg distribution",
		 xlab = "timepg", ylim = c(0, 0.03))
lines(density(timepg_1), col = "red", lwd = 2)


## we see here that no matter how we change alpha and beta, does not
## affect the variance



### Option 2

simulate_time_variable_lambda <- function(Nneed, alpha, beta) {
	csumpg <- 0
	timepg <- 0
	while (csumpg < Nneed) {
		lambdar <- rgamma(1, shape = alpha, rate = beta) 
		csumpg <- csumpg + rpois(1, lambda = lambdar)
		timepg <- rgamma(1, shape = Nneed, rate = lambdar) + 1
	}
	return(timepg)
}

timepg <- replicate(M, simulate_time_variable_lambda(Nneed, 32.4, 54.8))


mean(timepg>548)
1-pgammagamma(548, alpha = alpha, b = beta, c = 324)

t_vals <- 400:700

plot(t_vals, dgammagamma(t_vals, alpha = 32.4, b = 54.8, c = Nneed), 
		 type = "n",
		 main = "Density Time",
		 xlab = "Day",
		 ylab = "Density")

lines(t_vals, dgammagamma(t_vals, alpha = 32.4, b = 54.8, c = Nneed), 
			lwd = 4,
			col = "purple")

lines(density(timepg), 
			col = "blue",
			lwd = 2,
			lty = 2)

legend("topright",
			 legend = c("Theoretical Gg", "MC sampling"),
			 col = c("purple", "blue"),
			 lwd = c(4, 2),
			 lty = c(1, 2),
			 cex = 0.7)
