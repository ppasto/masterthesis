################################################# COUNTS
###################################### THEORETICAL
########################### Poisson Estimate and Spread V2
##### PLOT 2X2 
# par(mfrow = c(2,2))
par(mfrow = c(1,1))

# # 1 PATH
# t <- seq(1, 40, 1)
# lambda <- 15
# 
# cval <- rpois(n = t, lambda)
# cval_cum <- cumsum(cval)
# 
# data <- data.frame(Time = t, Counts = cval_cum)

# plot(data$Time, data$Counts, type = "l")

####----PATHS PLOT POISSON----####

# 100 PATHS
# Set parameters
set.seed(2025)

n <- 100
t <- seq(1, 550, 1)
lambda <- 0.591

# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(NA, nrow = n, ncol = length(t))

for (i in 1:n) {
	cval <- rpois(length(t), lambda)  
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]


# Reset graphics device to avoid layout issues
# if (dev.cur() > 1) dev.off()

# Set up layout: Histogram (small, upper right) + Main plot
layout(matrix(c(1, 2), ncol = 2), widths = c(3, 1), heights = c(4, 1))  

# Plot the main accrual time series
par(mar = c(4.1, 4.1, 2.1, 2.1))  # Restore normal margins
plot(t,  cval_cum_matrix[1,], 
		 type="n", 
		 main = "Accrual of 100 studies", 
		 xlab = "Time", 
		 ylab = "Count")

for(i in 1:n){
	lines(t,  cval_cum_matrix[i,], col = "lightgray")
}

# Add reference lines
lines(t, lambda*t)
lines(t, qpois(p = 0.975, lambda*t), lty = 2, col = "red")
lines(t, qpois(p = 0.025, lambda*t), lty = 2, col = "red")

legend("topleft",
			 legend = c("2.5th - 97.5th Percentile [95%]",
			 					 "Expected Accrual"),
			 col = c("red", "black"),
			 lty = c(2, 1),
			 lwd = c(1,2),
			 bg = "white",
			 cex = 0.5)  

# Plot histogram in the smaller upper right section
par(mar = c(20, 0.01, 2.1, 2.1))  # Minimize margins
hist_bins <- seq(min(final_counts), max(final_counts), length.out = 15)
hist_data <- hist(final_counts, breaks = hist_bins, plot = FALSE)

barplot(hist_data$counts, horiz = TRUE, space = 0, col = "gray", 
				axes = FALSE, xlab = "", ylab = "")



par(mfrow = c(1,1))
####----POISSON Theoretical PMF----####

# THEORETICAL: EXACT DISTRIBUTION
bar_positions <- barplot(dpois(200:500, lambda*550), 
												 main = "Distribution", 
												 xlab = "Counts", 
												 xaxt = "n")  

axis(1, at = seq(1, length(200:500), by = 50), labels = seq(200, 500, by = 50))  

par(mar = c(4.1, 4.1, 2.1, 2.1))

####----POISSON Theoretical CDF----####

# THEORETICAL: EXACT CUMULATIVE DISTRIBUTION
probdist <- dpois(200:500, lambda*550)
cdf <- cumsum(probdist)
cdf_positions <- barplot(cdf, 
												 main = "Cummulative distribution", 
												 xlab = "Counts", 
												 xaxt = "n")
axis(1, at = seq(1, length(200:500), by = 50), labels = seq(200, 500, by = 50))  

# 
# # Overlay the histogram at Time = length(t)
# par(new = TRUE)  # Allows adding another plot on top
# 
# hist(cval_cum_matrix[length(t), ], 
# 		 col = rgb(0.2, 0.2, 0.8, 0.5),  # Semi-transparent blue
# 		 border = "white",
# 		 xlab = "", ylab = "", main = "", axes = FALSE, 
# 		 freq = TRUE)
# 
# # Adjust position to align with Time = max(t)
# axis(4)  # Adds a secondary axis on the right for the histogram counts
# mtext("Final Count Distribution", side = 4, line = 2)  # Label for the histogram axis


# TODO: ADD histogram
# first: plot the histogram you want (T=length(t))
# then: locate, rotate, etc.

#### Empirical Density COUNTS at T=length(t)

# density_counts <- density(cval_cum_matrix[length(t), ])
# plot(density_counts,
# 		 main = "Empirical Density for Counts at time T = 40",
# 		 col = rgb(0.2, 0.2, 0.8, 0.5),
# 		 xlab = "Counts")


#### Empirical CDF
# 
# ecf_counts <- stats::ecdf(cval_cum_matrix[length(t), ])
# plot(ecf_counts,
# 		 main = "Empirical Cumulative Distribution Function for Counts at time T = 40",
# 		 col = rgb(0.2, 0.2, 0.8, 0.5),
# 		 xlab = "Counts",
# 		 ylab = "Cumulative Probability")

####----QUANTILES PLOT POISSON----####

########################### Poisson Process with Uncertainty Bands V2
# Set parameters
# Set parameters
n <- 100
t <- seq(1, 550, 1)
lambda <- 0.591

# Compute Poisson quantiles over time
q_975_line <- qpois(0.975, lambda * t)
q_900_line <- qpois(0.900, lambda * t)
q_750_line <- qpois(0.75, lambda * t)
q_250_line <- qpois(0.25, lambda * t)
q_100_line <- qpois(0.1, lambda * t)
q_025_line <- qpois(0.025, lambda * t)

# Define x and y axis limits
x_limits <- range(t)
y_limits <- range(cval_cum_matrix)  # Ensures all quantiles fit

# Create the plot
par(mar = c(4.1, 4.1, 2.1, 2.1))
plot(NA, NA, xlim = x_limits, ylim = y_limits, xlab = "Time", ylab = "Count",
		 main = "Quantiles")

# Fill the area with a green gradient
polygon(c(t, rev(t)), c(q_975_line, rev(q_900_line)), col = rgb(144/255, 238/255, 144/255, 0.4), border = NA) # Light Green
polygon(c(t, rev(t)), c(q_900_line, rev(q_750_line)), col = rgb(50/255, 205/255, 50/255, 0.4), border = NA)  # Lime Green
polygon(c(t, rev(t)), c(q_750_line, rev(q_250_line)), col = rgb(0/255, 100/255, 0/255, 0.4), border = NA)    # Dark Green
polygon(c(t, rev(t)), c(q_250_line, rev(q_100_line)), col = rgb(50/255, 205/255, 50/255, 0.4), border = NA)  # Lime Green
polygon(c(t, rev(t)), c(q_100_line, rev(q_025_line)), col = rgb(144/255, 238/255, 144/255, 0.4), border = NA) # Light Green

# Add the mean trend line
lines(t, lambda * t, col = "black", lwd = 2)  

# Legend
legend("topleft",
			 legend = c("2.5th - 97.5th Percentile [95%]", 
			 					 "10th - 90th Percentile     [80%]", 
			 					 "25th - 75th Percentile     [50%]", 
			 					 "Expected Accrual"),
			 col = c(rgb(144/255, 238/255, 144/255, 0.4), rgb(50/255, 205/255, 50/255, 0.4), rgb(0/255, 100/255, 0/255, 0.4), "black"),
			 pch = c(15, 15, 15, NA), lty = c(NA, NA, NA, 1), lwd = c(NA, NA, NA, 2),
			 bg = "white",
			 cex = 0.5)

####----POISSON vs POISSON-GAMMA----####

########################### Poisson vs Negbin
### PMF
plot(200:500, dpois(200:500, lambda*550), 
		 type = "l",
		 main = "Probability Mass Function", 
		 xlab = "Counts", 
		 ylab = "")

alpha = 324
beta = 548

lines(200:500, dnbinom(200:500, size = alpha, mu = lambda*550), col = 2)

legend("topleft",
			 legend = c("Poisson",
			 					 "Negative Binomial"),
			 col = c("black", "red"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.7)  

### CDF
plot(200:500, ppois(200:500, lambda*550), 
		 type = "l",
		 main = "Cummulative Distribution Function", 
		 xlab = "Counts", 
		 ylab = "")

alpha = 324
beta = 548

lines(200:500, pnbinom(200:500, size = alpha, mu = lambda*550), col = 2)

legend("topleft",
			 legend = c("Poisson",
			 					 "Negative Binomial"),
			 col = c("black", "red"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.7)

####----SENSITIVITY ANALYSIS COUNTS----####
### Density Gamma Distribution

curve(dgamma(x, shape = alpha, rate = beta), 
			from = 0, to = 1,
			main = "Probability Density Function", 
			xlab = "Recruitment rate", 
			ylab = "",
			col = "red")


legend("topleft",
			 legend = c(expression("Negative Binomial (" ~ alpha == 324 ~ beta == 548 ~ ")")),
			 col = c("red"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.7)

curve(dgamma(x, shape = alpha, rate = beta), 
			from = 0, to = 1,
			main = "Probability Density Function", 
			xlab = "Recruitment rate", 
			ylab = "",
			col = "red")

curve(dgamma(x, shape = 32.4, rate = 54.8), add = TRUE,
			col = "blue")


legend("topleft",
			 legend = c(expression("Negative Binomial (" ~ alpha == 324 ~ beta == 548 ~ ")"),
			 					 expression("Negative Binomial (" ~ alpha == 32.4 ~ beta == 54.8 ~ ")")),
			 col = c("red", "blue"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.7)

curve(dgamma(x, shape = alpha, rate = beta), 
			from = 0, to = 1,
			main = "Probability Density Function", 
			xlab = "Recruitment rate", 
			ylab = "",
			col = "red")

curve(dgamma(x, shape = 32.4, rate = 54.8), add = TRUE,
			col = "blue")

curve(dgamma(x, shape = 3.24, rate = 5.48), add = TRUE,
			col = "green")

legend("topleft",
			 legend = c(expression("Negative Binomial (" ~ alpha == 324 ~ beta == 548 ~ ")"),
			 						expression("Negative Binomial (" ~ alpha == 32.4 ~ beta == 54.8 ~ ")"),
			 						expression("Negative Binomial (" ~ alpha == 3.24 ~ beta == 5.48 ~ ")")),
			 col = c("red", "blue", "green"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.7)

### PMF
plot(200:500, dpois(200:500, lambda*550), 
		 type = "l",
		 main = "Probability Mass Function", 
		 xlab = "Counts", 
		 ylab = "")


lines(200:500, dnbinom(200:500, size = 324, mu = lambda*550), col = "red")


legend("topleft",
			 legend = c("Poisson",
			 					 expression("Negative Binomial (" ~ alpha == 324 ~ beta == 548 ~ ")")),
			 col = c("black", "red"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.7)

### PMF
plot(200:500, dpois(200:500, lambda*550), 
		 type = "l",
		 main = "Probability Mass Function", 
		 xlab = "Counts", 
		 ylab = "")


lines(200:500, dnbinom(200:500, size = 324, mu = lambda*550), col = "red")
lines(200:500, dnbinom(200:500, size = 32.4, mu = lambda*550), col = "blue")


legend("topleft",
			 legend = c("Poisson",
			 					 expression("Negative Binomial (" ~ alpha == 324 ~ beta == 548 ~ ")"),
			 					 expression("Negative Binomial (" ~ alpha == 32.4 ~ beta == 54.8 ~ ")")),
			 col = c("black", "red", "blue"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.7)


### PMF
plot(200:500, dpois(200:500, lambda*550), 
		 type = "l",
		 main = "Probability Mass Function", 
		 xlab = "Counts", 
		 ylab = "")


lines(200:500, dnbinom(200:500, size = 324, mu = lambda*550), col = "red")
lines(200:500, dnbinom(200:500, size = 32.4, mu = lambda*550), col = "blue")
lines(200:500, dnbinom(200:500, size = 3.24, mu = lambda*550), col = "green")


legend("topleft",
			 legend = c("Poisson",
			 					 expression("Negative Binomial (" ~ alpha == 324 ~ beta == 548 ~ ")"),
			 					 expression("Negative Binomial (" ~ alpha == 32.4 ~ beta == 54.8 ~ ")"),
			 					 expression("Negative Binomial (" ~ alpha == 3.24 ~ beta == 5.48 ~ ")")),
			 col = c("black", "red", "blue", "green"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.7)

### CDF
plot(200:500, ppois(200:500, lambda*550), 
		 type = "l",
		 main = "Cummulative Distribution Function", 
		 xlab = "Counts", 
		 ylab = "")



lines(200:500, pnbinom(200:500, size = 324, mu = lambda*550), col = "red")
lines(200:500, pnbinom(200:500, size = 32.4, mu = lambda*550), col = "blue")
lines(200:500, pnbinom(200:500, size = 3.24, mu = lambda*550), col = "green")


legend("topleft",
			 legend = c("Poisson",
			 					 expression("Negative Binomial (" ~ alpha == 324 ~ beta == 548 ~ ")"),
			 					 expression("Negative Binomial (" ~ alpha == 32.4 ~ beta == 54.8 ~ ")"),
			 					 expression("Negative Binomial (" ~ alpha == 3.24 ~ beta == 5.48 ~ ")")),
			 col = c("black", "red", "blue", "green"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.7)


# assumptions of epistemic uncertainty
# small if we have large alpha and beta
# more epistemic uncertainty the smaller alpha and beta
# add this to PMF and CDF, to show how increasing uncertainty 
# even though the rate is the same
# you can also explain with the variance (see report)
# make three plots 




####----PATHS PLOT POISSON-GAMMA----####
########################### Poisson-Gamma Estimate and Spread V2
######### FIRST OPTION
# 100 PATHS
# Set parameters
set.seed(2025)

n <- 100
t <- seq(1, 550, 1)
lambda <- 0.591

alpha <- 324
beta <- 548
# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(NA, nrow = n, ncol = length(t))
v_lambda <- rgamma(n, shape = alpha, rate = beta)

for (i in 1:n) {
	cval <- rpois(length(t), lambda = v_lambda[i])
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]

# Reset graphics device to avoid layout issues
if (dev.cur() > 1) dev.off()

# Set up layout: Histogram (small, upper right) + Main plot
layout(matrix(c(1, 2), ncol = 2), widths = c(3, 1), heights = c(4, 1))  

# Plot the main accrual time series
par(mar = c(4.1, 2.1, 2.1, 1))  # Adjust margins for visibility
plot(t,  cval_cum_matrix[1,], 
		 type="n", 
		 main = "Accrual of 100 studies", 
		 xlab = "Time", 
		 ylab = "Count")

for(i in 1:n){
	lines(t,  cval_cum_matrix[i,], col = "lightgray")
}

# Add reference lines
lines(t, lambda*t)
lines(t, qnbinom(p = 0.975, size = alpha, mu = lambda*t), lty = 2, col = "red")
lines(t, qnbinom(p = 0.025, size = alpha, mu = lambda*t), lty = 2, col = "red")

legend("topleft",
			 legend = c("2.5th - 97.5th Percentile [95%]",
			 					 "Expected Accrual"),
			 col = c("red", "black"),
			 lty = c(2, 1),
			 lwd = c(1,2),
			 bg = "white",
			 cex = 0.5)  

# Plot histogram in the smaller upper right section
par(mar = c(12, 0.1, 2.1, 1))  # Adjust margins for visibility
hist_bins <- seq(min(final_counts), max(final_counts), length.out = 15)
hist_data <- hist(final_counts, breaks = hist_bins, plot = FALSE)

barplot(hist_data$counts, horiz = TRUE, space = 0, col = "gray", 
				axes = FALSE, xlab = "", ylab = "")

############# SECOND OPTION
####----LAMBDA GENERATION----####
## KEEP ONLY histograms comparing V1 and V2 of PoG
## USE AT LEAST 1000000 (1m)
## USE truehist() from MASS library

# 100 PATHS
# Set parameters
set.seed(2025)

n <- 100000
t <- seq(1, 550, 1)
lambda <- 0.591

alpha <- 324
beta <- 548
### FIRST
cval_cum_matrix <- matrix(NA, nrow = n, ncol = length(t))
v_lambda <- rgamma(n, shape = alpha, rate = beta)

for (i in 1:n) {
	cval <- rpois(length(t), lambda = v_lambda[i])
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]

### SECOND
# Generate cumulative Poisson paths
cval_cum_matrix_2 <- matrix(NA, nrow = n, ncol = length(t))
cval <- rep(NA, length(t))

for (i in 1:n) {
	for(j in 1:length(t)){
		cval[j] <- rpois(1, lambda = rgamma(1, shape = alpha, rate = beta))
	}
	cval_cum_matrix_2[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts_2 <- cval_cum_matrix_2[, length(t)]


#################################### MORE EFFICIENT:
set.seed(2025)

n <- 10^5
t <- seq(1, 550, 1)
lambda <- 0.591

alpha <- 324
beta <- 548

### FIRST
v_lambda <- rgamma(n, shape = alpha, rate = beta)

# Use lapply and simplify2array instead of a for-loop
cval_cum_matrix <- t(simplify2array(lapply(v_lambda, function(l) cumsum(rpois(length(t), lambda = l)))))

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]

### SECOND
# Vectorized approach for the second process
cval_cum_matrix_2 <- t(simplify2array(
	lapply(1:n, function(x) cumsum(rpois(length(t), lambda = rgamma(1, shape = alpha, rate = beta))))
))

# Extract final counts for histogram
final_counts_2 <- cval_cum_matrix_2[, length(t)]


# library(MASS)
# truehist(final_counts, col = rgb(1, 0, 0, 0.4), main = "Comparison", xlab = "Counts", ylab = "Density")
# par(new = TRUE)
# truehist(final_counts_2, col = rgb(0, 0, 1, 0.4), axes = FALSE, xlab = "", ylab = "")
# 
# legend("topright", legend = c("Version 1", "Version 2"), fill = c(rgb(1, 0, 0, 0.4), rgb(0, 0, 1, 0.4)))


plot(density(final_counts),
		 main = "Comparison",
		 xlab = "Counts",
		 col = rgb(1, 0, 0, 0.4), 
		 lwd = 4)

lines(density(final_counts_2), 
			lwd = 2,
			lty = 2,
			col = rgb(0, 0, 1, 0.4))
legend("topright", 
			 legend = c("Version 1", "Version 2"), 
			 col = c(rgb(1, 0, 0, 0.4), rgb(0, 0, 1, 0.4)),
			 lwd = c(4, 2),
			 lty = c(1, 2), 
			 cex = 0.7)
# # Reset graphics device to avoid layout issues
# if (dev.cur() > 1) dev.off()
# 
# # Set up layout: Histogram (small, upper right) + Main plot
# layout(matrix(c(1, 2), ncol = 2), widths = c(3, 1), heights = c(4, 1))  
# 
# # Plot the main accrual time series
# par(mar = c(4.1, 2.1, 2.1, 1))  # Adjust margins for visibility
# plot(t,  cval_cum_matrix_2[1,], 
# 		 type="n", 
# 		 main = "Accrual of 100 studies", 
# 		 xlab = "Time", 
# 		 ylab = "Count")
# 
# for(i in 1:n){
# 	lines(t,  cval_cum_matrix_2[i,], col = "lightgray")
# }
# 
# # Add reference lines
# lines(t, lambda*t)
# lines(t, qnbinom(p = 0.975, size = alpha, mu = lambda*t), lty = 2, col = "red")
# lines(t, qnbinom(p = 0.025, size = alpha, mu = lambda*t), lty = 2, col = "red")
# 
# legend("topleft",
# 			 legend = c("2.5th - 97.5th Percentile [95%]",
# 			 					 "Expected Accrual"),
# 			 col = c("red", "black"),
# 			 lty = c(2, 1),
# 			 lwd = c(1,2),
# 			 bg = "white",
# 			 cex = 0.5)  
# 
# # Plot histogram in the smaller upper right section
# par(mar = c(12, 0.1, 2.1, 1))  # Adjust margins for visibility
# hist_bins <- seq(min(final_counts_2), max(final_counts_2), length.out = 15)
# hist_data <- hist(final_counts_2, breaks = hist_bins, plot = FALSE)
# 
# barplot(hist_data$counts, horiz = TRUE, space = 0, col = "gray", 
# 				axes = FALSE, xlab = "", ylab = "")




####----QUANTILES PLOT POISSON-GAMMA----####
########################### Poisson-Gamma Process with Uncertainty Bands V2
# Set parameters
n <- 100
t <- seq(1, 550, 1)
lambda <- 0.591
alpha <- 324
beta <- 548

# Compute Poisson quantiles over time
q_975_line <- qnbinom(p = 0.975, size = alpha, mu = lambda*t)
q_900_line <- qnbinom(p = 0.900, size = alpha, mu = lambda*t)
q_750_line <- qnbinom(p = 0.75, size = alpha, mu = lambda*t)
q_250_line <- qnbinom(p = 0.25, size = alpha, mu = lambda*t)
q_100_line <- qnbinom(p = 0.1, size = alpha, mu = lambda*t)
q_025_line <- qnbinom(p = 0.025, size = alpha, mu = lambda*t)

# Define x and y axis limits
x_limits <- range(t)
y_limits <- range(cval_cum_matrix)  # Ensures all quantiles fit

# Create the plot
par(mar = c(4.1, 4.1, 2.1, 2.1))
plot(NA, NA, xlim = x_limits, ylim = y_limits, xlab = "Time", ylab = "Count",
		 main = "Quantiles")

# Fill the area with a green gradient
polygon(c(t, rev(t)), c(q_975_line, rev(q_900_line)), col = rgb(144/255, 238/255, 144/255, 0.4), border = NA) # Light Green
polygon(c(t, rev(t)), c(q_900_line, rev(q_750_line)), col = rgb(50/255, 205/255, 50/255, 0.4), border = NA)  # Lime Green
polygon(c(t, rev(t)), c(q_750_line, rev(q_250_line)), col = rgb(0/255, 100/255, 0/255, 0.4), border = NA)    # Dark Green
polygon(c(t, rev(t)), c(q_250_line, rev(q_100_line)), col = rgb(50/255, 205/255, 50/255, 0.4), border = NA)  # Lime Green
polygon(c(t, rev(t)), c(q_100_line, rev(q_025_line)), col = rgb(144/255, 238/255, 144/255, 0.4), border = NA) # Light Green

# Add the mean trend line
lines(t, lambda * t, col = "black", lwd = 2)  

# Legend
legend("topleft",
			 legend = c("2.5th - 97.5th Percentile [95%]", 
			 					 "10th - 90th Percentile     [80%]", 
			 					 "25th - 75th Percentile     [50%]", 
			 					 "Expected Accrual"),
			 col = c(rgb(144/255, 238/255, 144/255, 0.4), rgb(50/255, 205/255, 50/255, 0.4), rgb(0/255, 100/255, 0/255, 0.4), "black"),
			 pch = c(15, 15, 15, NA), lty = c(NA, NA, NA, 1), lwd = c(NA, NA, NA, 2),
			 bg = "white",
			 cex = 0.5)

####----POISSON vs MC sampling----####
### PMF
set.seed(2025)

n <- 100000
t <- seq(1, 550, 1)
lambda <- 0.591

# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(NA, nrow = n, ncol = length(t))

for (i in 1:n) {
	cval <- rpois(length(t), lambda)  
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]


# THEORETICAL: EXACT DISTRIBUTION


library(MASS)
# Histogram with transparency
truehist(final_counts, col = rgb(0, 0, 1, 0.4),  
				 main = "Theoretical vs MC Sampling", 
				 xlab = "Counts", 
				 ylab = "Density")

# Overlaying bar plot with transparency
par(new = TRUE)
bar_positions <- barplot(dpois(250:400, lambda * 550),
												 xaxt = "n", yaxt = "n",
												 col = rgb(0, 1, 0, 0.4))  

# Updated legend
legend("topright", legend = c("MC sampling", "Theoretical Poisson"), 
			 fill = c(rgb(0, 0, 1, 0.4), rgb(0, 1, 0, 0.4)), cex = 0.7)




####----POISSON-GAMMA vs MC sampling----####
set.seed(2025)

n <- 100000
t <- seq(1, 550, 1)
lambda <- 0.591

alpha <- 324
beta <- 548

# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(NA, nrow = n, ncol = length(t))
v_lambda <- rgamma(n, shape = alpha, rate = beta)

for (i in 1:n) {
	cval <- rpois(length(t), lambda = v_lambda[i])
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]


library(MASS)
# Histogram with transparency
truehist(final_counts, col = rgb(0, 0, 1, 0.4),  
				 main = "Theoretical vs MC Sampling", 
				 xlab = "Counts", 
				 ylab = "Density")

# Overlaying bar plot with transparency
par(new = TRUE)
bar_positions <- barplot(dnbinom(250:400, size = alpha, mu = lambda*550),
												 xaxt = "n", yaxt = "n",
												 col = rgb(0, 1, 0, 0.4))  

# Updated legend
legend("topright", legend = c("MC sampling", "Theoretical PoG"), 
			 fill = c(rgb(0, 0, 1, 0.4), rgb(0, 1, 0, 0.4)), cex = 0.7)

####----MC sampling Counts: Poisson model ----####
set.seed(2025)

M <- 10^5
t <- seq(1, 550, 1)
lambda <- 0.591

# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(NA, nrow = M, ncol = length(t))

for (i in 1:M) {
	cval <- rpois(length(t), lambda)  
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]

mean(final_counts>324)
1-ppois(324, lambda*550)

x_vals <- 200:500

plot(x_vals, dpois(x_vals, lambda = lambda * 550), 
		 type = "n",
		 main = "Density Time",
		 xlab = "Day",
		 ylab = "Density")

lines(x_vals,  dpois(x_vals, lambda = lambda * 550), 
			lwd = 4,
			col = "purple")

lines(density(final_counts), 
			col = "blue",
			lwd = 2,
			lty = 2)

legend("topright",
			 legend = c("Theoretical Poisson", "MC sampling"),
			 col = c("purple", "blue"),
			 lwd = c(4, 2),
			 lty = c(1, 2),
			 cex = 0.7)


####----MC sampling Counts: Poisson-gamma model ----####
set.seed(2025)

n <- 10^5
t <- seq(1, 550, 1)
lambda <- 0.591

alpha <- 324
beta <- 548
# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(NA, nrow = n, ncol = length(t))
v_lambda <- rgamma(n, shape = alpha, rate = beta)

for (i in 1:n) {
	cval <- rpois(length(t), lambda = v_lambda[i])
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]


mean(final_counts>324)
1-pnbinom(324, size = 324, mu = lambda * 550)

x_vals <- 200:500

plot(x_vals,  dnbinom(x_vals, size = 324, mu = lambda * 550), 
		 type = "n",
		 main = "Density Time",
		 xlab = "Day",
		 ylab = "Density")

lines(x_vals,  dnbinom(x_vals, size = 324, mu = lambda * 550), 
			lwd = 4,
			col = "purple")

lines(density(final_counts), 
			col = "blue",
			lwd = 2,
			lty = 2)

legend("topright",
			 legend = c("Theoretical PoG", "MC sampling"),
			 col = c("purple", "blue"),
			 lwd = c(4, 2),
			 lty = c(1, 2),
			 cex = 0.7)


####----MC sampling Time: Erlang model ----####
set.seed(2025)
M <- 10^5
Nneed <- 324
lambda <- 0.591
t <- seq(1, 550, 1)


timep <- rep(0, M)
csump <- rep(0, M)

start.time <- Sys.time()

for(m in 1:M){
	while (csump[m] < Nneed) {
		csump[m] <- csump[m] + rpois(1, lambda)
		timep[m] <- timep[m] + 1
	}
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

mean(timep>548)
1-pgamma(548, shape = alpha, rate = lambda)

# for compiling:
# 1st saving
# write.csv(timep, "specify_path_and_file_name.csv")
# read.csv(...) instead of loop
# 2nd try M=10^6

# is the problem the loop? or the graph with so many (10^5) data points?

### M = 10^4: 8.6 secs
### M = 10^5: 1.45 mins
### M = 10^6: 12.8 mins


set.seed(2025)
M <- 10^5
Nneed <- 324
lambda <- 0.591
t <- seq(1, 550, 1)


alpha <- 324
beta <- 548

timep <- rep(0, M)
csump <- rep(0, M)

for(m in 1:M){
	while (csump[m] < Nneed) {
		csump[m] <- csump[m] + rpois(1, lambda)
		timep[m] <- timep[m] + 1
	}
}

simulate_time_fixed_lambda <- function(Nneed, alpha, beta) {
	csumpg <- 0
	timepg <- 0
	fixed_lambda <- alpha / beta
	while (csumpg < Nneed) {
		csumpg <- csumpg + rpois(1, lambda = fixed_lambda)
		timepg <- timepg + 1
	}
	return(timepg)
}

t_vals <- 400:700

plot(t_vals, dgamma(t_vals, shape = alpha, rate = lambda), 
		 type = "n",
		 main = "Density Time",
		 xlab = "Day",
		 ylab = "Density")

lines(t_vals, dgamma(t_vals, shape = alpha, rate = lambda), 
			lwd = 4,
			col = "purple")

lines(density(timep), 
			col = "blue",
			lwd = 2,
			lty = 2)

legend("topright",
			 legend = c("Theoretical Erlang", "MC sampling"),
			 col = c("purple", "blue"),
			 lwd = c(4, 2),
			 lty = c(1, 2),
			 cex = 0.7)



##### MR:

M <- 10^4
Ctarget <- 324
lambda <- 0.591

alpha <- 32.4
beta <- 54.8

simulate_time_to_threshold_Erlang_easier <- function(MM, CC, ll, rseed=265735) {
	set.seed(rseed)
	timeerlang <- rgamma(n=MM, shape = CC, rate = ll)
	return(timeerlang)
}

time_erlang <- simulate_time_to_threshold_Erlang_easier(MM=M, CC=Ctarget, ll=lambda, rseed=265735)

plot(t_vals, dgamma(t_vals, shape = Nneed, rate = lambda), 
		 type = "n",
		 main = "MC simulation vs theoretical density",
		 xlab = "Day",
		 ylab = "Density")

lines(t_vals, dgamma(t_vals, shape = Nneed, rate = lambda), 
			lwd = 4,
			col = "purple")

lines(density(time_erlang), 
			col = "blue",
			lwd = 2,
			lty = 2)

legend("topright",
			 legend = c("Theoretical Erlang", "MC simulation"),
			 col = c("purple", "blue"),
			 lwd = c(4, 2),
			 lty = c(1, 2),
			 cex = 0.7)



####----MC sampling Time: Gamma-Gamma model ----####
set.seed(2025)
M <- 10^5
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

# start.time <- Sys.time()
# 
# for(m in 1:M){
# 	while (csumpg[m] < Nneed) {
# 		csumpg[m] <- csumpg[m] + rnbinom(1, size = alpha, mu = lambda*t)
# 		timepg[m] <- timepg[m] + 1
# 	}
# }
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

simulate_time_fixed_lambda <- function(Nneed, alpha, beta) {
	csumpg <- 0
	timepg <- 0
	fixed_lambda <- alpha / beta
	while (csumpg < Nneed) {
		csumpg <- csumpg + rpois(1, lambda = fixed_lambda)
		timepg <- timepg + 1
	}
	return(timepg)
}

simulate_time_variable_lambda_no <- function(Nneed, alpha, beta) {
	csumpg <- 0
	timepg <- 0
	while (csumpg < Nneed) {
		lambdar <- rgamma(1, shape = alpha, rate = beta)  # Variable λ
		csumpg <- csumpg + rpois(1, lambda = lambdar)
		timepg <- timepg + 1
	}
	return(timepg)
}

#### variance does not change by changing alpha and beta

time_var <- replicate(M, simulate_time_variable_lambda_no(Nneed, alpha, beta))
time_fixed <- replicate(M, simulate_time_fixed_lambda(Nneed, alpha, beta))

# Compare variances
var(time_var)
var(time_fixed)

plot(density(time_var), col = "blue", lwd = 2, 
		 main = "Effect of Gamma variability on timepg",
		 xlab = "Time to reach threshold")
lines(density(time_fixed), col = "red", lwd = 2)
legend("topright", legend = c("Variable λ ~ Gamma", "Fixed λ = E[Gamma]"),
			 col = c("blue", "red"), lwd = 2)


## Option 2 -- good approximation

simulate_time_variable_lambda <- function(Nneed, alpha, beta) {
	csumpg <- 0
	timepg <- 0
	while (csumpg < Nneed) {
		lambdar <- rgamma(1, shape = alpha, rate = beta) 
		csumpg <- csumpg + rpois(1, lambda = lambdar)
		timepg <- rgamma(1, shape = Nneed, rate = lambdar)
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
			 legend = c("Theoretical GG", "MC sampling"),
			 col = c("purple", "blue"),
			 lwd = c(4, 2),
			 lty = c(1, 2),
			 cex = 0.7)


### M = 10^4: 
### M = 10^5: 4 mins
### M = 10^6: 


##### MR:

set.seed(2025)
M <- 10^4
Ctarget <-324 
lambda <- 0.591
alpha <- 32.4
beta <- 54.8

simulate_time_to_threshold_GG_easier <- function(MM, CC, aa, bb, rseed=265735) {
	set.seed(rseed)
	timegg <- rep(0, MM)
	lambdar <- rgamma(n=MM, shape = aa, rate = bb)
	for (i in 1:MM){
		timegg[i] <- rgamma(n=1, shape = CC, rate = lambdar[i])
	}
	return(timegg)
}

timegg_easier <- simulate_time_to_threshold_GG_easier(MM=M, CC=Ctarget, aa=alpha, bb=beta) 

t_vals <- 100:1100

plot(t_vals, dgammagamma(t_vals, a = 32.4, b = 54.8, c = Nneed), 
		 type = "n", main = "Density Time", xlab = "Day", ylab = "Density")
lines(t_vals, dgammagamma(t_vals, a = 32.4, b = 54.8, c = Nneed), 
			lwd = 4, col = "purple")
lines(density(timegg_easier), 
			col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Theoretical GG", "MC sampling"),
			 col = c("purple", "blue"), lwd = c(4, 2), lty = c(1, 2), cex = 0.7)


####----Poisson 100 paths histogram time + counts----####
set.seed(2025)

n <- 100
alpha <- 324
t <- seq(1, 550, 1)
lambda <- 0.591

# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(NA, nrow = n, ncol = length(t))
for (i in 1:n) {
	cval <- rpois(length(t), lambda)  
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

final_counts <- cval_cum_matrix[, length(t)]
tval <- rgamma(n, shape = alpha, rate = lambda)

# Reset layout
par(mfrow = c(1,1))
dev.new()  # Open a new clean graphics window if needed

# Define layout: 2 rows, 2 columns (first row spans both columns)
layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE), 
			 heights = c(1, 4), widths = c(3, 1))

# --- Plot 1: tval histogram (top, horizontal barplot) ---
par(mar = c(0.1, 25, 2, 10))
tval_bins <- seq(min(tval), max(tval), length.out = 15)
tval_hist <- hist(tval, breaks = tval_bins, plot = FALSE)

barplot(tval_hist$counts,
				space = 0,
				col = "skyblue",
				axes = FALSE,
				horiz = FALSE)

# --- Plot 2: Accrual time series (bottom-left) ---
par(mar = c(4, 4, 2, 1))
plot(t,  cval_cum_matrix[1,], 
		 type = "n", 
		 main = "Accrual of 100 studies", 
		 xlab = "Time", 
		 ylab = "Count")

for(i in 1:n){
	lines(t,  cval_cum_matrix[i,], col = "lightgray")
}

lines(t, lambda*t)
lines(t, qpois(p = 0.975, lambda*t), lty = 2, col = "red")
lines(t, qpois(p = 0.025, lambda*t), lty = 2, col = "red")

legend("topleft",
			 legend = c("2.5th - 97.5th Percentile [95%]",
			 					 "Expected Accrual"),
			 col = c("red", "black"),
			 lty = c(2, 1),
			 lwd = c(1,2),
			 bg = "white",
			 cex = 0.5)

# --- Plot 3: Final counts histogram (bottom-right) ---
par(mar = c(25, 0.5, 2, 1))
hist_bins <- seq(min(final_counts), max(final_counts), length.out = 15)
hist_data <- hist(final_counts, breaks = hist_bins, plot = FALSE)

barplot(hist_data$counts,
				horiz = TRUE,
				space = 0,
				col = "gray",
				axes = FALSE)



####----Po-G 100 paths histogram time + counts----####
set.seed(2025)

n <- 100
t <- seq(1, 550, 1)
lambda <- 0.591

alpha <- 324
beta <- 548

# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(NA, nrow = n, ncol = length(t))
v_lambda <- rgamma(n, shape = alpha, rate = beta)

for (i in 1:n) {
	cval <- rpois(length(t), lambda = v_lambda[i])
	cval_cum_matrix[i, ] <- cumsum(cval)  
}

# Extract final counts for histogram
final_counts <- cval_cum_matrix[, length(t)]

v_lambda <- rgamma(n, shape = alpha, rate = beta)
tval <- numeric(n)  

for (i in 1:n) {
	tval[i] <- rgamma(1, shape = alpha, rate = v_lambda[i])
}


# Reset layout
par(mfrow = c(1,1))
dev.new()  # Open a new clean graphics window if needed

# Define layout: 2 rows, 2 columns (first row spans both columns)
layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE), 
			 heights = c(1, 4), widths = c(3, 1))

# --- Plot 1: tval histogram (top, horizontal barplot) ---
par(mar = c(0.1, 25, 2, 10))
tval_bins <- seq(min(tval), max(tval), length.out = 15)
tval_hist <- hist(tval, breaks = tval_bins, plot = FALSE)

barplot(tval_hist$counts,
				space = 0,
				col = "skyblue",
				axes = FALSE,
				horiz = FALSE)

# --- Plot 2: Accrual time series (bottom-left) ---
par(mar = c(4, 4, 2, 1))
plot(t,  cval_cum_matrix[1,], 
		 type = "n", 
		 main = "Accrual of 100 studies", 
		 xlab = "Time", 
		 ylab = "Count")

for(i in 1:n){
	lines(t,  cval_cum_matrix[i,], col = "lightgray")
}

lines(t, lambda*t)
lines(t, qnbinom(p = 0.975, size = alpha, mu = lambda*t), lty = 2, col = "green")
lines(t, qnbinom(p = 0.025, size = alpha, mu = lambda*t), lty = 2, col = "green")

legend("topleft",
			 legend = c("2.5th - 97.5th Percentile [95%]",
			 					 "Expected Accrual"),
			 col = c("green", "black"),
			 lty = c(2, 1),
			 lwd = c(1,2),
			 bg = "white",
			 cex = 0.5)

# --- Plot 3: Final counts histogram (bottom-right) ---
par(mar = c(25, 0.5, 2, 1))
hist_bins <- seq(min(final_counts), max(final_counts), length.out = 15)
hist_data <- hist(final_counts, breaks = hist_bins, plot = FALSE)

barplot(hist_data$counts,
				horiz = TRUE,
				space = 0,
				col = "gray",
				axes = FALSE)







####----Sensitivity Analysis Time----####

dgammagamma <- function(t, alpha, b, c) {
	log_dens <- alpha * log(b) - lbeta(alpha, c) + (c - 1) * log(t) - (alpha + c) * log(b + t)
	dens <- exp(log_dens)
	return(dens)
}

pgammagamma <- function(t, alpha, b, c) {
	sapply(t, function(x) {
		integrate(function(u) dgammagamma(u, alpha, b, c), lower = 0, upper = x)$value
	})
}

par(mfrow=c(1,3))
curve(dgamma(x, shape = 324, rate = 548), 
			from = 0, to = 1,
			main = "Prior", 
			xlab = "Recruitment rate", 
			ylab = "",
			col = "red")

curve(dgamma(x, shape = 32.4, rate = 54.8), add = TRUE,
			col = "blue")

curve(dgamma(x, shape = 3.24, rate = 5.48), add = TRUE,
			col = "green")

legend("topleft",
			 legend = c(expression("G (" ~ alpha == 324 ~ beta == 548 ~ ")"),
			 					 expression("G (" ~ alpha == 32.4 ~ beta == 54.8 ~ ")"),
			 					 expression("G (" ~ alpha == 3.24 ~ beta == 5.48 ~ ")")),
			 col = c("red", "blue", "green"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.6)


plot(0:800, dgamma(0:800, shape = 324, rate = lambda), 
		 type = "l",
		 main = "PMF", 
		 xlab = "Counts", 
		 ylab = "", 
		 col = "black")

lines(0:800, dgammagamma(0:800, alpha = 324, b = 548, c = 324), col = "red")
lines(0:800, dgammagamma(0:800, alpha = 32.4, b = 54.8, c = 324), col = "blue")
lines(0:800, dgammagamma(0:800, alpha = 3.24, b = 5.48, c = 324), col = "green")


legend("topleft",
			 legend = c(expression(G(alpha == 324 ~ beta == 548)),
			 					 expression(Gg ~ (alpha == 324 ~ beta == 548)),
			 					 expression(Gg ~ (alpha == 32.4 ~ beta == 54.8)),
			 					 expression(Gg ~ (alpha == 3.24 ~ beta == 5.48))),
			 col = c("black", "red", "blue", "green"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.6)



plot(0:800, pgamma(0:800, shape = 324, rate = lambda), 
		 type = "l",
		 main = "PMF", 
		 xlab = "Counts", 
		 ylab = "", 
		 col = "black")

lines(0:800, pgammagamma(0:800, alpha = 324, b = 548, c = 324), col = "red")
lines(0:800, pgammagamma(0:800, alpha = 32.4, b = 54.8, c = 324), col = "blue")
lines(0:800, pgammagamma(0:800, alpha = 3.24, b = 5.48, c = 324), col = "green")


legend("topleft",
			 legend = c(expression(G(alpha == 324 ~ beta == 548)),
			 					 expression(Gg ~ (alpha == 324 ~ beta == 548)),
			 					 expression(Gg ~ (alpha == 32.4 ~ beta == 54.8)),
			 					 expression(Gg ~ (alpha == 3.24 ~ beta == 5.48))),
			 col = c("black", "red", "blue", "green"),
			 lty = c(1, 1),
			 bg = "white",
			 cex = 0.6)





####----APPENDIX----####
########################### Poisson Estimate and Spread V1
library(ggplot2)
library(ggExtra)

set.seed(123)
lambda <- 0.591
n <- 550

tval <- numeric(n)
cval <- numeric(n)

for (j in 1:n) {
	t <- j + 1
	c <- rpois(1, lambda * t)
	
	tval[j] <- t
	cval[j] <- c
}

data <- data.frame(Time = tval, Counts = cval)

# Main Scatter Plot with Poisson Estimate and Spread
p <- ggplot(data, aes(x = Time, y = Counts)) +
	geom_point(color = "gray", alpha = 0.6) +
	geom_abline(aes(intercept = 0, slope = lambda, color = "Point Estimate Expectation", linetype = "Point Estimate Expectation"), linewidth = 1.2) +
	geom_line(aes(y = lambda * Time + sqrt(lambda * Time), color = "Poisson Aleatory Uncertainty", linetype = "Poisson Aleatory Uncertainty")) +
	geom_line(aes(y = lambda * Time - sqrt(lambda * Time), color = "Poisson Aleatory Uncertainty", linetype = "Poisson Aleatory Uncertainty")) +
	scale_color_manual(name = "Legend", values = c("Point Estimate Expectation" = "red", "Poisson Aleatory Uncertainty" = "blue")) +
	scale_linetype_manual(name = "Legend", values = c("Point Estimate Expectation" = "solid", "Poisson Aleatory Uncertainty" = "dashed")) +
	labs(title = "Poisson Counts with Spread", x = "Time", y = "Counts") +
	theme_minimal() +
	theme(legend.position = c(0.05, 0.95),  # Top-left corner
				legend.justification = c(0, 1),   # Align top-left
				legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5))

# Add rotated histogram on the right
p_final <- ggMarginal(p, type = "histogram", fill = "lightblue", bins = 30, margins = "y")

print(p_final)


########################### Poisson Process with Uncertainty Bands
library(ggplot2)
library(dplyr)
library(tidyr)

# Parameters
lambda <- 0.591 
n <- 550
num_sim <- 1000  

# Generate Data
set.seed(123)  
tval <- 1:n
sim_data <- matrix(NA, nrow = n, ncol = num_sim)

for (sim in 1:num_sim) {
	sim_data[, sim] <- rpois(n, lambda * tval)
}

# Convert to DataFrame
df <- as.data.frame(sim_data)
df$t <- tval

# Compute Quantiles
df_long <- df %>% pivot_longer(-t, names_to = "simulation", values_to = "counts")

df_summary <- df_long %>%
	group_by(t) %>%
	summarise(
		median = median(counts),
		q25 = quantile(counts, 0.25),
		q75 = quantile(counts, 0.75),
		q95 = quantile(counts, 0.95),
		q5 = quantile(counts, 0.05)
	)

ggplot(df_summary, aes(x = t)) +
	geom_ribbon(aes(ymin = q5, ymax = q95, fill = "5th-95th Percentile"), alpha = 0.2) +
	geom_ribbon(aes(ymin = q25, ymax = q75, fill = "25th-75th Percentile"), alpha = 0.4) +
	geom_line(aes(y = median, color = "Median"), size = 1) +
	scale_fill_manual(name = "Uncertainty Bands", 
										values = c("5th-95th Percentile" = "darkgreen", 
															 "25th-75th Percentile" = "green")) +
	scale_color_manual(name = "Median", values = c("Median" = "black")) +
	labs(title = "Poisson Process with Uncertainty Bands",
			 x = "Time", y = "Counts") +
	theme_minimal() +
	theme(legend.position = "top")



################################################# TIME

num_iterations <- 1000  # Number of simulations
sample_size <- 324  # Desired sample size per simulation
lambda <- 0.591  # Poisson mean 

# Initialize vectors to store results
C_values <- numeric(num_iterations)
T_values <- numeric(num_iterations)

# Simulation
for (j in 1:num_iterations) {
	Nj <- 0  # Initialize sample count
	Tj <- 0  # Initialize time count
	
	while (Nj < sample_size) {
		x <- rpois(1, lambda)  # Generate Poisson random variable
		Nj <- Nj + x  # Update sample count
		Tj <- Tj + 1  # Increment time
	}
	
	C_values[j] <- Nj
	T_values[j] <- Tj  # Store the time for this iteration
}

# Calculate percentiles
percentiles_C <- quantile(C_values, probs = c(0.25, 0.50, 0.75, 0.95))
percentiles_T <- quantile(T_values, probs = c(0.25, 0.50, 0.75, 0.95))


# Output results
print(percentiles_C)
print(percentiles_T)

library(ggplot2)
ggplot(data.frame(T_values), aes(x = T_values)) +
	geom_density(fill = "blue", alpha = 0.5) +
	labs(title = "Density Plot of Time", x = "Time", y = "Density")

ggplot(data.frame(C_values), aes(x = C_values)) +
	geom_density(fill = "red", alpha = 0.5) +
	labs(title = "Density Plot of Counts", x = "Counts", y = "Density")

plot(T_values, C_values)



