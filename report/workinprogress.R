################################################# COUNTS
########################### plot 1 v1
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

# Define layout: 2 columns (Main plot + Histogram)
layout(matrix(c(1,2), nrow=1), widths = c(3,1))  # Main plot wider than histogram

# Main Scatter Plot
par(mar=c(5,4,4,1))  # Adjust margins
plot(tval, cval, main="Poisson Counts with Spread", xlab="Time", ylab="Counts", col="gray")

# Reference line (Estimate)
abline(a = 0, b = lambda, col= "red", lwd=2)

# Spread lines (Estimate ± SD)
lines(tval, lambda * tval + sqrt(lambda * tval), col="blue", lty=2)
lines(tval, lambda * tval - sqrt(lambda * tval), col="blue", lty=2)

legend("topleft", legend = c("Estimate", "Poisson - Aleatory"),
			 col = c("red", "blue"), lty = c(1, 2), lwd = c(2, 1))

# Histogram (Y-axis)
par(mar=c(5,1,4,4))  # Adjust margins for the histogram
hist(cval, breaks=30, col="lightblue", main="Counts Distribution", xlab="", ylab="", horiz=TRUE)

########################### plot 1 v2

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
	geom_abline(intercept = 0, slope = lambda, color = "red", linewidth = 1.2) +
	geom_line(aes(y = lambda * Time + sqrt(lambda * Time)), color = "blue", linetype = "dashed") +
	geom_line(aes(y = lambda * Time - sqrt(lambda * Time)), color = "blue", linetype = "dashed") +
	labs(title = "Poisson Counts with Spread", x = "Time", y = "Counts") +
	theme_minimal()

# Add rotated histogram on the right
p_final <- ggMarginal(p, type = "histogram", fill = "lightblue", bins = 30, margins = "y")

print(p_final)

########################### plot 2
library(ggplot2)
library(dplyr)
library(tidyr)

# Parameters
lambda <- 0.591 
n <- 550
num_sim <- 1000  # Number of simulations for uncertainty bands

# Generate Data
set.seed(123)  # For reproducibility
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

# Plot
ggplot(df_summary, aes(x = t)) +
	geom_ribbon(aes(ymin = q5, ymax = q95), fill = "darkgreen", alpha = 0.2) +
	geom_ribbon(aes(ymin = q25, ymax = q75), fill = "green", alpha = 0.4) +
	geom_line(aes(y = median), color = "black", size = 1) +
	labs(title = "Poisson Process with Uncertainty Bands",
			 x = "Time", y = "Counts") +
	theme_minimal()


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
