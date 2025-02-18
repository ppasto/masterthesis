################################################# COUNTS
###################################### THEORETICAL
########################### Poisson Estimate and Spread V2
library(ggplot2)
library(ggExtra)

# 1 PATH
t <- seq(0, 40, 1)
lambda <- 15

cval <- rpois(n = t, lambda)
cval_cum <- cumsum(cval)

data <- data.frame(Time = t, Counts = cval_cum)

plot(data$Time, data$Counts, type = "l")

# 1000 PATHS
n <- 1000
t <- seq(0, 40, 1)
lambda <- 15

cval_cum_matrix <- matrix(0, nrow = length(t), ncol = n)

for (i in 1:n) {
	cval <- rpois(length(t), lambda)  
	cval_cum_matrix[, i] <- cumsum(cval)  
}

matplot(t, cval_cum_matrix, type = "l", lty = 1, col = rainbow(n),
				main = "1000 Cumulative Poisson Paths", xlab = "Time", ylab = "Count")
abline(0, lambda)
abline(0, qpois(p = 0.975, lambda), lty = 2, col = "gray")
abline(0, qpois(p = 0.025, lambda), lty = 2, col = "gray")

# TODO: ADD histogram
# first: plot the histogram you want (T=length(t))
# then: locate, rotate, etc.

########################### Poisson Process with Uncertainty Bands V2
# Set parameters
n <- 100
t <- seq(0, 40, 1)
lambda <- 15

# Compute Poisson quantiles over time
q_975 <- qpois(0.975, lambda * t)
q_900 <- qpois(0.9, lambda * t)
q_750 <- qpois(0.75, lambda * t)
q_250 <- qpois(0.25, lambda * t)
q_100 <- qpois(0.1, lambda * t)
q_025 <- qpois(0.025, lambda * t)

# Define x and y axis limits
x_limits <- range(t)
y_limits <- range(q_025, q_975, na.rm = TRUE)  # Ensures all quantiles fit

# Create the plot
plot(NA, NA, xlim = x_limits, ylim = y_limits, xlab = "Time", ylab = "Count",
		 main = "Reference Quantiles with Green Gradient")

# Fill areas between quantile lines with shades of green
polygon(c(t, rev(t)), c(q_975, rev(q_900)), col = "lightgreen", border = NA)  # Lightest Green
polygon(c(t, rev(t)), c(q_900, rev(q_750)), col = "limegreen", border = NA)  # Light Green
polygon(c(t, rev(t)), c(q_750, rev(q_250)), col = "darkgreen", border = NA)  # Medium Green
polygon(c(t, rev(t)), c(q_250, rev(q_100)), col = "limegreen", border = NA)  # Dark Green
polygon(c(t, rev(t)), c(q_100, rev(q_025)), col = "lightgreen", border = NA)  # Darkest Green

# Add reference lines
abline(a = 0, b = lambda, col = "black", lwd = 2)  # Mean trend line
lines(t, q_975, lty = 2, lwd = 1.5)   # Upper bound
lines(t, q_900, lty = 2, lwd = 1.5)   # 90% quantile
lines(t, q_750, lty = 2, lwd = 1.5)   # 75% quantile
lines(t, q_250, lty = 2, lwd = 1.5)   # 25% quantile
lines(t, q_100, lty = 2, lwd = 1.5)   # 10% quantile
lines(t, q_025, lty = 2, lwd = 1.5)   # Lower bound


###################################### SIMULATIONS
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



