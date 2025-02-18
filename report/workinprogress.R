################################################# COUNTS
###################################### THEORETICAL
########################### Poisson Estimate and Spread V2
# 1 PATH
t <- seq(0, 40, 1)
lambda <- 15

cval <- rpois(n = t, lambda)
cval_cum <- cumsum(cval)

data <- data.frame(Time = t, Counts = cval_cum)

plot(data$Time, data$Counts, type = "l")

# 1000 PATHS
# Set parameters
set.seed(2025)

n <- 100
t <- seq(0, 40, 1)
lambda <- 15

# Generate cumulative Poisson paths
cval_cum_matrix <- matrix(0, nrow = length(t), ncol = n)

for (i in 1:n) {
	cval <- rpois(length(t), lambda)  
	cval_cum_matrix[, i] <- cumsum(cval)  
}

# Plot the cumulative Poisson paths
matplot(t, cval_cum_matrix, type = "l", lty = 1, col = rainbow(n),
				main = "100 Cumulative Poisson Paths", xlab = "Time", ylab = "Count")

# Add reference lines
abline(0, lambda)
abline(0, qpois(p = 0.975, lambda), lty = 2, col = "gray")
abline(0, qpois(p = 0.025, lambda), lty = 2, col = "gray")

# Overlay the histogram at Time = max(t)
par(new = TRUE)  # Allows adding another plot on top

hist(cval_cum_matrix[length(t), ], 
		 col = rgb(0.2, 0.2, 0.8, 0.5),  # Semi-transparent blue
		 border = "white",
		 xlab = "", ylab = "", main = "", axes = FALSE, 
		 freq = TRUE)

# Adjust position to align with Time = max(t)
axis(4)  # Adds a secondary axis on the right for the histogram counts
mtext("Final Count Distribution", side = 4, line = 2)  # Label for the histogram axis

legend("topleft",
			 legend = c("2.5th - 97.5th Percentile [95%]",
			 					 "Mean Trend",
			 					 "Histogram at T=40"),
			 col = c("grey", "black", rgb(0.2, 0.2, 0.8, 0.5)),
			 lty = c(2, 1, NA),
			 lwd = c(1,2, NA),
			 pch = c(NA,NA, 15),
			 bg = "white",
			 cex = 0.8) 
# TODO: ADD histogram
# first: plot the histogram you want (T=length(t))
# then: locate, rotate, etc.

########################### Poisson Process with Uncertainty Bands V2
# Set parameters
n <- 100
t <- seq(0, 40, 1)
lambda <- 15

# Compute Poisson quantiles over time
q_975 <- qpois(0.975, lambda)
q_900 <- qpois(0.9, lambda)
q_750 <- qpois(0.75, lambda)
q_250 <- qpois(0.25, lambda)
q_100 <- qpois(0.1, lambda)
q_025 <- qpois(0.025, lambda)

# Define x and y axis limits
x_limits <- range(t)
y_limits <- range(cval_cum_matrix)  # Ensures all quantiles fit

# Create the plot
plot(NA, NA, xlim = x_limits, ylim = y_limits, xlab = "Time", ylab = "Count",
		 main = "Reference Quantiles with Green Gradient")

# Compute y-values for diagonal quantile lines
q_975_line <- qpois(0.975, lambda) * t
q_900_line <- qpois(0.900, lambda) * t
q_750_line <- qpois(0.75, lambda)* t
q_250_line <- qpois(0.25, lambda)* t
q_100_line <- qpois(0.1, lambda)* t
q_025_line <- qpois(0.025, lambda)* t

# Fill the area between the two diagonal quantile lines
polygon(c(t, rev(t)), c(q_975_line, rev(q_900_line)), col = "lightgreen", border = NA)
polygon(c(t, rev(t)), c(q_750_line, rev(q_900_line)), col = "limegreen", border = NA)
polygon(c(t, rev(t)), c(q_750_line, rev(q_250_line)), col = "darkgreen", border = NA)
polygon(c(t, rev(t)), c(q_250_line, rev(q_100_line)), col = "limegreen", border = NA)  # Dark Green
polygon(c(t, rev(t)), c(q_100_line, rev(q_025_line)), col = "lightgreen", border = NA)  # Darkest Green

abline(a = 0, b = lambda, col = "black", lwd = 2)  # Mean trend line
# abline(0, q_975)   
# abline(0, q_900)   
# abline(0, q_750)   
# abline(0, q_250)   
# abline(0, q_100)   
# abline(0, q_025)  

legend("topleft",
			 legend = c("2.5th - 97.5th Percentile [95%]", 
			 					 "10th - 90th Percentile     [80%]", 
			 					 "25th - 75th Percentile     [50%]", 
			 					 "Mean Trend"),
			 col = c("lightgreen", "limegreen", "darkgreen", "black"),
			 pch = c(15, 15, 15, 15),
			 bg = "white",
			 cex = 0.8) 



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



