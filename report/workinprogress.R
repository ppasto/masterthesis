################################################# COUNTS
########################### Poisson Estimate and Spread
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
